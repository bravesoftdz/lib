unit Image_on_demand;

interface

uses windows,classes,streaming_class_lib, syncObjs, pngImage, command_class_lib,graphics,
  IGraphicObject_commands, ExtCtrls, Types, pngimageAdvanced;

type

TLogProc = procedure (text: string);

TGetImageProc = function: TExtendedPngObject of object;

IGetPngThread = interface
['{3726C791-0100-44DA-8D5A-E900A4EB6A62}']
  function GetImage: TExtendedPngObject;
  procedure SetImage(value: TExtendedPngObject);  //в основном потоке, без особенностей
  procedure Halt;
end;

IGetPngScale = interface (IGetPngThread)
['{6DC20893-9FC9-4FFB-8864-69D7D4064831}']
  function GetScale: Real;
end;

TAbstractGetImageThread = class (TThread, IGetPngThread)
  private
    fRefCount: Integer;
    fEvent: TEvent;
    fBitmap: TExtendedPngObject;
    fExceptionHandled: boolean;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure Halt;
    constructor Create; virtual;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetImage: TExtendedPngObject;
    procedure SetImage(value: TExtendedPngObject);
end;

TLoadBitmapThread = class (TAbstractGetImageThread, IGetPngThread) //будет отвечать за картинку головой!
  private
    fFilename: string;
  protected
    procedure Execute; override;
  public
    constructor Create(aOnTerminate: TNotifyEvent;filename: string=''); reintroduce; overload;
end;

TScalingThread = class (TAbstractGetImageThread, IGetPngThread, IGetPngScale)
  private
    fGetImageIntf: IGetPngThread; //ссылка на оригинальную картинку
    fScale: Real;
  protected
    procedure Execute; override;
  public
    constructor Create(aGetImageIntf: IGetPngThread; ascale: Real); reintroduce; overload;
    procedure UpdateRect(aRect: TRect); virtual;
    function GetScale: Real;
end;

T2to3ScalingThread = class (TScalingThread,IGetPngThread,IGetPngScale)
  protected
    procedure Execute; override;
  public
    procedure UpdateRect(aRect: TRect); override;
end;

TBrushShape = (bsSquare,bsRound);

TRasterImageDocument = class (TDocumentWithImage, IConstantComponentName)
  private
    fScaleNumber: Integer;
    fPrimaryColor: TColor;
    fSecondaryColor: TColor;
    fBrushSize: Integer;
    fBrushShape: TBrushShape;
    fLoadThread: IGetPngThread;
    fScalingThreads: array of IGetPngScale;
    fScaleLevelsReadyEvent: TEvent;
    fSizesChanged: Boolean;
    procedure LoadThreadTerminate(Sender: TObject);
    procedure WaitScaleLevelsReady;
    function GetBtmp: TExtendedPngObject;
    procedure SetBtmp(value: TExtendedPngObject);
  public
    PercentDone: Integer; //инкапсуляция ни к черту
    Image: TImage;
    ChangeRect: TRect;  //0,0,0,0 озн. пустую область
    onSavingProgress: TSavingProgressReport;
    procedure AddToChangeRect(const A: TRect);
    procedure SizesChanged;
    constructor Create(aOwner: TComponent); override;
    constructor CreateFromImageFile(aFileName: string);
    constructor LoadFromFile(const aFilename: string); override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
    function Get_Scaled_Btmp: TExtendedPNGObject;
    function Get_Scale: real; override;
    procedure SaveAndFree;  //имя файла уже задано в документе
    procedure FreeWithoutSaving;
    procedure SaveToFile(const filename: string); override;
    procedure Change; override;
    procedure ZoomIn;
    procedure ZoomOut;
  published
    property Btmp: TExtendedPNGObject read GetBtmp write SetBtmp stored false;
    property Scale: Real read Get_Scale stored false;
    property PrimaryColor: TColor read fPrimaryColor write fPrimaryColor;
    property SecondaryColor: TColor read fSecondaryColor write fSecondaryColor;
    property BrushSize: Integer read fBrushSize write fBrushSize;
    property BrushShape: TBrushShape read fBrushShape write fBrushShape;
    property ScaleNumber: integer read fScaleNumber write fScaleNumber;
  end;

  TDocumentsSavingProgress = class (TObject)
  private
    fAllDocsClearEvent: TEvent; //возвращаемся к истокам - работало неплохо
    fPrefetchedDoc: TRasterImageDocument;
    procedure ThreadTerminate(Sender: TObject);
    procedure WaitForAllDocsClearEvent;
  public
    fDocumentsSaving: TThreadList;
    onSaveThreadTerminate: TNotifyEvent;
    Log: TLogProc;
    constructor Create;
    destructor Destroy; override;
    function LoadDocFromFile(filename: string): TAbstractDocument;
    procedure PrefetchDocument(doc: TRasterImageDocument); //пусть загрузит на всякий случай
    function Count: Integer;  //debug
    function AsText: string; //debug
  end;
TSaveDocThread = class (TThread)
  private
    fPercentDone: Integer;  //а как результат для GetProgress
                            //вполне подходит Terminated
    procedure CallProgress;
    function GetProgressFromThread(Sender: TObject; PercentDone: Integer): boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;  //вызывается из Thread'a и должна выкинуть поток
    //из списка, после чего мирно удалиться
  public
    fDoc: TRasterImageDocument;
    constructor Create(doc: TRasterImageDocument);
end;

procedure CoverRect(var Orig: TRect;const newRect: TRect);
//почти как UnionRect, но более специализированная

var
  DocumentsSavingProgress: TDocumentsSavingProgress;  //для TRasterImageDocument

implementation

uses SysUtils,strUtils,math,typinfo,forms;

procedure CoverRect(var Orig: TRect; const newRect: TRect);
begin
  if IsRectEmpty(Orig) then Orig:=newRect
  else if not IsRectEmpty(newRect) then begin
    Orig.Left:=min(Orig.Left,newRect.Left);
    Orig.Right:=max(Orig.Right,newRect.Right);
    Orig.Top:=min(Orig.Top,newRect.Top);
    Orig.Bottom:=max(Orig.Bottom,newRect.Bottom);
  end;
end;

(*
    TAbstractGetImageThread
                              *)
(*
  этот объект является "владельцем" изображения, которое должен получить либо
  из файла, либо обработав какой-то другой объект, поддерживающий интерфейс
  IGetPngThread.

  Если мы обратимся к этому объекту, пока изображение еще не загружено, выполнение
  приостановится до тех пор, пока
  - загрузится изображение, мы получим его и продолжим работу
  - будет выполнен метод Terminate, объект возратит nil и уничтожится при первой
  возможности
  *)
function TAbstractGetImageThread._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TAbstractGetImageThread._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TAbstractGetImageThread.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

constructor TAbstractGetImageThread.Create;
begin
  inherited Create(true);
  fEvent:=TEvent.Create(nil,true,false,'');
end;

destructor TAbstractGetImageThread.Destroy;
begin
  //должен запуститься лишь тогда, когда никто не указывает на нас, поэтому
  //все просто.
  WaitFor;
  fBitmap.Free;
  fEvent.Free;
  inherited;
end;

procedure TAbstractGetImageThread.Halt;
begin
  Terminate;
  fEvent.SetEvent;
end;

function TAbstractGetImageThread.GetImage: TExtendedPngObject;
begin
  case fEvent.WaitFor(20000) of
    wrTimeout: raise Exception.Create('GetImage.fEvent timeout');
    wrError: raise Exception.Create('GetImage.fEvent error');
    wrAbandoned: raise Exception.Create('GetImage.fEvent abandoned');
  end;
  if (FatalException=nil) or fExceptionHandled then
    if Terminated then Result:=nil
    else Result:=fBitmap
  else begin
    fExceptionHandled:=true;
    Raise Exception.Create(Exception(FatalException).Message);
  end;
end;

procedure TAbstractGetImageThread.SetImage(value: TExtendedPngObject);
begin
  GetImage.Free;
  fBitmap:=value;
end;

(*
    TLoadBitmapThread
                        *)
(*
    Загружает картинку из файла
                                  *)
constructor TLoadBitmapThread.Create(aOnTerminate: TNotifyEvent; filename: string='');
begin
  inherited Create; //TAbstractGetImageThread.Create
  OnTerminate:=aOnTerminate;
  fFileName:=filename;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  fBitmap:=TExtendedPngObject.Create;
  Resume;
end;

procedure TLoadBitmapThread.Execute;
var ext: string;
    pic: TPicture;
begin
  try
  if fFileName<>'' then begin
    ext:=Uppercase(ExtractFileExt(fFileName));
    if ext='.PNG' then fBitmap.LoadFromFile(fFileName)
    else begin
      pic:=TPicture.Create;
      try
        pic.LoadFromFile(fFileName);
        if pic.Graphic is TBitmap then fBitmap.Assign(TBitmap(pic.Graphic))
        else fBitmap.Canvas.Draw(0,0,pic.Graphic);
      finally
        pic.Free;
      end;
    end;
  end;
  finally
    fEvent.SetEvent;
  end;
end;
(*
    TScalingThread
                      *)
constructor TScalingThread.Create(aGetImageIntf: IGetPngThread; aScale: Real);
begin
  inherited Create;
  fGetImageIntf:=aGetImageIntf;
  fScale:=aScale;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  Resume;
end;

procedure TScalingThread.Execute;
var img: TExtendedPNGObject;
begin
  //чувствуется, придется самостоятельно реализовывать масштабирование
  img:=fGetImageIntf.GetImage; //возможно ожидание, когда нам дадут, наконец, картинку
  if Assigned(img) and not Terminated then
    fBitmap:=img.Get2TimesDownScaled;
  fEvent.SetEvent;
end;

procedure T2to3ScalingThread.Execute;
var img: TExtendedPNGObject;
begin
  img:=fGetImageIntf.GetImage;
  if Assigned(img) and not Terminated then
    fBitmap:=img.Get2To3DownScaled;
  fEvent.SetEvent;
end;

procedure TScalingThread.UpdateRect(aRect: TRect);
begin
  fGetImageIntf.GetImage.DownScale2TimesInto(aRect,fBitmap);
end;

procedure T2to3ScalingThread.UpdateRect(aRect: TRect);
begin
  fGetImageIntf.GetImage.DownScale2To3Into(aRect,fBitmap);
end;

function TScalingThread.GetScale: Real;
begin
  Result:=fScale;
end;

(*
    TSaveDocThread
                      *)
constructor TSaveDocThread.Create(doc: TRasterImageDocument);
begin
  inherited Create(true);
  fDoc:=doc;
  DocumentsSavingProgress.fAllDocsClearEvent.ResetEvent;
  Assert(Assigned(fDoc));
  //сначала предупредим всех, что мы сохраняемся
  DocumentsSavingProgress.fDocumentsSaving.Add(fDoc);
  onTerminate:=documentsSavingProgress.ThreadTerminate;
  FreeOnTerminate:=true;
  Priority:=tpLower;
  Resume;
end;

function TSaveDocThread.GetProgressFromThread(Sender: TObject; PercentDone: Integer): Boolean;
begin
//перевалочный пункт между потоками - протискиваемся сквозь synchronize
  fPercentDone:=PercentDone;
  Synchronize(CallProgress);
  Result:=not Terminated;
end;

procedure TSaveDocThread.CallProgress;
begin
  if assigned(fDoc.onSavingProgress) then
    if not fDoc.onSavingProgress(self,fPercentDone) then
      Terminate;
end;

procedure TSaveDocThread.Execute;
var INeedAVacation: Boolean;
    i: Integer;
begin
  Assert(Assigned(fDoc));
  fDoc.Btmp.onSavingProgress:=GetProgressFromThread;
  fDoc.CriticalSection.Acquire;
  try
    SafeSaveToFile(fDoc.SaveToFile,fDoc.FileName);
//    fDoc.SaveToFile(fDoc.FileName); //весьма вероятна ошибка (файл используется и др)
  finally
    fDoc.CriticalSection.Release;
  end;
  //при возникновении искл. ситуации документ остаётся висеть в списке - пользователь
  //должен либо перезапустить поток сохранения (retry)
  //либо отменить сохранение

  //если мы в списке, значит, пора уходить, а вот если нет, значит народ уже передумал
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do begin
      i:=IndexOf(fDoc);
      INeedAVacation:=(i>=0);
      if INeedAVacation then
        Delete(i);
    end;
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  if INeedAVacation then fDoc.Release;
end;

procedure TSaveDocThread.DoTerminate;
var INeedAVacation: Boolean;
begin
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do
      INeedAVacation:=(Count=0);
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  if INeedAVacation then DocumentsSavingProgress.fAllDocsClearEvent.SetEvent;
  inherited;
  //вызов onTerminate
  //если осн. поток застрял на AllDocsClearEvent.WaitFor, то мы застрянем здесь же
  //пока последний из могикан наконец не создаст это событие
  //похоже, что onTerminate при этом не будут обработаны, поскольку уже началось
  //уничтожение формы.
  //но это даже хорошо, хотя FastMM4 выругается.
end;

(*
    TDocumentsSavingProgress
                                *)
constructor TDocumentsSavingProgress.Create;
begin
  fDocumentsSaving:=TThreadList.Create;
  fDocumentsSaving.Duplicates:=dupIgnore;
//  fDocumentsSaving.Duplicates:=dupError;
  fAllDocsClearEvent:=TEvent.Create(nil,false,true,''); //не хотим имени, чтобы
  //одновременно запущенные 2 проги не "сцепились"
end;

procedure TDocumentsSavingProgress.ThreadTerminate(Sender: TObject);
begin
  if Assigned(onSaveThreadTerminate) then
    onSaveThreadTerminate(Sender);
end;

procedure TDocumentsSavingProgress.WaitForAllDocsClearEvent;
var i: Integer;
begin
  i:=0;
  while i<120 do begin
    case fAllDocsClearEvent.WaitFor(500) of
      wrSignaled: Exit;
      wrError: raise Exception.Create('WaitForAllDocsClearEvent error');
      wrAbandoned: raise Exception.Create('WaitForAllDocsClearEvent abandoned');
    end;
    CheckSynchronize(500);
    inc(i);
  end;
  raise Exception.Create('WaitForAllDocsClearEvent timeout');
end;

destructor TDocumentsSavingProgress.Destroy;
//var i: Integer;
begin
  if Assigned(fPrefetchedDoc) then
    fPrefetchedDoc.SaveAndFree;
//    fPrefetchedDoc.Release;
  try
    log('wait for all docs saved and cleared');
    log('number of docs: ' + IntToStr(Count));
    log(AsText);
    WaitForAllDocsClearEvent;
  finally
    fAllDocsClearEvent.Free;
    fDocumentsSaving.Free;
    inherited Destroy;
  end;
end;

function TDocumentsSavingProgress.LoadDocFromFile(filename: string): TAbstractDocument;
var i: Integer;
  doc: TAbstractDocument;
begin
  Result:=nil;
  if Assigned(fPrefetchedDoc) and (fPrefetchedDoc.FileName=filename) then begin
    Result:=fPrefetchedDoc;
    fPrefetchedDoc:=nil;
  end;
  try
    with fDocumentsSaving.LockList do
      for i:=0 to Count-1 do begin
        doc:=TAbstractDocument(Items[i]);
        if doc.FileName=filename then begin
          //этот проект у нас сохраняется - надо ему "сказать", чтобы не спешил освобождать
          //память, а ссылку можем сразу дать, внут. крит. секция не позволит изменить док.
          //пока он не сохранится в нынешнем виде
          Result:=doc;
          //удалим его из списка "на сохранение и удаление" - он поймет
          Delete(i);
          break;
        end;
      end;
  finally
    fDocumentsSaving.UnlockList;
  end;
end;

procedure TDocumentsSavingProgress.PrefetchDocument(doc: TRasterImageDocument);
begin
//  FreeAndNil(fPrefetchThread);  //если уже загружется другой документ - мы передумали
  if Assigned(fPrefetchedDoc) then begin
    //debug
    log('releasing prefetched '+ExtractFileName(fPrefetchedDoc.FileName)+'; addr='+IntToHex(Integer(fPrefetchedDoc),8));
    fPrefetchedDoc.SaveAndFree;
  end;
//  fPrefetchedDoc.SaveAndFree; //потихоньку уничтожается, в т.ч из списка уйти должна
  fPrefetchedDoc:=doc;
  if Assigned(doc) then begin
    log('prefetched doc: '+ExtractFileName(fPrefetchedDoc.FileName)+'; addr='+IntToHex(Integer(fPrefetchedDoc),8));
  //документ загр. сразу, а вот картинку он тянет фоновым потоком
    log('list count: '+IntToStr(count));
    log('list of docs in progress (saving and destroying):');
    log(AsText);
  end;
end;

function TDocumentsSavingProgress.Count: Integer;
begin
  with fDocumentsSaving.LockList do
    Result:=Count;
  fDocumentsSaving.UnlockList;
end;

function TDocumentsSavingProgress.AsText: string;
var i: Integer;
begin
  with fDocumentsSaving.LockList do begin
    for i:=0 to Count-1 do
      Result:=Result+IntToHex(Integer(Items[i]),8)+': '+TRasterImageDocument(Items[i]).FileName+#13+#10;
  end;
  fDocumentsSaving.UnlockList;
end;


(*
    TRasterImageDocument
                            *)
constructor TRasterImageDocument.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLoadThread:=TLoadBitmapThread.Create(LoadThreadTerminate);
  BrushSize:=10;
  PrimaryColor:=clWhite;
  fScaleLevelsReadyEvent:=TEvent.Create(nil,true,false,'');
end;

constructor TRasterImageDocument.CreateFromImageFile(aFileName: string);
begin
  Create(nil);
  fLoadThread.Halt;
  fLoadThread:=TLoadBitmapThread.Create(LoadThreadTerminate,aFileName);
end;

constructor TRasterImageDocument.LoadFromFile(const aFilename: string);
var s: string;
begin
  inherited LoadFromFile(aFilename);
  s:=ExtractFilePath(aFilename);
  s:=LeftStr(s,Length(s)-5);  //выкинули \DLRN
  fLoadThread.Halt;
  fLoadThread:=TLoadBitmapThread.Create(LoadThreadTerminate,s+ChangeFileExt(ExtractFileName(aFilename),'.png'));
end;

destructor TRasterImageDocument.Destroy;
var i: Integer;
begin
  fLoadThread.Halt;
  for i:=0 to Length(fScalingThreads)-1 do
    fScalingThreads[i].Halt;
  fScaleLevelsReadyEvent.Free;
  inherited Destroy;
end;

function TRasterImageDocument.Get_Image: TImage;
begin
  Result:=Image;
end;

procedure TRasterImageDocument.WaitScaleLevelsReady;
var i: Integer;
begin
  i:=0;
  while i<120 do begin
    case fScaleLevelsReadyEvent.WaitFor(500) of
      wrSignaled: Exit;
      wrError: raise Exception.Create('WaitScaleLevelsReady error');
      wrAbandoned: raise Exception.Create('WaitScaleLevelsReady abandoned');
    end;
    CheckSynchronize(500);
    inc(i);
  end;
  raise Exception.Create('WaitScaleLevelsReady timeout');
end;

function TRasterImageDocument.Get_Scale: Real;
begin
  WaitScaleLevelsReady;
  assert((scaleNumber>=0) and (scaleNumber<=Length(fScalingThreads)));
  if scaleNumber=0 then Result:=1
  else Result:=fScalingThreads[scaleNumber-1].GetScale;
end;

function TRasterImageDocument.Get_Scaled_Btmp: TExtendedPngObject;
begin
  WaitScaleLevelsReady;
  assert((scaleNumber>=0) and (scaleNumber<=Length(fScalingThreads)));
  if scaleNumber=0 then Result:=fLoadThread.GetImage
  else Result:=fScalingThreads[scaleNumber-1].GetImage;
end;

procedure TRasterImageDocument.SaveAndFree;
begin
  if Assigned(self) then
    TSaveDocThread.Create(self);
end;

procedure TRasterImageDocument.FreeWithoutSaving;
begin
  if Assigned(self) then
    with documentsSavingProgress.fDocumentsSaving.LockList do
      remove(self);
  Destroy;
end;

procedure TRasterImageDocument.SaveToFile(const filename: string);
var s,fn1: string;
begin
  if Changed or not FileExists(filename) then
    inherited SaveToFile(filename);
  s:=ExtractFilePath(filename);
  s:=LeftStr(s,Length(s)-5);
  fn1:=s+ChangeFileExt(ExtractFileName(filename),'.png');
  If Changed or not FileExists(fn1) then
    SafeSaveToFile(btmp.SmartSaveToFile,fn1);
//    btmp.SmartSaveToFile(fn1);
  initial_pos:=UndoTree.current;
  new_commands_added:=false;
end;

function TRasterImageDocument.GetBtmp: TExtendedPngObject;
begin
  Result:=fLoadThread.GetImage;
end;

procedure TRasterImageDocument.SetBtmp(value: TExtendedPngObject);
begin
  fLoadThread.SetImage(value);
end;

procedure TRasterImageDocument.LoadThreadTerminate(Sender: TObject);
var t: string;
    i,c: Integer;
    s: Real;
    thread: TLoadBitmapThread;
begin
  thread:=Sender as TLoadBitmapThread;
  if Assigned(thread.FatalException) then begin
    t:=Format('Не удалось загрузить изображение %s: %s',
      [thread.fFilename,Exception(thread.FatalException).Message]);
    Application.MessageBox(@t[1],'AMBIC');
    Exit;
  end;
  //ладно, смасштабируем здесь. Пока ровно через 2
  if (thread.fBitmap.Width=0) or (Thread.fBitmap.Height=0) then Exit;

  c:=Floor(log2(min(Thread.fBitmap.Width,Thread.fBitmap.Height)))-1;
  SetLength(fScalingThreads,2*c);
  s:=2/3;
  fScalingThreads[0]:=T2to3ScalingThread.Create(fLoadThread,s);
  fScalingThreads[1]:=TScalingThread.Create(fLoadThread,s*3/4);
  for i:=1 to c-1 do begin
    s:=s/2;
    fScalingThreads[2*i]:=TScalingThread.Create(fScalingThreads[2*(i-1)],s);
    fScalingThreads[2*i+1]:=TScalingThread.Create(fScalingThreads[2*i-1],s*3/4);
  end;
  fScaleLevelsReadyEvent.SetEvent;
end;

procedure TRasterImageDocument.AddToChangeRect(const A: TRect);
begin
  CoverRect(ChangeRect,A);
end;

procedure TRasterImageDocument.SizesChanged;
begin
  fSizesChanged:=true;
end;


procedure TRasterImageDocument.Change;
var i: Integer;
begin
  //прорисуем изменения, причем при первом включении ChangeRect должен равняться
  //всему изображению!
  if fSizesChanged then begin
    fScalingThreads[0].SetImage(btmp.Get2To3DownScaled);
    fScalingThreads[1].SetImage(btmp.Get2TimesDownScaled);
    for i:=0 to (Length(fScalingThreads) div 2)-2 do begin
      fScalingThreads[i*2+2].SetImage(fScalingThreads[i*2].GetImage.Get2TimesDownscaled);
      fScalingThreads[i*2+3].SetImage(fScalingThreads[i*2+1].GetImage.Get2TimesDownscaled);
    end;
  end
  else if not IsRectEmpty(ChangeRect) then begin
    btmp.DownScale2To3Into(ChangeRect,fScalingThreads[0].getImage);
    btmp.DownScale2TimesInto(ChangeRect,fScalingThreads[1].getImage);
    for i:=0 to (Length(fScalingThreads) div 2)-2 do begin
      fScalingThreads[i*2].GetImage.DownScale2TimesInto(ChangeRect,fScalingThreads[i*2+2].getImage);
      fScalingThreads[i*2+1].GetImage.DownScale2TimesInto(ChangeRect,fScalingThreads[i*2+3].getImage);
    end;
  end;
  fSizesChanged:=false;
  ChangeRect:=Rect(0,0,0,0);
  inherited Change;
end;

procedure TRasterImageDocument.ZoomIn;
begin
  if fScaleNumber>0 then begin
    dec(fScaleNumber);
    Change;
  end;
end;

procedure TRasterImageDocument.ZoomOut;
begin
  if fScaleNumber<Length(fScalingThreads) then begin
    inc(fScaleNumber);
    Change;
  end;
end;


initialization
  RegisterClasses([TRasterImageDocument]);
  DocumentsSavingProgress:=TDocumentsSavingProgress.Create;
finalization
  DocumentsSavingProgress.Free;
end.
