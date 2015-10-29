unit Image_on_demand;

interface

uses classes,streaming_class_lib, syncObjs, pngImage, command_class_lib,graphics,
  IGraphicObject_commands, ExtCtrls, Types;

type
(*
TImageOnDemandStatus = (iodWaitForLoading,iodImageInMemory,iodImageSaved);

TImageOnDemand = class (TStreamingClass)
  private
    fImage: TPngObject;
    fCriticalSection: TCriticalSection;

end;
*)
TAsyncSavePNG = class (TPngObject)
  public
    procedure SaveToFileAndFree(filename: string);
end;

TSavePNGThread = class (TThread)
  private
    fImg: TAsyncSavePNG;
    fFileName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Img: TAsyncSavePNG; filename: string);
end;

TImageSavingProgress = class (TObject)
  private
    fCounter: Integer;
    fCriticalSection: TCriticalSection;
    fAllClearEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override; //освободить объекты и дождаться завершения всех потоков
    procedure WaitForAllClearEvent;
    procedure SaveAndFreeStartup;
    procedure SaveAndFreeTermination(Sender: TObject);  //notify event
    property Counter: Integer read fCounter;
  end;

(*
TDocumentPrefetchThread = class (TThread)
  private
    fFileName: string;
  protected
    procedure Execute; override;
    function GetDoc: TAbstractDocument;
  public
    constructor Create(Filename: string);
end;
*)

TLoadBitmapThread = class (TThread) //будет отвечать за картинку головой!
  private
    fFilename: string;
    fBitmap: TAsyncSavePNG;
  protected
    procedure Execute; override;
  public
    constructor CreateBlank;
    constructor Create(filename: string);
    destructor Destroy; override;
    function GetBitmap: TAsyncSavePNG;
end;

TScalingThread = class (TThread)
  private
    fOrig: TAsyncSavePNG; //ссылка на оригинальную картинку
    fScale: Real;
    fbitmap: TBitmap;
  protected
    procedure Execute; override;
  public
    constructor Create(aOrig: TAsyncSavePNG; ascale: Real);
    destructor Destroy; override;
    function GetScaled: TBitmap;
end;


TRasterImageDocument = class (TDocumentWithImage, IConstantComponentName)
  private
//    fBtmp: TAsyncSavePNG;
    fRefCount: Integer;
    fPrimaryColor: TColor;
    fSecondaryColor: TColor;
    fBrushSize: Integer;
    fScale: Real;
    fScaleMultiplier: Real;
    fLoadThread: TLoadBitmapThread;
    fScalingThreads: array of TScalingThread;
    procedure LoadThreadTerminate(Sender: TObject);
    function GetBtmp: TAsyncSavePNG;
  public
    Image: TImage;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    constructor Create(aOwner: TComponent); override;
    constructor LoadFromFile(aFilename: string); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
    procedure SaveAndFree;  //имя файла уже задано в документе
    procedure SaveToFile(filename: string); override;
    procedure PrepareScales(scaleMultiplier: Real);
  published
    property Btmp: TAsyncSavePNG read GetBtmp stored false;
    property PrimaryColor: TColor read fPrimaryColor write fPrimaryColor;
    property SecondaryColor: TColor read fSecondaryColor write fSecondaryColor;
    property BrushSize: Integer read fBrushSize write fBrushSize;
    property Scale: Real read fScale write fScale;
  end;

TDocumentsSavingProgress = class (TObject)
  private
    fDocumentsSaving: TThreadList;
    fAllDocsClearEvent: TEvent;
//    fPrefetchThread: TDocumentPrefetchThread;
    fPrefetchedDoc: TRasterImageDocument;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WaitForAllDocsClearEvent;
    function LoadDocFromFile(filename: string): TAbstractDocument;
    procedure PrefetchDocument(doc: TRasterImageDocument); //пусть загрузит на всякий случай
    function Count: Integer;  //debug
  end;

TSaveDocThread = class (TThread)
  private
    fDoc: TRasterImageDocument;
  protected
    procedure Execute; override;
    procedure AfterTermination(Sender: TObject);
  public
    constructor Create(doc: TRasterImageDocument);
end;

TRasterImageDocumentCommand = class (TAbstractTreeCommand)
  public
    function GetDoc: TRasterImageDocument;
  end;

TPatchImageCommand = class (TRasterImageDocumentCommand)
//базовая команда при работе с растровыми изображениями
  protected
    fDiff: TPngObject;
    fPredictor: TPngObject;
    fLeft,fTop,fRight,fBottom: Integer; //расположение заплатки
    procedure GetBounds; virtual; abstract;//инициализировать fRect
    function UndoPrediction: Boolean; virtual;  //false означает
    //что мы ничего не можем предсказать, поэтому оставляем картинку как есть
    function InternalExecute: Boolean; virtual; abstract; //false означает,
    //что применение команды ничего не изменило и ее стоит изъять из дерева
  public
    constructor Create(aOwner: TComponent); overload; override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Undo: Boolean; override;
  published
    property diff: TPngObject read fDiff write fDiff;
  end;

TRectBrushCommand = class (TPatchImageCommand)
//закрашивает прямоугольник текущей ширины (заданной в BrushSize) осн. цветом
//реальное движение кисти порождает десятки, сотни таких команд.
//если после прохода ничего вообще не поменялось, команда не сохраняется
  private
    fX,fY: Integer; //коорд. центра
  protected
    procedure GetBounds; override;
    function InternalExecute: Boolean; override;
  public
    constructor Create(aX,aY: Integer); reintroduce; overload;
  published
    property X: Integer read fX write fX;
    property Y: Integer read fY write fY;
  end;

var
  ImageSavingProgress: TImageSavingProgress;  //для одиночных TAsyncSavePNG
  DocumentsSavingProgress: TDocumentsSavingProgress;  //для TRasterImageDocument


implementation
uses SysUtils,strUtils,gamma_function,math;
(*
      TAsyncSavePNG
                          *)
procedure TAsyncSavePNG.SaveToFileAndFree(filename: string);
begin
  TSavePNGThread.Create(self,filename); //запустится, выполнится и сам удалит
end;

(*
      TSavePNGThread
                        *)
constructor TSavePNGThread.Create(Img: TAsyncSavePNG; filename: string);
begin
  inherited Create(true);
  fImg:=Img;
  fFilename:=filename;
  FreeOnTerminate:=true;
  ImageSavingProgress.SaveAndFreeStartup;
  Resume;
end;

procedure TSavePNGThread.Execute;
begin
  fImg.SaveToFile(fFilename);
  fImg.Free;
  ImageSavingProgress.SaveAndFreeTermination(self);
end;

(*
    TSaveDocThread
                      *)
constructor TSaveDocThread.Create(doc: TRasterImageDocument);
begin
  inherited Create(true);
  fDoc:=doc;
  //сначала предупредим всех, что мы сохраняемся
  DocumentsSavingProgress.fAllDocsClearEvent.ResetEvent;
  DocumentsSavingProgress.fDocumentsSaving.Add(fDoc);
  FreeOnTerminate:=true;
  Priority:=tpLower;
  onTerminate:=AfterTermination;
  Resume;
end;

procedure TSaveDocThread.Execute;
var INeedAVacation: Boolean;
    i: Integer;
begin
  fDoc.CriticalSection.Acquire;
    fDoc.SaveToFile(fDoc.FileName);
  fDoc.CriticalSection.Release;
  //если мы в списке, значит, пора уходить, а вот если нет, значит народ уже передумал
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do begin
      i:=IndexOf(fDoc);
      INeedAVacation:=(i>=0);
      if INeedAVacation then begin
        Delete(i);
        if Count=0 then DocumentsSavingProgress.fAllDocsClearEvent.SetEvent;
      end;
    end;
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  if INeedAVacation then fDoc.Free;
end;

procedure TSaveDocThread.AfterTermination(Sender: TObject);
begin
  if Assigned(FatalException) then
    Raise FatalException;
end;

(*
    TLoadBitmapThread
                              *)
constructor TLoadBitmapThread.Create(filename: string);
begin
  inherited Create(true);
  FreeAndNil(fBitmap);
  fFileName:=filename;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  Resume;
end;

constructor TLoadBitmapThread.CreateBlank;
begin
  inherited Create(true);
  FreeAndNil(fBitmap);
  fBitmap:=TAsyncSavePng.CreateBlank(color_RGB,8,0,0);
  fFileName:='';
  Resume;
  //нет нужды вообще запускать поток - уже всё есть
end;

destructor TLoadBitmapThread.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

procedure TLoadBitmapThread.Execute;
var ext: string;
    pic: TPicture;
begin
  if fFileName='' then Exit;
  fBitmap:=TAsyncSavePNG.Create;
  ext:=Uppercase(ExtractFileExt(fFileName));
  if ext='.PNG' then
    fBitmap.LoadFromFile(fFileName)
  else begin
    pic:=TPicture.Create;
    pic.LoadFromFile(fFileName);
    fBitmap.Resize(pic.Width,pic.Height);
    fBitmap.Canvas.Draw(0,0,pic.Graphic);
  end;
end;

function TLoadBitmapThread.GetBitmap: TAsyncSavePNG;
begin
  WaitFor;
  Result:=fBitmap;
end;

(*
    TScalingThread
                      *)
constructor TScalingThread.Create(aOrig: TAsyncSavePNG; aScale: Real);
begin
  inherited Create(true);
  fOrig:=aOrig;
  fScale:=aScale;
  fBitmap:=TBitmap.Create;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  Resume;
end;

destructor TScalingThread.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

procedure TScalingThread.Execute;
begin
  fBitmap.Width:=Round(fOrig.Width/fscale);
  fBitmap.Height:=Round(fOrig.Height/fscale);
  fBitmap.Canvas.CopyRect(Rect(0,0,fBitmap.Width,fBitmap.Height),fOrig.Canvas,Rect(0,0,fOrig.Width,fOrig.Height));
end;

function TScalingThread.GetScaled: TBitmap;
begin
  Waitfor;
  Result:=fBitmap;
end;

(*
    TImageSavingProgress
                              *)
constructor TImageSavingProgress.Create;
begin
  inherited Create;
  fCriticalSection:=TCriticalSection.Create;
  fAllClearEvent:=TEvent.Create(nil,false,true,'ImageOnDemandAllClearEvent');
end;

procedure TImageSavingProgress.WaitForAllClearEvent;
begin
  case fAllClearEvent.WaitFor(20000) of
    wrTimeout: raise Exception.Create('WaitForAllClearEvent timeout');
    wrError: raise Exception.Create('WaitForAllClearEvent error');
    wrAbandoned: raise Exception.Create('WaitForAllClearEvent abandoned');
  end;
end;

destructor TImageSavingProgress.Destroy;
begin
  try
    WaitForAllClearEvent;
  finally
    fAllClearEvent.Free;
    fCriticalSection.Free;
    inherited Destroy;
  end;
end;

procedure TImageSavingProgress.SaveAndFreeStartup;
begin
  fCriticalSection.Acquire;
  inc(fCounter);
  fAllClearEvent.ResetEvent;
  fCriticalSection.Leave;
end;

procedure TImageSavingProgress.SaveAndFreeTermination(Sender: TObject);
begin
  fCriticalSection.Acquire;
  dec(fCounter);
  if fCounter=0 then
    fAllClearEvent.SetEvent;
  fCriticalSection.Leave;
end;

(*
    TDocumentsSavingProgress
                                *)
constructor TDocumentsSavingProgress.Create;
begin
  fDocumentsSaving:=TThreadList.Create;
  fDocumentsSaving.Duplicates:=dupIgnore;
//  fDocumentsSaving.Duplicates:=dupError;
  fAllDocsClearEvent:=TEvent.Create(nil,false,true,'AllDocsClearEvent');
end;

procedure TDocumentsSavingProgress.WaitForAllDocsClearEvent;
begin
  case fAllDocsClearEvent.WaitFor(20000) of
    wrTimeout: raise Exception.Create('WaitForAllDocsClearEvent timeout');
    wrError: raise Exception.Create('WaitForAllDocsClearEvent error');
    wrAbandoned: raise Exception.Create('WaitForAllDocsClearEvent abandoned');
  end;
end;

destructor TDocumentsSavingProgress.Destroy;
//var //i,j: Integer;
//    s: string;
//    P: Pointer;
begin
  if Assigned(fPrefetchedDoc) then
    fPrefetchedDoc._Release;
  try
//    i:=Count;
//    Assert(i>0);
//    with fDocumentsSaving.LockList do
//      for j:=0 to i-1 do begin
//        s:=TRasterImageDocument(Items[j]).FileName;
//        P:=Items[j];
//        assert(s<>'');
//        assert(P<>nil);
//      end;
//    fDocumentsSaving.UnlockList;

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
  if Assigned(fPrefetchedDoc) then fPrefetchedDoc._Release;
//  fPrefetchedDoc.SaveAndFree; //потихоньку уничтожается, в т.ч из списка уйти должна
  fPrefetchedDoc:=doc;
  fPrefetchedDoc._AddRef;
  //документ загр. сразу, а вот картинку он тянет фоновым потоком
  fDocumentsSaving.Add(fPrefetchedDoc);
end;

function TDocumentsSavingProgress.Count: Integer;
begin
  with fDocumentsSaving.LockList do
    Result:=Count;
  fDocumentsSaving.UnlockList;
end;


(*
    TRasterImageDocument
                            *)
constructor TRasterImageDocument.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  inc(fRefCount);
  fLoadThread:=TLoadBitmapThread.CreateBlank;
  fLoadThread.OnTerminate:=LoadThreadTerminate;
  BrushSize:=10;
  PrimaryColor:=clWhite;
  scale:=1;
end;

procedure TRasterImageDocument.AfterConstruction;
begin
  inherited AfterConstruction;
  dec(fRefCount);
end;

destructor TRasterImageDocument.Destroy;
var i: Integer;
begin
  fLoadThread.Free;
  for i:=0 to Length(fScalingThreads)-1 do
    fScalingThreads[i].free;
  inherited Destroy;
end;

function TRasterImageDocument._AddRef: Integer;
begin
  inc(fRefCount);
  Result:=fRefCount;
end;

function TRasterImageDocument._Release: Integer;
begin
  dec(fRefCount);
  Result:=fRefCount;
  if fRefCount=0 then
    SaveAndFree;
end;

function TRasterImageDocument.Get_Image: TImage;
begin
  Result:=Image;
end;

procedure TRasterImageDocument.SaveAndFree;
begin
  if Assigned(self) then
    TSaveDocThread.Create(self);
end;

procedure TRasterImageDocument.SaveToFile(filename: string);
var s: string;
begin
  if Changed or not FileExists(filename) then
    inherited SaveToFile(filename);
  s:=ExtractFilePath(filename);
  s:=LeftStr(s,Length(s)-5);
  filename:=s+ChangeFileExt(ExtractFileName(filename),'.png');
  If Changed or not FileExists(filename) then
    btmp.SaveToFile(filename);
  initial_pos:=UndoTree.current;
  new_commands_added:=false;
end;

function TRasterImageDocument.GetBtmp: TAsyncSavePNG;
begin
  Result:=fLoadThread.GetBitmap;
end;

constructor TRasterImageDocument.LoadFromFile(aFilename: string);
var s: string;
begin
  inherited LoadFromFile(aFilename);
  s:=ExtractFilePath(aFilename);
  s:=LeftStr(s,Length(s)-5);  //выкинули \DLRN
  fLoadThread.Create(s+ChangeFileExt(ExtractFileName(aFilename),'.png'));
//  btmp.LoadFromFile(s+ChangeFileExt(ExtractFileName(aFilename),'.png'));
end;

procedure TRasterImageDocument.PrepareScales(scaleMultiplier: Real);
begin
  fScaleMultiplier:=scaleMultiplier;
end;

procedure TRasterImageDocument.LoadThreadTerminate(Sender: TObject);
var c,i: Integer; //количество копий
    s: Real;
begin
  if Assigned(fLoadThread.FatalException) then
    raise fLoadThread.FatalException;
  if fScaleMultiplier=0 then Exit;
  c:=Floor(ln(min(fLoadThread.GetBitmap.Width,fLoadThread.GetBitmap.Height) div 2)/ln(fScaleMultiplier));
  SetLength(fScalingThreads,c);
  s:=1;
  for i:=0 to c-1 do begin
    s:=s/fScaleMultiplier;
    fScalingThreads[i].Free;
    fScalingThreads[i]:=TScalingThread.Create(fLoadThread.GetBitmap,s);
  end;
end;

(*
      TRasterImageDocumentCommand
                                      *)
function TRasterImageDocumentCommand.GetDoc: TRasterImageDocument;
begin
  Result:=FindOwner as TRasterImageDocument;
end;

(*
      TPatchImageCommand
                            *)
constructor TPatchImageCommand.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fDiff:=TPngObject.CreateBlank(color_RGB,8,0,0); //будет сохраняться в файл
  fDiff.Filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  fDiff.CompressionLevel:=9;
  fPredictor:=TPngObject.CreateBlank(color_RGB,8,0,0);  //хранится временно
end;

destructor TPatchImageCommand.Destroy;
begin
  fDiff.Free;
  fPredictor.Free;
  inherited Destroy;
end;

function TPatchImageCommand.Execute: Boolean;
var i,j: Integer;
  C1,C2: RGBColor;
begin
  getBounds;
  //обозначили заплатку
  fDiff.Resize(fRight-fLeft,fBottom-fTop);
  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),GetDoc.Btmp.Canvas,
    Rect(fLeft,fTop,fRight,fBottom));
  //храним в fDiff копию того фрагмента, который начнем мучать
  Result:=InternalExecute;
  if Result then begin
    fPredictor.Resize(fDiff.Width,fDiff.Height);
    if UndoPrediction then
      //нарисует на Predictor исх. картинку исходя из того, что он
      //может знать по конечному результату
      //сейчас "скользкая" часть - вычесть одну картинку из другой
      //может оказаться довольно долгой
      for j:=0 to fDiff.Height-1 do
        for i:=0 to fDiff.Width-1 do begin
          C1.Color:=fDiff.Pixels[i,j];
          C2.Color:=fPredictor.Pixels[i,j];
          C1.R:=128+C1.R-C2.R;
          C1.G:=128+C1.G-C2.G;
          C1.B:=128+C1.B-C2.B;
          fDiff.Pixels[i,j]:=C1.Color;
        end;
    //а на нет и суда нет
  end;
  //в противном случае команда вот-вот будет удалена, нет нужды освобождать
  //память впереди паровоза
end;

function TPatchImageCommand.Undo: Boolean;
var i,j: Integer;
    C1,C2: RGBColor;
begin
  getBounds;  //размеры заплатки мы могли бы по изображению понять, но расположение
  //все равно узнавать надо!
  fPredictor.Resize(fDiff.Width,fDiff.Height);
  if UndoPrediction then
    for j:=0 to fDiff.Height-1 do
      for i:=0 to fDiff.Width-1 do begin
        C1.Color:=fDiff.Pixels[i,j];
        C2.Color:=fPredictor.Pixels[i,j];
        C1.R:=C1.R+C2.R-128;
        C1.G:=C1.G+C2.G-128;
        C1.B:=C1.B+C2.B-128;
        fDiff.Pixels[i,j]:=C1.Color;
      end;
    //а на нет и суда нет
  //осталось вправить картинку на место
  GetDoc.Btmp.Canvas.CopyRect(Rect(fLeft,fTop,fRight,fBottom),fDiff.Canvas,Rect(0,0,fDiff.Width,fDiff.Height));
  Result:=true;
end;

function TPatchImageCommand.UndoPrediction: Boolean;
begin
  Result:=false;
end;

(*
    TRectBrushCommand
                              *)
constructor TRectBrushCommand.Create(aX,aY: Integer);
begin
  Create(nil);
  X:=aX;
  Y:=aY;
end;

procedure TRectBrushCommand.GetBounds;
var size: Integer;
begin
  size:=Round(GetDoc.BrushSize/GetDoc.scale);
  fLeft:=X-size;
  fRight:=X+size;
  fTop:=Y-size;
  fBottom:=Y+size;
end;

function TRectBrushCommand.InternalExecute: Boolean;
var PrimeCol: TColor;
    i,j: Integer;
begin
  PrimeCol:=GetDoc.PrimaryColor;
  //пока поступим упрощенно
  with GetDoc.Btmp.Canvas do begin
    Brush.Color:=PrimeCol;
    for i:=fLeft to fRight-1 do
      for j:=fTop to fBottom-1 do
        if Pixels[i,j]<>PrimeCol then begin
          FillRect(Rect(fLeft,fTop,fRight,fBottom));
          Result:=true;
          Exit;
        end;
  end;
  //дошли досюда, значит, так ничего и не закрасили
  Result:=false;
end;


initialization
  RegisterClasses([TRasterImageDocument,TRectBrushCommand]);
  ImageSavingProgress:=TImageSavingProgress.Create;
  DocumentsSavingProgress:=TDocumentsSavingProgress.Create;
finalization
  ImageSavingProgress.Free;
  DocumentsSavingProgress.Free;

end.
