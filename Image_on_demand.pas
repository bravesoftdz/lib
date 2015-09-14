unit Image_on_demand;

interface

uses classes,streaming_class_lib, syncObjs, pngImage, command_class_lib,graphics,
  IGraphicObject_commands, ExtCtrls;

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

TDocumentsSavingProgress = class (TObject)
  private
    fDocumentsSaving: TThreadList;
    fAllDocsClearEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WaitForAllDocsClearEvent;
    function LoadDocFromFile(filename: string): TAbstractDocument;
  end;

TRasterImageDocument = class (TDocumentWithImage)
  private
    fBtmp: TAsyncSavePNG;
  public
    Image: TImage;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
    procedure SaveAndFree;  //имя файла уже задано в документе
    property Btmp: TAsyncSavePNG read fBtmp;
  end;

TSaveDocThread = class (TThread)
  private
    fDoc: TRasterImageDocument;
  protected
    procedure Execute; override;
  public
    constructor Create(doc: TRasterImageDocument);
end;

TPatchImageCommand = class (THashedCommand)
  private //базовая команда при работе с растровыми изображениями
    fLeft,fRight,fTop,fBottom: Integer; //рабочий участок
    fDiff: TPngObject;
  public
    constructor Create(aOwner: TComponent); overload; override;
    constructor Create(aLeft,aTop,aRight,aBottom: Integer; aPatch: TBitmap); reintroduce; overload;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Undo: Boolean; override;
    function Caption: string; override;
  published
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Right: Integer read fRight write fRight;
    property Bottom: Integer read fBottom write fBottom;
    property diff: TPngObject read fDiff write fDiff;
  end;

var
  ImageSavingProgress: TImageSavingProgress;  //для одиночных TAsyncSavePNG
  DocumentsSavingProgress: TDocumentsSavingProgress;  //для TRasterImageDocument


implementation
uses SysUtils;
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
  inherited Create(false);
  fDoc:=doc;
  FreeOnTerminate:=true;
  Resume;
end;

procedure TSaveDocThread.Execute;
var INeedAVacation: Boolean;
    i: Integer;
begin
  //сначала предупредим всех, что мы сохраняемся
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do
      Add(fDoc);
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  fDoc.SaveToFile(fDoc.FileName);
  //если мы в списке, значит, пора уходить, а вот если нет, значит народ уже передумал
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do begin
      i:=IndexOf(fDoc);
      INeedAVacation:=(i>=0);
      if INeedAVacation then Delete(i);
    end;
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  if INeedAVacation then fDoc.Free;
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
  fDocumentsSaving.Duplicates:=dupAccept;
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
begin
  try
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
  if Result=nil then
    Result:=TAbstractDocument.LoadComponentFromFile(filename) as TAbstractDocument;
end;


(*
      TPatchImageCommand
                            *)
constructor TPatchImageCommand.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fDiff:=TPngObject.CreateBlank(color_RGB,8,0,0);
end;

destructor TPatchImageCommand.Destroy;
begin
  fDiff.Free;
  inherited Destroy;
end;

constructor TPatchImageCommand.Create(aLeft,aTop,aRight,aBottom: Integer; aPatch: TBitmap);
begin
  Create(nil);
  fLeft:=aLeft;
  fTop:=aTop;
  fRight:=aRight;
  fBottom:=aBottom;

end;

function TPatchImageCommand.Execute: Boolean;
begin

end;

function TPatchImageCommand.Undo: Boolean;
begin

end;

function TPatchImageCommand.Caption: string;
begin

end;

(*
    TRasterImageDocument
                            *)
constructor TRasterImageDocument.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fBtmp:=TAsyncSavePng.CreateBlank(color_RGB,8,0,0);

end;

destructor TRasterImageDocument.Destroy;
begin
  fBtmp.Free;
  inherited Destroy;
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

initialization
  ImageSavingProgress:=TImageSavingProgress.Create;
  DocumentsSavingProgress:=TDocumentsSavingProgress.Create;
finalization
  ImageSavingProgress.Free;
  DocumentsSavingProgress.Free;

end.
