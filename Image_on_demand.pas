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

TRasterImageDocument = class (TDocumentWithImage)
  private
    fBtmp: TAsyncSavePNG;
  public
    Image: TImage;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
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
  ImageSavingProgress: TImageSavingProgress;

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
    TImageSavingProgress
                              *)
constructor TImageSavingProgress.Create;
begin
  inherited Create;
  fCriticalSection:=TCriticalSection.Create;
  fAllClearEvent:=TEvent.Create(nil,false,false,'ImageOnDemandAllClearEvent');
  fAllClearEvent.SetEvent;
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

initialization
  ImageSavingProgress:=TImageSavingProgress.Create;
finalization
  ImageSavingProgress.Free;

end.
