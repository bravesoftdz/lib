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
    destructor Destroy; override; //���������� ������� � ��������� ���������� ���� �������
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
    fPrimaryColor: TColor;
    fSecondaryColor: TColor;
    fBrushSize: Integer;
  public
    Image: TImage;
    constructor Create(aOwner: TComponent); override;
    constructor LoadFromFile(aFilename: string); override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
    procedure SaveAndFree;  //��� ����� ��� ������ � ���������
    procedure SaveToFile(filename: string); override;
  published
    property Btmp: TAsyncSavePNG read fBtmp write fBtmp stored false;
    property PrimaryColor: TColor read fPrimaryColor write fPrimaryColor;
    property SecondaryColor: TColor read fSecondaryColor write fSecondaryColor;
    property BrushSize: Integer read fBrushSize write fBrushSize;
  end;

TSaveDocThread = class (TThread)
  private
    fDoc: TRasterImageDocument;
  protected
    procedure Execute; override;
  public
    constructor Create(doc: TRasterImageDocument);
end;

TRasterImageDocumentCommand = class (THashedCommand)
  public
    function GetDoc: TRasterImageDocument;
  end;

TPatchImageCommand = class (TRasterImageDocumentCommand)
//������� ������� ��� ������ � ���������� �������������
  protected
    fDiff: TPngObject;
    fPredictor: TPngObject;
    fLeft,fTop,fRight,fBottom: Integer; //������������ ��������
    procedure GetBounds; virtual; abstract;//���������������� fRect
    function UndoPrediction: Boolean; virtual;  //false ��������
    //��� �� ������ �� ����� �����������, ������� ��������� �������� ��� ����
    function InternalExecute: Boolean; virtual; abstract; //false ��������,
    //��� ���������� ������� ������ �� �������� � �� ����� ������ �� ������
  public
    constructor Create(aOwner: TComponent); overload; override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Undo: Boolean; override;
  published
    property diff: TPngObject read fDiff write fDiff;
  end;

TRectBrushImageCommand = class (TPatchImageCommand)
//����������� ������������� ������� ������ (�������� � BrushSize) ���. ������
//�������� �������� ����� ��������� �������, ����� ����� ������.
//���� ����� ������� ������ ������ �� ����������, ������� �� �����������
  private
    fX,fY: Integer; //�����. ������
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
  ImageSavingProgress: TImageSavingProgress;  //��� ��������� TAsyncSavePNG
  DocumentsSavingProgress: TDocumentsSavingProgress;  //��� TRasterImageDocument


implementation
uses SysUtils,strUtils,gamma_function;
(*
      TAsyncSavePNG
                          *)
procedure TAsyncSavePNG.SaveToFileAndFree(filename: string);
begin
  TSavePNGThread.Create(self,filename); //����������, ���������� � ��� ������
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
  DocumentsSavingProgress.fAllDocsClearEvent.ResetEvent;
  FreeOnTerminate:=true;
  Priority:=tpLower;
  Resume;
end;

procedure TSaveDocThread.Execute;
var INeedAVacation: Boolean;
    i: Integer;
begin
  //������� ����������� ����, ��� �� �����������
  try
    with DocumentsSavingProgress.fDocumentsSaving.LockList do
      Add(fDoc);
  finally
    DocumentsSavingProgress.fDocumentsSaving.UnlockList;
  end;
  fDoc.CriticalSection.Acquire;
    fDoc.SaveToFile(fDoc.FileName);
  fDoc.CriticalSection.Release;
  //���� �� � ������, ������, ���� �������, � ��� ���� ���, ������ ����� ��� ���������
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
          //���� ������ � ��� ����������� - ���� ��� "�������", ����� �� ������ �����������
          //������, � ������ ����� ����� ����, ����. ����. ������ �� �������� �������� ���.
          //���� �� �� ���������� � �������� ����
          Result:=doc;
          //������ ��� �� ������ "�� ���������� � ��������" - �� ������
          Delete(i);
          break;
        end;
      end;
  finally
    fDocumentsSaving.UnlockList;
  end;
end;


(*
    TRasterImageDocument
                            *)
constructor TRasterImageDocument.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fBtmp:=TAsyncSavePng.CreateBlank(color_RGB,8,0,0);

  BrushSize:=10;
  PrimaryColor:=clWhite;
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

procedure TRasterImageDocument.SaveToFile(filename: string);
var s: string;
begin
  if Changed then
    inherited SaveToFile(filename);
  s:=ExtractFilePath(filename);
  s:=LeftStr(s,Length(s)-5);
  If Changed or not FileExists(filename) then
    btmp.SaveToFile(s+ChangeFileExt(ExtractFileName(filename),'.png'));
end;

constructor TRasterImageDocument.LoadFromFile(aFilename: string);
var s: string;
begin
  inherited LoadFromFile(aFilename);
  s:=ExtractFilePath(aFilename);
  s:=LeftStr(s,Length(s)-5);
  btmp.LoadFromFile(s+ChangeFileExt(ExtractFileName(aFilename),'.png'));
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
  fDiff:=TPngObject.CreateBlank(color_RGB,8,0,0); //����� ����������� � ����
  fDiff.Filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  fDiff.CompressionLevel:=9;
  fPredictor:=TPngObject.CreateBlank(color_RGB,8,0,0);  //�������� ��������
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
  //���������� ��������
  fDiff.Resize(fRight-fLeft,fBottom-fTop);
  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),GetDoc.Btmp.Canvas,
    Rect(fLeft,fTop,fRight,fBottom));
  //������ � fDiff ����� ���� ���������, ������� ������ ������
  Result:=InternalExecute;
  if Result then begin
    fPredictor.Resize(fDiff.Width,fDiff.Height);
    if UndoPrediction then
      //�������� �� Predictor ���. �������� ������ �� ����, ��� ��
      //����� ����� �� ��������� ����������
      //������ "���������" ����� - ������� ���� �������� �� ������
      //����� ��������� �������� ������
      for j:=0 to fDiff.Height-1 do
        for i:=0 to fDiff.Width-1 do begin
          C1.Color:=fDiff.Pixels[i,j];
          C2.Color:=fPredictor.Pixels[i,j];
          C1.R:=128+C1.R-C2.R;
          C1.G:=128+C1.G-C2.G;
          C1.B:=128+C1.B-C2.B;
          fDiff.Pixels[i,j]:=C1.Color;
        end;
    //� �� ��� � ���� ���
  end;
  //� ��������� ������ ������� ���-��� ����� �������, ��� ����� �����������
  //������ ������� ��������
end;

function TPatchImageCommand.Undo: Boolean;
var i,j: Integer;
    C1,C2: RGBColor;
begin
  getBounds;  //������� �������� �� ����� �� �� ����������� ������, �� ������������
  //��� ����� �������� ����!
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
    //� �� ��� � ���� ���
  //�������� �������� �������� �� �����
  GetDoc.Btmp.Canvas.CopyRect(Rect(fLeft,fTop,fRight,fBottom),fDiff.Canvas,Rect(0,0,fDiff.Width,fDiff.Height));
  Result:=true;
end;

function TPatchImageCommand.UndoPrediction: Boolean;
begin
  Result:=false;
end;

(*
    TRectBrushImageCommand
                              *)
constructor TRectBrushImageCommand.Create(aX,aY: Integer);
begin
  Create(nil);
  X:=aX;
  Y:=aY;
end;

procedure TRectBrushImageCommand.GetBounds;
var size: Integer;
begin
  size:=GetDoc.BrushSize;
  fLeft:=X-size;
  fRight:=X+size;
  fTop:=Y-size;
  fBottom:=Y+size;
end;

function TRectBrushImageCommand.InternalExecute: Boolean;
begin
  //���� �������� ���������
  with GetDoc.Btmp.Canvas do begin
    Brush.Color:=GetDoc.PrimaryColor;
    FillRect(Rect(fLeft,fTop,fRight,fBottom));
  end;
  Result:=true;
end;


initialization
  RegisterClasses([TRasterImageDocument]);
  ImageSavingProgress:=TImageSavingProgress.Create;
  DocumentsSavingProgress:=TDocumentsSavingProgress.Create;
finalization
  ImageSavingProgress.Free;
  DocumentsSavingProgress.Free;

end.
