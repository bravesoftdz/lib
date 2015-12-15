unit Image_on_demand;

interface

uses classes,streaming_class_lib, syncObjs, pngImage, command_class_lib,graphics,
  IGraphicObject_commands, ExtCtrls, Types, pngimageAdvanced;

type

TLogProc = procedure (text: string);

TGetImageProc = function: TExtendedPngObject of object;

IGetPngThread = interface
['{3726C791-0100-44DA-8D5A-E900A4EB6A62}']
  function GetImage: TExtendedPngObject;
end;

TAbstractGetImageThread = class (TThread, IGetPngThread)
  private
    fEvent: TEvent;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetImage: TExtendedPngObject;
end;

TLoadBitmapThread = class (TThread) //����� �������� �� �������� �������!
  private
    fFilename: string;
    fBitmap: TExtendedPngObject;
    fLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(aOnTerminate: TNotifyEvent;filename: string='');
    destructor Destroy; override;
    function GetBitmap: TExtendedPngObject;
end;
TScalingThread = class (TThread)
  private
    fGetImageProc: TGetImageProc; //������ �� ������������ ��������
    fScale: Real;
    fbitmap: TExtendedPngObject;
    fLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(aGetImageProc: TGetImageProc; ascale: Real);
    destructor Destroy; override;
    function GetScaled: TExtendedPngObject;
end;
TRasterImageDocument = class (TDocumentWithImage, IConstantComponentName)
  private
    fScaleNumber: Integer;
    fPrimaryColor: TColor;
    fSecondaryColor: TColor;
    fBrushSize: Integer;
    fLoadThread: TLoadBitmapThread;
    fScalingThreads: array of TScalingThread;
    procedure LoadThreadTerminate(Sender: TObject);
    function GetBtmp: TExtendedPngObject;
    function GetRealScale: real;
  public
    PercentDone: Integer; //������������ �� � �����
    Image: TImage;
    ChangeRect: TRect;  //0,0,0,0 ���. ������ �������
    onSavingProgress: TSavingProgressReport;
    procedure AddToChangeRect(const A: TRect);
    constructor Create(aOwner: TComponent); override;
    constructor CreateFromImageFile(aFileName: string);
    constructor LoadFromFile(const aFilename: string); override;
    destructor Destroy; override;
    function Get_Image: TImage; override;
    function Get_Scaled_Btmp: TExtendedPNGObject;
    procedure SaveAndFree;  //��� ����� ��� ������ � ���������
    procedure FreeWithoutSaving;
    procedure SaveToFile(const filename: string); override;
    procedure Change; override;
  published
    property Btmp: TExtendedPNGObject read GetBtmp stored false;
    property Scale: Real read GetRealScale stored false;
    property PrimaryColor: TColor read fPrimaryColor write fPrimaryColor;
    property SecondaryColor: TColor read fSecondaryColor write fSecondaryColor;
    property BrushSize: Integer read fBrushSize write fBrushSize;
    property ScaleNumber: integer read fScaleNumber write fScaleNumber;
  end;
TDocumentsSavingProgress = class (TObject)
  private
    fAllDocsClearEvent: TEvent; //������������ � ������� - �������� �������
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
    procedure PrefetchDocument(doc: TRasterImageDocument); //����� �������� �� ������ ������
    function Count: Integer;  //debug
    function AsText: string; //debug
  end;
TSaveDocThread = class (TThread)
  private
    fPercentDone: Integer;  //� ��� ��������� ��� GetProgress
                            //������ �������� Terminated
    procedure CallProgress;
    function GetProgressFromThread(Sender: TObject; PercentDone: Integer): boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;  //���������� �� Thread'a � ������ �������� �����
    //�� ������, ����� ���� ����� ���������
  public
    fDoc: TRasterImageDocument;
    constructor Create(doc: TRasterImageDocument);
end;
TRasterImageDocumentCommand = class (TAbstractTreeCommand)
  public
    function GetDoc: TRasterImageDocument;
  end;
TPatchImageCommand = class (TRasterImageDocumentCommand)
//������� ������� ��� ������ � ���������� �������������
  protected
    fDiff: TExtendedPngObject;
    fPredictor: TExtendedPngObject;
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
    property diff: TExtendedPngObject read fDiff write fDiff;
  end;
TRectBrushCommand = class (TPatchImageCommand)
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
  DocumentsSavingProgress: TDocumentsSavingProgress;  //��� TRasterImageDocument

implementation
uses SysUtils,strUtils,gamma_function,math,typinfo,forms;

(*
    TSaveDocThread
                      *)
constructor TSaveDocThread.Create(doc: TRasterImageDocument);
begin
  inherited Create(true);
  fDoc:=doc;
  DocumentsSavingProgress.fAllDocsClearEvent.ResetEvent;
  Assert(Assigned(fDoc));
  //������� ����������� ����, ��� �� �����������
  DocumentsSavingProgress.fDocumentsSaving.Add(fDoc);
  onTerminate:=documentsSavingProgress.ThreadTerminate;
  FreeOnTerminate:=true;
  Priority:=tpLower;
  Resume;
end;

function TSaveDocThread.GetProgressFromThread(Sender: TObject; PercentDone: Integer): Boolean;
begin
//������������ ����� ����� �������� - �������������� ������ synchronize
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
//    fDoc.SaveToFile(fDoc.FileName); //������ �������� ������ (���� ������������ � ��)
  finally
    fDoc.CriticalSection.Release;
  end;
  //��� ������������� ����. �������� �������� ������� ������ � ������ - ������������
  //������ ���� ������������� ����� ���������� (retry)
  //���� �������� ����������

  //���� �� � ������, ������, ���� �������, � ��� ���� ���, ������ ����� ��� ���������
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
  //����� onTerminate
  //���� ���. ����� ������� �� AllDocsClearEvent.WaitFor, �� �� ��������� ����� ��
  //���� ��������� �� ������� ������� �� ������� ��� �������
  //������, ��� onTerminate ��� ���� �� ����� ����������, ��������� ��� ��������
  //����������� �����.
  //�� ��� ���� ������, ���� FastMM4 ����������.
end;

(*
    TLoadBitmapThread
                              *)
constructor TLoadBitmapThread.Create(aOnTerminate: TNotifyEvent; filename: string='');
begin
  inherited Create(true);
  fLock:=TCriticalSection.Create;
  OnTerminate:=aOnTerminate;
  fFileName:=filename;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  fBitmap:=TExtendedPngObject.Create;
  Resume;
end;

destructor TLoadBitmapThread.Destroy;
begin
  Terminate;  //������ �� �������� ��������, �� ���-������ ��������
  WaitFor;
  fLock.Free;
  fBitmap.Free;
  inherited Destroy;
end;

procedure TLoadBitmapThread.Execute;
var ext: string;
    pic: TPicture;
begin
  if fFileName='' then Exit;
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

function TLoadBitmapThread.GetBitmap: TExtendedPNGObject;
begin
  fLock.Acquire;
  try
    WaitFor;
    if FatalException=nil then
      if Terminated then Result:=nil else Result:=fBitmap
    else
      Result:=nil;    
//      Raise Exception.Create(Exception(FatalException).Message);
  finally
    fLock.Leave;
  end;
end;

(*
    TScalingThread
                      *)
constructor TScalingThread.Create(aGetImageProc: TGetImageProc; aScale: Real);
begin
  inherited Create(true);
  fLock:=TCriticalSection.Create;
  fGetImageProc:=aGetImageProc;
  fScale:=aScale;
  //priority:=tpNormal;
  FreeOnTerminate:=false;
  Resume;
end;

destructor TScalingThread.Destroy;
begin
  Terminate;
  WaitFor;
  fLock.Free; //���� �� ����������� - �� �����������
  fBitmap.Free;
  inherited Destroy;
end;

procedure TScalingThread.Execute;
var img: TExtendedPNGObject;
begin
  //�����������, �������� �������������� ������������� ���������������
  img:=fGetImageProc(); //�������� ��������, ����� ��� �����, �������, ��������
  if Assigned(img) and not Terminated then begin
    img.Synchronizer.BeginRead; //�� ��� ���������� img ��� �����
                                //�� ���, ���� �� ��� ��������� ����� ����� �����
                                //��������!?
    try
      fBitmap:=img.Get2TimesDownScaled;
    finally
      img.Synchronizer.EndRead;
    end;
  end;
end;

function TScalingThread.GetScaled: TExtendedPngObject;
begin
  fLock.Acquire;
  try
    Waitfor;
    if FatalException=nil then
      if not Terminated then
        Result:=fBitmap
      else
        Result:=nil
    else
      Result:=nil;
//      Raise exception.Create(Exception(FatalException).Message);
  finally
    fLock.Leave;
  end;
end;

(*
    TDocumentsSavingProgress
                                *)
constructor TDocumentsSavingProgress.Create;
begin
  fDocumentsSaving:=TThreadList.Create;
  fDocumentsSaving.Duplicates:=dupIgnore;
//  fDocumentsSaving.Duplicates:=dupError;
  fAllDocsClearEvent:=TEvent.Create(nil,false,true,''); //�� ����� �����, �����
  //������������ ���������� 2 ����� �� "���������"
end;

procedure TDocumentsSavingProgress.ThreadTerminate(Sender: TObject);
begin
  if Assigned(onSaveThreadTerminate) then
    onSaveThreadTerminate(Sender);
end;

procedure TDocumentsSavingProgress.WaitForAllDocsClearEvent;
var i: Integer;
label tryagain;
begin
  i:=0;
  tryagain:
  CheckSynchronize(500);
  inc(i);
  if i=120 then raise Exception.Create('WaitForAllDocsClearEvent timeout');
  case fAllDocsClearEvent.WaitFor(500) of
    wrTimeout: goto tryagain;
    wrError: raise Exception.Create('WaitForAllDocsClearEvent error');
    wrAbandoned: raise Exception.Create('WaitForAllDocsClearEvent abandoned');
  end;
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

procedure TDocumentsSavingProgress.PrefetchDocument(doc: TRasterImageDocument);
begin
//  FreeAndNil(fPrefetchThread);  //���� ��� ���������� ������ �������� - �� ����������
  if Assigned(fPrefetchedDoc) then begin
    //debug
    log('releasing prefetched '+ExtractFileName(fPrefetchedDoc.FileName)+'; addr='+IntToHex(Integer(fPrefetchedDoc),8));
    fPrefetchedDoc.SaveAndFree;
  end;
//  fPrefetchedDoc.SaveAndFree; //���������� ������������, � �.� �� ������ ���� ������
  fPrefetchedDoc:=doc;
  if Assigned(doc) then begin
    log('prefetched doc: '+ExtractFileName(fPrefetchedDoc.FileName)+'; addr='+IntToHex(Integer(fPrefetchedDoc),8));
  //�������� ����. �����, � ��� �������� �� ����� ������� �������
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
end;

constructor TRasterImageDocument.CreateFromImageFile(aFileName: string);
begin
  Create(nil);
  fLoadThread.Free;
  fLoadThread:=TLoadBitmapThread.Create(LoadThreadTerminate,aFileName);
end;

constructor TRasterImageDocument.LoadFromFile(const aFilename: string);
var s: string;
begin
  inherited LoadFromFile(aFilename);
  s:=ExtractFilePath(aFilename);
  s:=LeftStr(s,Length(s)-5);  //�������� \DLRN
  fLoadThread.Free;
  fLoadThread:=TLoadBitmapThread.Create(LoadThreadTerminate,s+ChangeFileExt(ExtractFileName(aFilename),'.png'));
end;

destructor TRasterImageDocument.Destroy;
var i: Integer;
begin
  fLoadThread.Free;
  for i:=0 to Length(fScalingThreads)-1 do
    fScalingThreads[i].free;
  inherited Destroy;
end;

function TRasterImageDocument.Get_Image: TImage;
begin
  Result:=Image;
end;

function TRasterImageDocument.GetRealScale: Real;
begin
  assert((scaleNumber>=0) and (scaleNumber<=Length(fScalingThreads)));
  if scaleNumber=0 then Result:=1
  else Result:=fScalingThreads[scaleNumber-1].fScale;
end;

function TRasterImageDocument.Get_Scaled_Btmp: TExtendedPngObject;
begin
  assert((scaleNumber>=0) and (scaleNumber<=Length(fScalingThreads)));
  if scaleNumber=0 then Result:=fLoadThread.GetBitmap
  else Result:=fScalingThreads[scaleNumber-1].GetScaled;
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
  Result:=fLoadThread.GetBitmap;
end;

procedure TRasterImageDocument.LoadThreadTerminate(Sender: TObject);
var t: string;
    i,c: Integer;
    s: Real;
begin
  if Assigned(fLoadThread.FatalException) then begin
    t:=Format('�� ������� ��������� ����������� %s: %s',
      [fLoadThread.fFilename,Exception(fLoadThread.FatalException).Message]);
    Application.MessageBox(@t[1],'AMBIC');
    Exit;
  end;
  //�����, ������������� �����. ���� ����� ����� 2
  if (fLoadThread.fBitmap.Width=0) or (fLoadThread.fBitmap.Height=0) then Exit;

  c:=Floor(log2(min(fLoadThread.fBitmap.Width,fLoadThread.fBitmap.Height)));
  SetLength(fScalingThreads,c);
  s:=0.5;
  fScalingThreads[0].Free;
  fScalingThreads[0]:=TScalingThread.Create(fLoadThread.GetBitmap,s);
  for i:=1 to c-1 do begin
    s:=s/2;
    fScalingThreads[i].Free;
    fScalingThreads[i]:=TScalingThread.Create(fScalingThreads[i-1].GetScaled,s);
  end;
  
end;

procedure TRasterImageDocument.AddToChangeRect(const A: TRect);
begin
  if IsRectEmpty(ChangeRect) then ChangeRect:=A
  else if not IsRectEmpty(A) then begin
    ChangeRect.Left:=min(ChangeRect.Left,A.Left);
    ChangeRect.Right:=max(ChangeRect.Right,A.Right);
    ChangeRect.Top:=min(ChangeRect.Top,A.Top);
    ChangeRect.Bottom:=max(ChangeRect.Bottom,A.Bottom);
  end;
end;

procedure TRasterImageDocument.Change;
begin
  inherited Change;
  //��������� ���������, ������ ��� ������ ��������� ChangeRect ������ ���������
  //����� �����������!
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
  fDiff:=TExtendedPngObject.CreateBlank(color_RGB,8,0,0); //����� ����������� � ����
  fDiff.Filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  fDiff.CompressionLevel:=9;
  fPredictor:=TExtendedPngObject.CreateBlank(color_RGB,8,0,0);  //�������� ��������
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
  doc: TRasterImageDocument;
begin
  getBounds;
  doc:=GetDoc;
  doc.AddToChangeRect(Rect(fLeft,fTop,fRight,fBottom));
  //���������� ��������
  fDiff.Resize(fRight-fLeft,fBottom-fTop);
  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),Doc.Btmp.Canvas,
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
    doc: TRasterImageDocument;
begin
  getBounds;  //������� �������� �� ����� �� �� ����������� ������, �� ������������
  //��� ����� �������� ����!
  doc:=GetDoc;
  doc.AddToChangeRect(Rect(fLeft,fTop,fRight,fBottom));
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
  doc.Btmp.Canvas.CopyRect(Rect(fLeft,fTop,fRight,fBottom),fDiff.Canvas,Rect(0,0,fDiff.Width,fDiff.Height));
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
  //���� �������� ���������
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
  //����� ������, ������, ��� ������ � �� ���������
  Result:=false;
end;

initialization
  RegisterClasses([TRasterImageDocument,TRectBrushCommand]);
  DocumentsSavingProgress:=TDocumentsSavingProgress.Create;
finalization
  DocumentsSavingProgress.Free;
end.
