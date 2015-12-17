unit RasterImageCommands;

interface

uses classes,command_class_lib,Image_on_demand,pngImageAdvanced,graphics;

type

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

TBrushCommand = class (TPatchImageCommand)
  private
    fFutureOwner: TRasterImageDocument;
    fBrushColor: TColor;
    fBrushSize: Word;
    fBrushShape: TBrushShape;
    fXPoints,fYPoints: array of Word;
    fColorExistsInPalette: Boolean; //���� �� ������� ������ ��� �����, �������
    //�����������
    fNeedToChangeBitDepth: Boolean; //� ������� ������ ��� �������, �������� ���/������� ��������
    fNewBitDepth: Byte;
    fNewColorMode: Byte;
    fColorIndex: Cardinal;  //�� 1 ���� �� 24 ���, �� ���������������
    procedure WritePoints(stream: TStream);
    procedure ReadPoints(stream: TStream);
  protected
    procedure DefineProperties(filer: TFiler); override;
  public
    constructor CreateStandalone(myFutureOwner: TRasterImageDocument);
    //��������� �� ������ �������� � ���������� (������ ��� ������) ��� ��
    //���������� DispatchCommand, ������� ��� ���������������
    procedure Draw(aX,aY: Word);  //���� ����� �����
    //���� ���� ���-�� ��������, ������� � fXPoints/fYPoints
    

  end;
implementation

uses pngImage,gamma_function,sysUtils;

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

(*
    TBrushCommand
                    *)
function BrushColorIsMonochrome(color: TColor; out index: Cardinal): boolean;
var rgbcol: RGBColor absolute color;
begin
  Result:=(rgbcol.R=rgbcol.G) and (rgbcol.G=rgbcol.B);
  if Result then index:=rgbcol.R;
end;

function BrushColorInMonochromePalette(color: TColor;aBitDepth: Byte; out index: Cardinal): boolean;
var rgbcol: RGBColor absolute color;
    maxColorVal: Word;
begin
  maxColorVal:=(1 shl aBitDepth)-1;
  Result:= BrushColorIsMonochrome(color,index) and
    (((index * maxColorVal div 255) * 255) div maxColorVal = index);
  if Result then index:=(index*maxColorVal) div 255; //� �������
end;

constructor TBrushCommand.CreateStandalone(myFutureOwner: TRasterImageDocument);
var btmp: TExtendedPNGObject;
  fActualCount: Integer;
  p: Integer;
begin
//��������� �����: ���� ���� ����������� �������� ��� ������������������ ��������,
//���� �� �� ���������
//�� ���� ��� ����� �������� ���������� ��� �� �������, �� �� ���������, � �����
//������ ��������, � �������.
  inherited Create(nil);
  fFutureOwner:=myFutureOwner;
  fBrushColor:=fFutureOwner.PrimaryColor;
  fBrushSize:=fFutureOwner.BrushSize;
  fBrushShape:=fFutureOwner.BrushShape;
  btmp:=fFutureOwner.Btmp;
  case Btmp.Header.ColorType of
    COLOR_RGB: begin
      fColorExistsInPalette:=true;
      fColorIndex:=TColorToRGBTriple(fBrushColor);
      //������ ������ �� ����!
      end;
    COLOR_GRAYSCALE: begin
      fColorExistsInPalette:=
        BrushColorInMonochromePalette(fBrushColor,Btmp.Header.BitDepth,fColorIndex);
      if not fColorExistsInPalette then begin
        //����� �� ������� �������� ����/�������
        fNeedToChangeBitDepth:=not Btmp.IsColorNumberLessThen(1 shl Btmp.Header.BitDepth, fActualCount);
        if fNeedToChangeBitDepth then begin
          fNewBitDepth:=8;
          if BrushColorIsMonochrome(fBrushColor,fColorIndex) then
            fNewColorMode:=COLOR_GRAYSCALE
          else
            fNewColorMode:=COLOR_RGB;
        end
        else begin
          fNewColorMode:=COLOR_PALETTE;
          fColorIndex:=fActualCount;
        end;
      end;
    end;
    COLOR_PALETTE: begin
      //����� ����������, ���� �� ��� ���� � �������
      p:=btmp.GetColorIndex(fBrushColor);
      fColorExistsInPalette:=(p>=0);
      if not fColorExistsInPalette then begin
        //��� ���� ��������
        fNeedToChangeBitDepth:=not Btmp.IsColorNumberLessThen(1 shl Btmp.Header.BitDepth, fActualCount);
        if fNeedToChangeBitDepth then begin
          fNewBitDepth:=8;
          if fActualCount>=256 then
            fNewColorMode:=COLOR_RGB
          else
            fNewColorMode:=COLOR_PALETTE;
        end
        else begin
          fNewColorMode:=COLOR_PALETTE;
          fColorIndex:=fActualCount;
        end;
      end;
    end;
    else
      Raise Exception.Create('sorry, alpha channel support under construction');
  end;
end;

procedure TBrushCommand.Draw(aX,aY: Word);
begin
  //���������, ������ �� ��� ���� �����-�� �������?

end;

procedure TBrushCommand.DefineProperties(filer: TFiler);
begin
  filer.DefineBinaryProperty('points',ReadPoints,WritePoints,true);
end;

procedure TBrushCommand.WritePoints(stream: TStream);
begin

end;

procedure TBrushCommand.ReadPoints(stream: TStream);
begin

end;

initialization
  RegisterClasses([TRectBrushCommand]);

end.
