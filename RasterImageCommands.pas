unit RasterImageCommands;

interface

uses classes,command_class_lib,Image_on_demand,pngImageAdvanced,graphics,types;

type

TRasterImageDocumentCommand = class (TAbstractTreeCommand)
  public
    function GetDoc: TRasterImageDocument;
  end;
TPatchImageCommand = class (TRasterImageDocumentCommand)
//������� ������� ��� ������ � ���������� �������������
//GetBounds ���������� ��������������� ������� ��������������, ��� ���������� ��������
//� ��� ������ Execute �������� �� ���� ����� ��������� � fDiff, � ������ internalExecute
//� �������� ��������� internalExecute - ��� �������� �����������
//�� ����� � ����� ������� �������� ������ ����� �����������.
//��� ���� ��� ������ ���������� ����� ������ �������. �������� PointChanged, ��� �����
//������� ���������� �����. �������������, ������� ����� ��� undo.
//�����, Top/Left ��������� ������������� ��������.
//�������, ���������� UndoPrediction, ������� ��������, ���������� ������ ��������������
//��������� � ���������� �������, ������� ����������� � ���� (�� ������ fDiff), ������������
//(�����������) �������� �����������. ���� ��� �����������, �� �� fDiff ����� ������ fPredictor.
//UndoPrediction ����� ����� ������ ColorMode � BitDepth �� fDiff.

//��� ���������� Undo ������ ����� ����������� UndoPrediction, ������� ���� ����� �� ��
//������������� �����������, ��� � ������. ��� �� ���������� � ������������ fDiff.
//���� ������ ����������� � ��������� ��������� � fDiff �� �������� ��� ��� �������,
//�� �� �������� ��� � ������� �������, �� �����������, ��������, ���������� �������
//�������, �� "���������" �� ����� fDiff.
  protected
    fDiff: TExtendedPngObject;
    fPredictor: TExtendedPngObject;
    fRect,fUpdateRect: TRect; //������������ �������� � ���������� ���. �����.
    fImageFormatChanged: Boolean;
    fOldBitDepth,fOldColorType: Byte;
    procedure GetBounds; virtual; abstract;//���������������� fLeft,fTop,fRight,fBottom
    procedure PointChanged(const x,y: Integer); //���������� �� �������,
    // ���� � (x,y) ��������� ���������, ����� ��� ���������� ������������ �������.
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
    property diff: TExtendedPngObject read fDiff write fDiff stored fActiveBranch;
    property Left: Integer read fRect.Left write fRect.Left;
    property Top: Integer read fRect.Top write fRect.Top;
    property BitDepth: Byte read fOldBitDepth write fOldBitDepth stored fImageFormatChanged;
    property ColorType: Byte read fOldColorType write fOldColorType stored fImageFormatChanged;
  end;

TWordPoint=record
  case Integer of
    0: (X,Y: Word);
    1: (AsPointer: Pointer);
end;

TPixelBelongsToBrushFunc = function (x,y: Integer): Boolean of object;

TBrushCommand = class (TPatchImageCommand)
  private
    fR2: Integer;
    fBrushColor: TColor;
    fBrushSize: Word;
    fBrushShape: TBrushShape;
    fBelongFunc: TPixelBelongsToBrushFunc;
    fPoints: TList;
    fColorExistsInPalette: Boolean; //���� �� ������� ������ ��� �����, �������
    //�����������
    fNeedToChangeBitDepth: Boolean; //� ������� ������ ��� �������, �������� ���/������� ��������
    fNewBitDepth: Byte;
    fNewColorMode: Byte;
    fColorIndex: Cardinal;  //�� 1 ���� �� 24 ���, �� ���������������
//    fRect: TRect;
    fPointsReduced: Boolean;  //�������, ��� �� ����� �������� ����� ����� ��-�����
    procedure ReadPointsAsText(reader: TReader);
    procedure WritePointsAsText(writer: TWriter);
    procedure WritePoints(stream: TStream);
    procedure ReadPoints(stream: TStream);
    procedure ReducePoints;
    procedure FigureAboutColorsAndBitdepth;
    procedure SetBrushSize(value: Word);
    procedure SetBrushShape(value: TBrushShape);
    function PixelBelongsToRectBrush(x,y: Integer): Boolean;
    function PixelBelongsToEvenRoundBrush(x,y: Integer): Boolean;
    function PixelBelongsToOddRoundBrush(x,y: Integer): Boolean;
  protected
    procedure DefineProperties(filer: TFiler); override;
    procedure GetBounds; override;  //��������� fRect
    function UndoPrediction: Boolean; override;
    //������ ������� ������� ����. �������.
    //��� ����� �� ���� ��������� ��, ��� �� ����������
  public
    constructor Create(aOwner: TComponent); override;
    //��������� �� ������ �������� � ���������� (������ ��� ������) ��� ��
    //���������� DispatchCommand, ������� ��� ���������������
    destructor Destroy; override; //TList ���� ����������
    function Draw(aX,aY: Word): boolean;  //���� ����� �����
    //���� ���� ���-�� ��������, ������� � fXPoints/fYPoints
    function InternalExecute: Boolean; override;
  published
    property BrushColor: TColor read fBrushColor write fBrushColor;
    property BrushSize: Word read fBrushSize write SetBrushSize;
    property BrushShape: TBrushShape read fBrushShape write SetBrushShape;
  end;
implementation

uses pngImage,gamma_function,sysUtils,simple_parser_lib,math;

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
  fDiff:=TExtendedPngObject.Create; //����� ����������� � ����
end;

destructor TPatchImageCommand.Destroy;
begin
  fDiff.Free;
  inherited Destroy;
end;

procedure TPatchImageCommand.PointChanged(const X,Y: Integer);
begin
  fUpdateRect.Left:=min(fUpdateRect.Left,X);
  fUpdateRect.Right:=max(fUpdateRect.Right,X+1);
  fUpdateRect.Top:=min(fUpdateRect.Top,Y);
  fUpdateRect.Bottom:=max(fUpdateRect.Bottom,Y+1);
end;

function TPatchImageCommand.Execute: Boolean;
var doc: TRasterImageDocument;
  btmp,tmp: TExtendedPNGObject;
  src,dest: TPngObjectIterator;
begin
  getBounds;  //�������������� fRect
  doc:=GetDoc;
  btmp:=doc.Btmp;
  fOldBitDepth:=btmp.Header.BitDepth;
  fOldColorType:=btmp.Header.ColorType;
  //���������� ��������
  fDiff.Free;
  fDiff:=TExtendedPNGObject.CreateBlank(Btmp.Header.ColorType,
    Btmp.Header.BitDepth,fRect.Right-fRect.Left,fRect.Bottom-fRect.Top);

//  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),Doc.Btmp.Canvas,
//    Rect(fLeft,fTop,fRight,fBottom));
  //copyRect ����� �������, �� �� thread-safe (� �� ����� ���� thread-safe, �� ���� �����)
  //� ��� ��� �����, ��� �� ��� ������, ���� stretchBlt
  //��� ��������:
  src:=btmp.CreateIteratorForCropped(fRect);
  dest:=fDiff.CreateIterator;
  assert(fRect.Right-fRect.Left=fDiff.width,'execute patch command: width not equal:'+IntToStr(fRect.Right-fRect.Left)+' and '+IntToStr(fDiff.Width));
  assert(fRect.Bottom-fRect.Top=fDiff.height,'execute patch command: height not equal'+IntToStr(fRect.Bottom-fRect.Top)+' and '+IntToStr(fDiff.Height));
  while not src.isEOF do
    dest.WriteNextPixel(src.ReadNextPixel);
  dest.Free;
  src.Free;
  //����� ��� ������ �����-������ �������� �����, ���� ��� �������
  //� fUpdateRect �������� ���������� -Inf � +Inf
  fUpdateRect.Left:=fRect.Right;
  fUpdateRect.Right:=fRect.Left-1;
  fUpdateRect.Top:=fRect.Bottom;
  fUpdateRect.Bottom:=fRect.Top-1;
  //������ � fDiff ����� ���� ���������, ������� ������ ������
  Result:=InternalExecute and not IsRectEmpty(fUpdateRect);
  if Result then begin
  //����� InternalExecute ��� ����� ���������� �������
    if (fRect.Left<>fUpdateRect.Left) or (fRect.Right<>fUpdateRect.Right)
      or (fRect.Top<>fUpdateRect.Top) or (fRect.Bottom<>fUpdateRect.Bottom) then
    begin
      tmp:=fDiff;
      fDiff:=TExtendedPNGObject.CreateBlank(tmp.Header.ColorType,
        tmp.Header.BitDepth,fUpdateRect.Right-fUpdateRect.Left,fUpdateRect.Bottom-fUpdateRect.Top);
      src:=tmp.CreateIteratorForCropped(Rect(fUpdateRect.Left-fRect.Left,fUpdateRect.Top-fRect.Top,
        fUpdateRect.Right-fRect.Left,fUpdateRect.Bottom-fRect.Top));
      dest:=fDiff.CreateIterator;
      while not src.isEOF do
        dest.WriteNextPixel(src.ReadNextPixel);
      dest.Free;
      src.Free;
      tmp.Free;
      fRect:=fUpdateRect;
    end;
    //fPredictor ������ ����� �� �� ��������� ColorType,BitDepth, Width, Height
    fPredictor:=TExtendedPngObject.CreateBlank(fDiff.Header.ColorType,
      fDiff.Header.BitDepth,fDiff.Width,fDiff.Height);
    if UndoPrediction then begin
      //�������� �� Predictor ���. �������� ������ �� ����, ��� ��
      //����� ����� �� ��������� ����������
      //������ "���������" ����� - ������� ���� �������� �� ������
      //RGB24 - ���� �������� �� ������� �����
      //Grayscale - �������� �������
      //Palette - �������� �������, ������������ ������ ���������� UndoPrediction
      src:=fPredictor.CreateIterator;
      dest:=fDiff.CreateIterator;
      //that's how I sub.
      while not src.isEOF do
        dest.WriteNextSubpixel((dest.PeekNextSubpixel-src.ReadNextSubpixel) and 255);
      //�� ��������, �� �� ��� "����� ����������" ����� ������� � ����� ���������, �����
      //�� �� ���� �� ����������� ����. ������ �����.
      //������� �������� �������� BitDepth � ColorType
      dest.Free;
      src.Free;
    end;
    fPredictor.Free;
    //� �� ��� � ���� ���
    //�������, ������� ���������, ����� ������� �� ��������
    doc.AddToChangeRect(fRect);
  end;
  //� ��������� ������ ������� ���-��� ����� �������, ��� ����� �����������
  //������ ������� ��������
end;

function TPatchImageCommand.Undo: Boolean;
var doc: TRasterImageDocument;
    src,dest: TPngObjectIterator;
    tmp: TExtendedPngObject;
begin
  //��������� �������������, �� ������� ��������� ���������. ��� ������� ���� �������� � Top/Left
  //�� �� ���� �� �����, � �� �������� �������� �����. ������ ������ ����
  fRect.Bottom:=fRect.Top+fDiff.Height;
  fRect.Right:=fRect.Left+fDiff.Width;
  doc:=GetDoc;
  doc.AddToChangeRect(fRect); //����� ����� ��������, �� ��� ��� �����.
  if ColorType=0 then ColorType:=doc.Btmp.Header.ColorType;//���� ������ �� ���������,
  if BitDepth=0 then BitDepth:=doc.Btmp.Header.BitDepth;//�� ��� � �� ���������!
  //��������, ������ fDiff ����� ���������� ����� ����������, ��� ���������� �������
  if (fDiff.Header.BitDepth<>BitDepth) or (fDiff.Header.ColorType<>ColorType) then begin
    tmp:=fDiff.GetInOtherFormat(ColorType,BitDepth);
    fDiff.Free;
    fDiff:=tmp;
  end;
  fPredictor:=TExtendedPngObject.CreateBlank(ColorType,BitDepth,fDiff.Width,fDiff.Height);
  if UndoPrediction then begin
    //� ������� ��� �������� fPredictor, ������ �������� �����.
    src:=fPredictor.CreateIterator;
    dest:=fDiff.CreateIterator;
    //that's how I add.
    while not src.isEOF do
      dest.WriteNextSubpixel((dest.PeekNextSubpixel+src.ReadNextSubpixel) and 255);
    dest.Free;
    src.Free;
  end;
  fPredictor.Free;
  //� �� ��� � ���� ���
  //�������� �������� �������� �� �����
  if (doc.Btmp.Header.ColorType<>ColorType) or (doc.Btmp.Header.BitDepth<>BitDepth) then begin
    tmp:=doc.Btmp.GetInOtherFormat(ColorType,BitDepth);
    doc.Btmp.Free;
    doc.Btmp:=tmp;
  end;

  src:=fDiff.CreateIterator;
  dest:=doc.Btmp.CreateIteratorForCropped(fRect);
  assert(fRect.Right-fRect.Left=fDiff.width,'undo patch: width not equal:'+IntToStr(fRect.Right-fRect.Left)+' and '+IntToStr(fDiff.Width));
  assert(fRect.Bottom-fRect.Top=fDiff.height,'undo patch: height not equal'+IntToStr(fRect.Bottom-fRect.Top)+' and '+IntToStr(fDiff.Height));
  while not src.isEOF do
    dest.WriteNextPixel(src.ReadNextPixel);
  dest.Free;
  src.Free;
  Result:=true;
end;

function TPatchImageCommand.UndoPrediction: Boolean;
begin
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

constructor TBrushCommand.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fPoints:=TList.Create;
end;

procedure TBrushCommand.SetBrushSize(value: Word);
begin
  fBrushSize:=value;
  fR2:=Sqr(value);
  SetBrushShape(fBrushShape); //����� ����������
end;

procedure TBrushCommand.SetBrushShape(value: TBrushShape);
begin
  fBrushShape:=value;
  if value=bsSquare then fBelongFunc:=PixelBelongsToRectBrush
  else if value=bsRound then
    if fBrushSize div 2 = 0 then
      fBelongFunc:=PixelBelongsToEvenRoundBrush
    else
      fBelongFunc:=PixelBelongsToOddRoundBrush;
end;

function TBrushCommand.PixelBelongsToRectBrush(x,y: Integer): Boolean;
begin
  Result:=true;
end;

function TBrushCommand.PixelBelongsToEvenRoundBrush(x,y: Integer): Boolean;
begin
  Result:=(Sqr(x)+Sqr(y)+x+y<fR2);
end;

function TBrushCommand.PixelBelongsToOddRoundBrush(x,y: Integer): Boolean;
begin
  Result:=(Sqr(x)+Sqr(y)<fR2);
end;

procedure TBrushCommand.FigureAboutColorsAndBitdepth;
var btmp: TExtendedPNGObject;
  fActualCount: Integer;
  p: Integer;
  fFutureOwner: TRasterImageDocument;
  //����� ���� �� �������� � ��� ����
begin
//��������� �����: ���� ���� ����������� �������� ��� ������������������ ��������,
//���� �� �� ���������
//�� ���� ��� ����� �������� ���������� ��� �� �������, �� �� ���������, � �����
//������ ��������, � �������.
  fFutureOwner:=GetDoc;
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
      if fColorExistsInPalette then
        fColorIndex:=p
      else begin
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

destructor TBrushCommand.Destroy;
begin
  fPoints.Free;
  inherited Destroy;
end;

procedure TBrushCommand.GetBounds;
var ourRect: TRect;
    i: Integer;
    p: TWordPoint;
begin
  BrushColor:=GetDoc.PrimaryColor;
  BrushSize:=GetDoc.BrushSize;
  BrushShape:=GetDoc.BrushShape;
  fRect:=Rect(0,0,0,0);
  for i:=0 to fPoints.Count-1 do begin
    p.AsPointer:=fPoints[i];
    ourRect:=Rect(p.X-fBrushSize div 2,p.Y-fBrushSize div 2,(p.X+(fBrushSize-1) div 2)+1, (p.Y+(fBrushSize-1) div 2)+1);
    //��������� Left � ����������� Top �������� ������������ �������,
    //� Right � Bottom - ���.
    CoverRect(fRect,ourRect);
  end;
end;

function TBrushCommand.Draw(aX,aY: Word): Boolean;
var point: TWordPoint;
begin
  point.X:=aX;
  point.Y:=aY;
  Result:=(fPoints.IndexOf(point.AsPointer)=-1);
  if Result then begin
    fPoints.Add(point.AsPointer);
  end;
end;

procedure TBrushCommand.DefineProperties(filer: TFiler);
begin
//  filer.DefineBinaryProperty('points',ReadPoints,WritePoints,true);
  filer.DefineProperty('points',ReadPointsAsText,WritePointsAsText,true);
end;

procedure TBrushCommand.WritePoints(stream: TStream);
begin
  ReducePoints;

end;

procedure TBrushCommand.ReadPoints(stream: TStream);
begin

end;

procedure TBrushCommand.WritePointsAsText(writer: TWriter);
var i: Integer;
    cur: TWordPoint;
begin
  ReducePoints;
  writer.WriteListBegin;
  for i:=0 to fPoints.Count-1 do begin
    cur:=TWordPoint(fPoints[i]);
    writer.WriteString('('+IntToStr(cur.X)+';'+IntToStr(cur.Y)+')');
  end;
  writer.WriteListEnd;
end;

procedure TBrushCommand.ReadPointsAsText(reader: TReader);
var p: TSimpleParser;
    cur: TWordPoint;
begin
  reader.ReadListBegin;
  p:=TSimpleParser.Create;
  fPoints.Clear;
  while not reader.EndOfList do begin
    p.AssignString(reader.ReadString);
    p.getChar; //'('
    cur.X:=p.getInt;
    //; - ��� delimiter, ��� ������ �� ����. 
    cur.Y:=p.getInt;
    fPoints.Add(cur.AsPointer);
  end;
  p.Free;
  reader.ReadListEnd;
  fPointsReduced:=true; //�� ��� ����� ������������, ����� �����������!
end;


procedure TBrushCommand.ReducePoints;
var pix: array of array of Byte;
    i,j,k: Integer;
    offset: TWordPoint;
    hasUniquePoint: boolean;
begin
  if fPointsReduced then Exit;

  GetBounds;
  //�������� ������� ������������ ���������� �����
  SetLength(pix,fRect.Right-fRect.Left,fRect.Bottom-fRect.Top);
  for i:=0 to fPoints.Count-1 do begin
    offset.AsPointer:=fPoints[i];
    dec(offset.X,fRect.Left);
    dec(offset.Y,fRect.Top);
    for j:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do
      for k:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do begin
        Assert(offset.X+k>=0,'x<0');
        Assert(offset.X+k<fRect.Right-fRect.Left,'x>=length');
        Assert(offset.Y+j>=0,'y<0');
        Assert(offset.Y+j<fRect.Bottom-fRect.Top,'y>=length');
        if fBelongFunc(k,j) and (pix[offset.X+k,offset.Y+j]<255) then
          inc(pix[offset.X+k,offset.Y+j])
      end;
  end;  //����������, ������� ��� "���������" ������ �����.
  for i:=fPoints.Count-1 downto 0 do begin //� ������ ��������� ������� ���� ���-������
    offset.AsPointer:=fPoints[i];
    dec(offset.X,fRect.Left);
    dec(offset.Y,fRect.Top);
    HasUniquePoint:=false;
    for j:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do
      for k:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do
        if fBelongFunc(k,j) and (pix[offset.X+k,offset.Y+j]=1) then begin
          HasUniquePoint:=true;
          break;
        end;
    if not HasUniquePoint then begin
      fPoints.Delete(i);
      for j:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do
        for k:=-(fBrushSize div 2) to ((fBrushSize-1) div 2) do
          if fBelongFunc(k,j) then
            dec(pix[offset.X+k,offset.Y+j]);
    end;
  end;
  
  fPointsReduced:=true;
end;

function TBrushCommand.InternalExecute: Boolean;
var btmp: TExtendedPngObject;
    dest: TPngObjectIterator;
    i: Integer;
    point: TWordPoint;
begin
  Result:=fPoints.Count>0;
  if not Result then exit;
  //���� ��� ��������� � ����� TBrushCommand,
  //�����, ����� ����� ������� ����� ������, ���������,
  //��� ����� ����� ������� � ������
  FigureAboutColorsAndBitdepth;
  fPointsReduced:=false; //�������  
  ReducePoints;
  btmp:=GetDoc.Btmp;
  //����� ����, ������, ���� ����, ������������ �����������
  if not fColorExistsInPalette then begin
    if fNeedToChangeBitDepth then begin
      //�������� ������������������
      Btmp:=btmp.GetInOtherFormat(fNewColorMode,fNewBitDepth);
      GetDoc.Btmp.Free;
      GetDoc.Btmp:=Btmp;
    end
    else begin
      raise Exception.Create('convert to palette: dangerous procedure, may corrupt data');
      Btmp.ConvertToPalette;
    end;
    if (fNewColorMode=COLOR_PALETTE) and (Btmp.GetColorIndex(fBrushColor)=-1) then
      fColorIndex:=Btmp.AddToPalette(fBrushColor);
  end;
  //�������� ������
  for i:=0 to fPoints.Count-1 do begin
    point:=TWordPoint(fPoints[i]);
    dest:=btmp.CreateIteratorForCropped(Rect(point.X-fBrushSize div 2,point.Y-fBrushSize div 2,
      point.X + (fBrushSize-1) div 2 + 1, point.Y+(fBrushSize-1) div 2 + 1));
    while not dest.isEOF do
      if fBelongFunc(dest.CurColumn-point.X,dest.CurLine-point.Y)
        and (dest.PeekNextPixel<>fColorIndex) then begin
        PointChanged(dest.CurColumn,dest.CurLine);
        dest.WriteNextPixel(fColorIndex);
      end
      else
        dest.SkipPixel;
    dest.Free;
  end;
end;

function TBrushCommand.UndoPrediction: Boolean;
var src,dest: TPngObjectIterator;
begin
  src:=GetDoc.Btmp.CreateIteratorForCropped(fRect);
  dest:=fPredictor.CreateIterator;
  //src ����� �� ��� "�����", ��� dest �� ������.
  if (GetDoc.Btmp.Header.ColorType=fPredictor.Header.ColorType) and
    (GetDoc.Btmp.Header.BitDepth=fPredictor.Header.BitDepth) then begin
      while not src.isEOF do
        dest.WriteNextPixel(src.ReadNextPixel);
    Result:=true;
  end
  else
    Result:=false;  //��������� ���������� ������, �� ���������� DLRN
  src.Free;
  dest.Free;
end;


initialization
  RegisterClasses([TBrushCommand]);

end.
