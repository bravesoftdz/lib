unit RasterImageCommands;

interface

uses classes,command_class_lib,Image_on_demand,pngImageAdvanced,graphics,types;

type

TRasterImageDocumentCommand = class (TAbstractTreeCommand)
  public
    function GetDoc: TRasterImageDocument;
  end;
TPatchImageCommand = class (TRasterImageDocumentCommand)
//базовая команда при работе с растровыми изображениями
  protected
    fDiff: TExtendedPngObject;
    fPredictor: TExtendedPngObject;
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
    property diff: TExtendedPngObject read fDiff write fDiff;
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

TWordPoint=record
  case Integer of
    0: (X,Y: Word);
    1: (AsPointer: Pointer);
end;

TBrushCommand = class (TRasterImageDocumentCommand)
  private
    fFutureOwner: TRasterImageDocument;

    fDiff: TExtendedPngObject;

    fBrushColor: TColor;
    fBrushSize: Word;
    fBrushShape: TBrushShape;
    fPoints: TList;
    fColorExistsInPalette: Boolean; //есть ли готовый индекс для цвета, которым
    //закрашиваем
    fNeedToChangeBitDepth: Boolean; //и палитра набита под завязку, придется бит/пиксель повышать
    fNewBitDepth: Byte;
    fNewColorMode: Byte;
    fColorIndex: Cardinal;  //от 1 бита до 24 бит, по обстоятельствам
    fRect: TRect;
    fPointsReduced: Boolean;  //отметка, что не нужно шерстить набор точек по-новой
    procedure ReadPointsAsText(reader: TReader);
    procedure WritePointsAsText(writer: TWriter);
    procedure WritePoints(stream: TStream);
    procedure ReadPoints(stream: TStream);
    procedure ReducePoints;
  protected
    procedure DefineProperties(filer: TFiler); override;
  public
    constructor CreateStandalone(myFutureOwner: TRasterImageDocument);
    //поскольку он должен работать с документом (только для чтения) еще до
    //исполнения DispatchCommand, передаём ему заблаговременно
    destructor Destroy; override; //TList надо освободить
    procedure Draw(aX,aY: Word);  //один мазок кисти
    //если хоть как-то меняется, заносим в fXPoints/fYPoints
    function Execute: Boolean; override;
  published
    property Top: Integer read fRect.Top write fRect.Top;
    property Left: Integer read fRect.Left write fRect.Left;
  end;
implementation

uses pngImage,gamma_function,sysUtils,simple_parser_lib;

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
  fDiff:=TExtendedPngObject.CreateBlank(color_RGB,8,0,0); //будет сохраняться в файл
  fDiff.Filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  fDiff.CompressionLevel:=9;
  fPredictor:=TExtendedPngObject.CreateBlank(color_RGB,8,0,0);  //хранится временно
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
  //обозначили заплатку
  fDiff.Resize(fRight-fLeft,fBottom-fTop);
  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),Doc.Btmp.Canvas,
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
    doc: TRasterImageDocument;
begin
  getBounds;  //размеры заплатки мы могли бы по изображению понять, но расположение
  //все равно узнавать надо!
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
    //а на нет и суда нет
  //осталось вправить картинку на место
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
  if Result then index:=(index*maxColorVal) div 255; //к примеру
end;

constructor TBrushCommand.CreateStandalone(myFutureOwner: TRasterImageDocument);
var btmp: TExtendedPNGObject;
  fActualCount: Integer;
  p: Integer;
begin
//философия такая: если есть возможность обойтись без переформатирования картинки,
//надо за неё цепляться
//но если все равно повышать количество бит на пиксель, то не жадничать, а сразу
//делать побольше, с запасом.
  inherited Create(nil);

  fDiff:=TExtendedPngObject.Create;

  fPoints:=TList.Create;
  fFutureOwner:=myFutureOwner;
  fBrushColor:=fFutureOwner.PrimaryColor;
  fBrushSize:=fFutureOwner.BrushSize;
  fBrushShape:=fFutureOwner.BrushShape;
  btmp:=fFutureOwner.Btmp;
  case Btmp.Header.ColorType of
    COLOR_RGB: begin
      fColorExistsInPalette:=true;
      fColorIndex:=TColorToRGBTriple(fBrushColor);
      //ничего менять не надо!
      end;
    COLOR_GRAYSCALE: begin
      fColorExistsInPalette:=
        BrushColorInMonochromePalette(fBrushColor,Btmp.Header.BitDepth,fColorIndex);
      if not fColorExistsInPalette then begin
        //очень не хочется изменять биты/пиксель
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
      //нужно определить, если ли наш цвет в палитре
      p:=btmp.GetColorIndex(fBrushColor);
      fColorExistsInPalette:=(p>=0);
      if fColorExistsInPalette then
        fColorIndex:=p
      else begin
        //вот ведь незадача
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
  fDiff.Free;
  inherited Destroy;
end;

procedure TBrushCommand.Draw(aX,aY: Word);
var i: TPngObjectIterator;
    changes: Boolean;
    point: TWordPoint;
    ourRect: TRect;
begin
  ourRect:=Rect(aX-fBrushSize div 2,aY-fBrushSize div 2,aX+fBrushSize div 2, aY+fBrushSize div 2);
  //проверяем, окажет ли это хоть какое-то влияние?
  if fColorExistsInPalette then begin
    i:=fFutureOwner.Btmp.CreateIteratorForCropped(ourRect);
    changes:=false;
    while not i.isEOF do
      if i.ReadNextPixel<>fColorIndex then begin
        changes:=true;
        break;
      end;
    i.Free;
    if not changes then Exit;
  end;
  point.X:=aX;
  point.Y:=aY;
  fPoints.Add(point.AsPointer);
  CoverRect(fRect,ourRect);
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
    p.NextChar; //'('
    cur.X:=p.getInt;
    p.NextChar; //';'
    cur.Y:=p.getInt;
    fPoints.Add(cur.AsPointer);
  end;
  reader.ReadListEnd;
end;


procedure TBrushCommand.ReducePoints;
begin
  if fPointsReduced then Exit;
  //надеемся малость подсократить количество точек
  //а пока ничего не делаем
  fPointsReduced:=true;
end;

function TBrushCommand.Execute: Boolean;
var btmp: TExtendedPngObject;
    src,dest: TPngObjectIterator;
    i: Integer;
    point: TWordPoint;
    sizesq: Integer;
begin
  Result:=fPoints.Count>0;
  if not Result then exit;
  //пока все выполняем в одной TBrushCommand,
  //потом, когда новую команду будем делать, сообразим,
  //как общие куски вынести в предка
  ReducePoints;
  fFutureOwner.AddToChangeRect(fRect); //для корректного отобр. изменений
//  fDiff.Resize(fRect.Right-fRect.Left,fRect.Bottom-fRect.Top);  //сюда сохраним изобр.
  //перед началом экзекуции
  btmp:=fFutureOwner.Btmp;
  fDiff.Free;
  fDiff:=TExtendedPngObject.CreateBlank(Btmp.Header.ColorType,Btmp.Header.BitDepth,
    fRect.Right-fRect.Left,fRect.Bottom-fRect.Top);
  src:=btmp.CreateIteratorForCropped(fRect);
  dest:=fDiff.CreateIterator;
  while not src.isEOF do
    dest.WriteNextPixel(src.ReadNextPixel); //сейчас режим цвета и бит/пиксель совпадают
  dest.Free;
  src.Free;
  //бекап есть, теперь, если надо, переделываем изображение
  if not fColorExistsInPalette then begin
    if fNeedToChangeBitDepth then begin
      //коренное переформатирование
      Btmp:=btmp.GetInOtherFormat(fNewColorMode,fNewBitDepth);
      fFutureOwner.Btmp.Free;
      fFutureOwner.Btmp:=Btmp;
    end
    else begin
      raise Exception.Create('convert to palette: dangerous procedure, may corrupt data');
      Btmp.ConvertToPalette;
    end;
    if (fNewColorMode=COLOR_PALETTE) and (Btmp.GetColorIndex(fBrushColor)=-1) then
      fColorIndex:=Btmp.AddToPalette(fBrushColor);
  end;
  //работаем кистью
  for i:=0 to fPoints.Count-1 do begin
    point:=TWordPoint(fPoints[i]);
    dest:=btmp.CreateIteratorForCropped(Rect(point.X-fBrushSize div 2,point.Y-fBrushSize div 2,
      point.X + fBrushSize div 2, point.Y+fBrushSize div 2));
    if fBrushShape=bsSquare then
      while dest.isEOF do
        dest.WriteNextPixel(fColorIndex)
    else begin
      sizesq:=Sqr(fBrushSize div 2);
      while dest.isEOF do
       if sqr(dest.CurLine-point.Y)+sqr(dest.CurColumn-point.X)<sizesq then
        dest.WriteNextPixel(fColorIndex)
    end;
    dest.Free;
  end;
end;


initialization
  RegisterClasses([TRectBrushCommand]);

end.
