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

//GetBounds выставляет предварительные размеры прямоугольника, где произойдет действие

//если была палитра, преобразуем картинку в RGB или Grayscale, уж больно неудобно
//с палитрой обращаться.

//и при вызове Execute картинка из него будет сохранена в fDiff, и вызван internalExecute

//в потомках описываем internalExecute - как изменить изображение
//он может в числе прочего поменять формат всего изображения.
//при этом для каждой измененной точки внутри прямоуг. вызываем PointChanged, тем самым
//находим минимально необх. прямоугольник, который нужен для undo.
//далее, Top/Left принимают окончательное значение.
//наконец, вызывается UndoPrediction, который пытается, располагая только результирующей
//картинкой и свойствами команды, которые сохраняются в файл (не считая fDiff), восстановить
//(предсказать) исходное изображение. Если это реализовано, то из fDiff будет вычтен fPredictor.
//UndoPrediction имеет право узнать ColorMode и BitDepth.

//при исполнении Undo первым делом запускается UndoPrediction, который дает ровно то же
//предсказанное изображение, что и раньше. Его мы прибавляем к загруженному fDiff.
//если формат изображения в документе поменялся и fDiff не занимает всю его область,
//то мы приводим его к старому формату, за исключением, возможно, измененной области
//наконец, мы "вклеиваем" на место fDiff.
  protected
    fDiff: TExtendedPngObject;
    fPredictor: TExtendedPngObject;
    fRect,fUpdateRect: TRect; //расположение заплатки и вычисление мин. необх.
    fOldBitDepth,fOldColorType: Byte;
    fImageFormatChanged: Boolean;
    procedure SetOldBitDepth(value: Byte);
    procedure SetOldColorType(value: Byte);
    procedure GetBounds; virtual; abstract;//инициализировать fLeft,fTop,fRight,fBottom
    procedure PointChanged(const x,y: Integer); //вызывается из потомка,
    // если в (x,y) произошли изменения, нужна для нахождения минимального прямоуг.
    function UndoPrediction: Boolean; virtual;  //false означает
    //что мы ничего не можем предсказать, поэтому оставляем картинку как есть
    function InternalExecute: Boolean; virtual; abstract; //false означает,
    //что применение команды ничего не изменило и ее стоит изъять из дерева
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Undo: Boolean; override;
  published
    property diff: TExtendedPngObject read fDiff write fDiff stored fActiveBranch;
    property Left: Integer read fRect.Left write fRect.Left;
    property Top: Integer read fRect.Top write fRect.Top;
    property BitDepth: Byte read fOldBitDepth write SetOldBitDepth;
    property ColorType: Byte read fOldColorType write SetOldColorType default 255;
    //на всякий случай, BitDepth и ColorType сохраняем всегда, "не утянет"
    //потом подумаем, нельзя ли иногда схалявить
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
    fColorExistsInPalette: Boolean; //есть ли готовый индекс для цвета, которым
    //закрашиваем
    fNeedToChangeBitDepth: Boolean; //и палитра набита под завязку, придется бит/пиксель повышать
    fNewBitDepth: Byte;
    fNewColorMode: Byte;
    fColorIndex: Cardinal;  //от 1 бита до 24 бит, по обстоятельствам
//    fRect: TRect;
    fPointsReduced: Boolean;  //отметка, что не нужно шерстить набор точек по-новой
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
    procedure GetBounds; override;  //заполняет fRect
    function UndoPrediction: Boolean; override;
    //просто заносит текущее сост. прямоуг.
    //тем самым не надо сохранять то, что не изменилось
  public
    constructor Create(aOwner: TComponent); override;
    //поскольку он должен работать с документом (только для чтения) еще до
    //исполнения DispatchCommand, передаём ему заблаговременно
    destructor Destroy; override; //TList надо освободить
    function Draw(aX,aY: Word): boolean;  //один мазок кисти
    //если хоть как-то меняется, заносим в fXPoints/fYPoints
    function InternalExecute: Boolean; override;
  published
    property BrushColor: TColor read fBrushColor write fBrushColor;
    property BrushSize: Word read fBrushSize write SetBrushSize;
    property BrushShape: TBrushShape read fBrushShape write SetBrushShape;
  end;

TSaveToJPEGCommand = class (TRasterImageDocumentCommand,ITerminalCommand)
  public
    function Execute: Boolean; override;
    function Undo: Boolean; override;
    function Caption: string; override;
end;

implementation

uses pngImage,gamma_function,sysUtils,simple_parser_lib,math,PNGImageIterators,
  JPEG,strUtils;

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
  fDiff:=TExtendedPngObject.Create; //будет сохраняться в файл
  fOldColorType:=255;
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

procedure TPatchImageCommand.SetOldBitDepth(value: Byte);
begin
  if fOldBitDepth<>value then begin
    fOldBitDepth:=value;
    fImageFormatChanged:=true;
  end;
end;

procedure TPatchImageCommand.SetOldColorType(value: Byte);
begin
  if fOldColorType<>value then begin
    fOldColorType:=value;
    fImageFormatChanged:=true;
  end;
end;

function TPatchImageCommand.Execute: Boolean;
var doc: TRasterImageDocument;
  btmp,tmp: TExtendedPNGObject;
  src,dest: TPngObjectIterator;
begin
  getBounds;  //инициализирует fRect
  doc:=GetDoc;
  btmp:=doc.Btmp;
  //избавимся от palette, если таковая имелась
  if btmp.GetInMinimalGrayscaleOrRGB(tmp) then begin
    doc.btmp.Free;
    doc.btmp:=tmp;
    btmp:=doc.Btmp;
  end;
  //вот это и примем за исходную картинку
  fOldBitDepth:=btmp.Header.BitDepth;
  fOldColorType:=btmp.Header.ColorType;
  //обозначили заплатку
  fDiff.Free;
  fDiff:=TExtendedPNGObject.CreateBlank(Btmp.Header.ColorType,
    Btmp.Header.BitDepth,fRect.Right-fRect.Left,fRect.Bottom-fRect.Top);
  //палитра будет стандартной, если ColorType=COLOR_PALETTE
  //перед сохранением необходимо присвоить реальную
//  fDiff.Canvas.CopyRect(Rect(0,0,fDiff.Width,fDiff.Height),Doc.Btmp.Canvas,
//    Rect(fLeft,fTop,fRight,fBottom));
  //copyRect вроде быстрый, но не thread-safe (а мы хотим быть thread-safe, не знаю зачем)
  //и кто его знает, что он там делает, этот stretchBlt
  //так надежнее:
  src:=btmp.CreateIteratorForCropped(fRect);
  dest:=fDiff.CreateIterator;
  assert(fRect.Right-fRect.Left=fDiff.width,'execute patch command: width not equal:'+IntToStr(fRect.Right-fRect.Left)+' and '+IntToStr(fDiff.Width));
  assert(fRect.Bottom-fRect.Top=fDiff.height,'execute patch command: height not equal'+IntToStr(fRect.Bottom-fRect.Top)+' and '+IntToStr(fDiff.Height));
  while not src.isEOF do
    dest.WriteNextPixel(src.ReadNextPixel);
  dest.Free;
  src.Free;
  //можно еще ввести какой-нибудь пакетный режим, чтоб ещё быстрее
  //в fUpdateRect загоняем фактически -Inf и +Inf
  fUpdateRect.Left:=fRect.Right;
  fUpdateRect.Right:=fRect.Left-1;
  fUpdateRect.Top:=fRect.Bottom;
  fUpdateRect.Bottom:=fRect.Top-1;
  //храним в fDiff копию того фрагмента, который начнем мучать
  Result:=InternalExecute and not IsRectEmpty(fUpdateRect);
  if Result then begin
  //после InternalExecute еще могли поменяться границы
    if (fRect.Left<>fUpdateRect.Left) or (fRect.Right<>fUpdateRect.Right)
      or (fRect.Top<>fUpdateRect.Top) or (fRect.Bottom<>fUpdateRect.Bottom) then
    begin
      tmp:=fDiff;
      fDiff:=TExtendedPNGObject.CreateBlank(tmp.Header.ColorType,
        tmp.Header.BitDepth,fUpdateRect.Right-fUpdateRect.Left,fUpdateRect.Bottom-fUpdateRect.Top);
      //и снова станд. палитра
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
    //fPredictor должен иметь те же параметры ColorType,BitDepth, Width, Height
    fPredictor:=TExtendedPngObject.CreateBlank(fDiff.Header.ColorType,
      fDiff.Header.BitDepth,fDiff.Width,fDiff.Height);
    //и у предиктора тоже палитра стандартная
    if UndoPrediction then begin
      //нарисует на Predictor исх. картинку исходя из того, что он
      //может знать по конечному результату
      //сейчас "скользкая" часть - вычесть одну картинку из другой
      //RGB24 - надо вычитать по каждому цвету
      //Grayscale - значение целиком
      //Palette - значение целиком, адекватность должен обеспечить UndoPrediction
      src:=fPredictor.CreateIterator;
      dest:=fDiff.CreateIterator;
      //that's how I sub.
      while not src.isEOF do
        dest.WriteNextSubpixel((dest.PeekNextSubpixel-src.ReadNextSubpixel) and 255);
      //ох незадача, он же при "умном сохранении" может ужаться в плане цветности, тогда
      //мы по нему не восстановим ориг. формат изобр.
      //поэтому добавили свойства BitDepth и ColorType
      dest.Free;
      src.Free;
    end;
    fPredictor.Free;
    //а на нет и суда нет
    //наконец, сообщим документу, какую область мы изменили
    doc.AddToChangeRect(fRect);
  end;
  //в противном случае команда вот-вот будет удалена, нет нужды освобождать
  //память впереди паровоза
end;

function TPatchImageCommand.Undo: Boolean;
var doc: TRasterImageDocument;
    src,dest: TPngObjectIterator;
    tmp: TExtendedPngObject;
begin
  //формируем прямоугольник, на котором произошли изменения. Его верхний угол хранится в Top/Left
  //он по сути на месте, а по размерам заплатки восст. нижний правый угол
  fRect.Bottom:=fRect.Top+fDiff.Height;
  fRect.Right:=fRect.Left+fDiff.Width;
  doc:=GetDoc;
  doc.AddToChangeRect(fRect); //можно сразу сообщить, мы уже все знаем.
  if ColorType=255 then ColorType:=doc.Btmp.Header.ColorType;//если формат не поменялся,
  if BitDepth=0 then BitDepth:=doc.Btmp.Header.BitDepth;//мы его и не сохраняем!
  //возможно, формат fDiff успел поменяться после сохранения, для уменьшения размера
  if (fDiff.Header.BitDepth<>BitDepth) or (fDiff.Header.ColorType<>ColorType) then begin
    tmp:=fDiff.GetInOtherFormat(ColorType,BitDepth);
    fDiff.Free;
    fDiff:=tmp;
  end;
  fPredictor:=TExtendedPngObject.CreateBlank(ColorType,BitDepth,fDiff.Width,fDiff.Height);
  if UndoPrediction then begin
    //в прошлый раз вычитали fPredictor, теперь прибавим назад.
    src:=fPredictor.CreateIterator;
    dest:=fDiff.CreateIterator;
    //that's how I add.
    while not src.isEOF do
      dest.WriteNextSubpixel((dest.PeekNextSubpixel+src.ReadNextSubpixel) and 255);
    dest.Free;
    src.Free;
  end;
  fPredictor.Free;
  //а на нет и суда нет
  //осталось вправить картинку на место
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
  if Result then index:=(index*maxColorVal) div 255; //к примеру
end;

constructor TBrushCommand.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fPoints:=TList.Create;
end;

procedure TBrushCommand.SetBrushSize(value: Word);
begin
  fBrushSize:=value;
  fR2:=Sqr((value-1) div 2);
  if (value and 1) = 0 then
    fR2:=fR2 + (value-1) div 2;
  SetBrushShape(fBrushShape); //может поменяться
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
  //можно было бы обойтись и без него
begin
//философия такая: если есть возможность обойтись без переформатирования картинки,
//надо за неё цепляться
//но если все равно повышать количество бит на пиксель, то не жадничать, а сразу
//делать побольше, с запасом.
  fFutureOwner:=GetDoc;
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
          fNewBitDepth:=Btmp.Header.BitDepth;
          fColorIndex:=fActualCount;
        end;
      end;
    end;
    //COLOR_PALETTE никогда не появится, потому что TPatchImageCommand его не любит
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
  BrushSize:=Round(GetDoc.BrushSize/GetDoc.get_Scale);
  BrushShape:=GetDoc.BrushShape;
  fRect:=Rect(0,0,0,0);
  for i:=0 to fPoints.Count-1 do begin
    p.AsPointer:=fPoints[i];
    ourRect:=Rect(p.X-fBrushSize div 2,p.Y-fBrushSize div 2,(p.X+(fBrushSize-1) div 2)+1, (p.Y+(fBrushSize-1) div 2)+1);
    //вертикаль Left и горизонталь Top содержат изменившиеся пиксели,
    //а Right и Bottom - нет.
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
    //; - это delimiter, его читать не надо. 
    cur.Y:=p.getInt;
    fPoints.Add(cur.AsPointer);
  end;
  p.Free;
  reader.ReadListEnd;
  fPointsReduced:=true; //их еще давно редуцировали, перед сохранением!
end;


procedure TBrushCommand.ReducePoints;
var pix: array of array of Byte;
    i,j,k: Integer;
    offset: TWordPoint;
    hasUniquePoint: boolean;
begin
  if fPointsReduced then Exit;
  GetBounds;
  //надеемся малость подсократить количество точек
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
  end;  //определили, сколько раз "стирается" каждая точка.
  for i:=fPoints.Count-1 downto 0 do begin //а теперь попробуем удалить хоть что-нибудь
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
  //пока все выполняем в одной TBrushCommand,
  //потом, когда новую команду будем делать, сообразим,
  //как общие куски вынести в предка
  FigureAboutColorsAndBitdepth;
  fPointsReduced:=false; //отладка  
  ReducePoints;
  btmp:=GetDoc.Btmp;
  //бекап есть, теперь, если надо, переделываем изображение
  if not fColorExistsInPalette then begin
//    if fNeedToChangeBitDepth then begin
      //коренное переформатирование
      Btmp:=btmp.GetInOtherFormat(fNewColorMode,fNewBitDepth);
      GetDoc.Btmp.Free;
      GetDoc.Btmp:=Btmp;
//    end
//    else begin
//      raise Exception.Create('convert to palette: dangerous procedure, may corrupt data');
//      Btmp.ConvertToPalette;
//    end;
    if (fNewColorMode=COLOR_PALETTE) and (Btmp.GetColorIndex(fBrushColor)=-1) then
      fColorIndex:=Btmp.AddToPalette(fBrushColor);
  end;
  //работаем кистью
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
  Result:=true;
  //src такой же или "ширше", чем dest по цветам.
  if ColorType=Color_GRAYSCALE then
    while not src.isEOF do
      dest.WriteNextAsGrayscale(src.ReadNextAsGrayscale)
  else if ColorType=COLOR_RGB then
    while not src.isEOF do
      dest.WriteNextAsRGB(src.ReadNextAsRGB)
  else
    //других не поддерживает
    Result:=false;  //обеспечит корректную работу, но громоздкий DLRN
  src.Free;
  dest.Free;
end;

(*
    TSaveToJPEGCommand
                          *)
function TSaveToJPEGCommand.Execute: Boolean;
var jpgImg: TJPEGImage;
    diff: TExtendedPngObject;
    btmp: TBitmap;
    s,fn1: string;
begin
  btmp:=TBitmap.Create;
  try
    GetDoc.Btmp.AssignTo(btmp);
    jpgImg:=TJPEGImage.Create;
    try
      jpgImg.Assign(Btmp);
      jpgImg.CompressionQuality:=1;  //потом сделаем настройку
      s:=ExtractFilePath(GetDoc.filename);
      s:=LeftStr(s,Length(s)-5);
      fn1:=s+ChangeFileExt(ExtractFileName(GetDoc.filename),'.jpg');
      jpgImg.SaveToFile(fn1);
      //теперь снова загружаем и находим разность
      jpgImg.LoadFromFile(fn1);
      btmp.Assign(jpgImg);
      diff:=TExtendedPngObject.Create;
      try
        diff.Assign(btmp);
        


        //        GetDoc.Btmp.Assign(btmp);
      finally
        diff.Free;
      end;
      Result:=true;
    finally
      jpgImg.Free;
    end;
  finally
    btmp.Free;
  end;
end;

function TSaveToJPEGCommand.Undo: Boolean;
begin
  Result:=true;
end;

function TSaveToJpegCommand.Caption: String;
var s,fn1: string;
begin
  s:=ExtractFilePath(GetDoc.filename);
  s:=LeftStr(s,Length(s)-5);
  fn1:=s+ChangeFileExt(ExtractFileName(GetDoc.filename),'.jpg');
  Result:=Format('saved to %s',[fn1]);
end;

initialization
  RegisterClasses([TBrushCommand,TSaveToJpegCommand]);

end.
