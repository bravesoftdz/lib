unit IGraphicObject_commands;

interface

uses command_class_lib,classes,types,observer_pattern_interfaces,graphics,
streamable_component_list,iterator_lib,ExtCtrls;

type

TGraphicResizeCorrector=class(TPersistent)
  private
    fstack: array of TRect;
    fcount,fpopcount: Integer;
    fReverse: Boolean;
    procedure WriteData(stream: TStream);
    procedure ReadData(stream: TStream);
    function NonTrivial: boolean;
    procedure SetReverse(value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    property reverse: boolean read fReverse write SetReverse;
    procedure Push(Rect: TRect);
    function Pop: TRect;
    procedure Clear;
end;

IGraphicObject=Interface
['{403B6D11-F900-4927-A65E-CD1ABEC0F017}']
  function Rect: TRect; //реальные размеры
  function SymbolRect: TRect; //как оно отобр. на экране
  function DistanceTo(ax,ay: Integer): Real;
  procedure Move(dx,dy: Integer);
  procedure Resize(dx1,dy1,dx2,dy2: Integer;Corrector: TGraphicResizeCorrector=nil);
  procedure Rotate(alpha: Real; Xcenter,Ycenter: Integer; corrector: TGraphicResizeCorrector=nil);
  function ResizableByX: boolean;
  function ResizableByY: boolean;
  function isOkToPaste: boolean;
  function Implementor: TComponent;
  procedure Draw(canvas: TCanvas);
end;

TDocumentWithImage = class(TAbstractDocument)
  protected
    fGraphicObjectsIterator: TAbstractDocumentInterfaceIterator;
  public
    constructor Create(owner: TComponent); override;
    function XPix2Val(X: Integer): Integer; virtual;  //default: result:=x
    function YPix2Val(Y: Integer): Integer; virtual;  //default: result:=y
    function XVal2Pix(X: Integer): Integer; virtual;  //default: result:=x
    function YVal2Pix(Y: Integer): Integer; virtual;  //default: result:=y
    function Get_square_size: Integer; virtual; //default impl: 10
    function Get_sensitivity: Integer; virtual; //default impl: 15
    function Get_duplicate_shift_x: Integer; virtual; //default impl: 25
    function Get_duplicate_shift_y: Integer; virtual; //default impl: 25
    function Get_scale: Real; virtual;  //default impl: 1
    function Get_Image: TImage; virtual; abstract;
    property GraphicObjectsIterator: TAbstractDocumentInterfaceIterator read fGraphicObjectsIterator;
  end;

TGraphicObjectGroup=class(TStreamableComponentList,IGraphicObject,IAdvancedProperties)
public
  function Rect: TRect;
  function SymbolRect: TRect;
  function DistanceTo(ax,ay: Integer): Real;
  procedure Move(dx,dy: Integer);
  procedure Resize(dx1,dy1,dx2,dy2: Integer;Corrector: TGraphicResizeCorrector=nil);
  procedure Rotate(alpha: Real; XCenter,YCenter: Integer; Corrector: TGraphicResizeCorrector=nil);
  procedure SetSize(x1,y1,x2,y2: Integer);
  function implementor: TComponent;
  function Iitem(index: Integer): IGraphicObject;
  procedure Draw(canvas: TCanvas);
  function ResizableByX: boolean;
  function ResizableByY: boolean;
  function isOkToPaste: boolean;
  procedure RegisterProperties(proc: RegisterPropertyProc);
  procedure AddTitleAndHint(proc: AddTitleAndHintProc);
  procedure UnregisterProperties(proc: UnregisterPropertyProc);
end;


TGraphicCommands=class(THashedCommand)
  private
    fObjects: TGraphicObjectGroup;
    fObjectNames: string;
  public
    procedure ResolveMemory; override;
  published
    property Objects: TGraphicObjectGroup read fObjects write fObjects;
  end;

TAddGraphicCommand=class(TGraphicCommands)
  private
    fFromClipbd: Boolean;
    procedure SetClipbd(value: boolean); virtual;
  public
    constructor Create(owner: TComponent); override;
    constructor NewGr(aObjects: TGraphicObjectGroup; paste: Boolean=false; aImageIndex: Integer=-1);
    constructor NewObj(aObject: IGraphicObject; paste: Boolean=false; aImageIndex: Integer=-1);
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
  published
    property FromClipbd: boolean read fFromClipbd write SetClipbd default false;
end;

TDeleteGraphicCommand=class(TAddGraphicCommand)
  private
    procedure SetClipbd(value: boolean); override;
  public
    constructor Create(owner: TComponent); override;
    constructor New(aObjects: TGraphicObjectGroup; cut: Boolean=false; aImageIndex: Integer=-1);
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
end;

TMoveGraphicCommand=class(TGraphicCommands)
  //всего лишь передвинуть, поэтому не нужно хитрых бекапов, хватит 2 коорд
  private
    fx,fy: Integer;  //ну да, величина перемещения
  public
    constructor Create(Owner: TComponent); override;
    constructor New(aObjects: TGraphicObjectGroup;x,y: Integer; aImageIndex: Integer=-1); overload;
    constructor New(aObject: IGraphicObject;x,y: Integer; aImageIndex: Integer=-1); overload;
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
  published
    property dX: Integer read fx write fx default 0;
    property dY: Integer read fy write fy default 0;
end;

TResizeGraphicCommand=class(TGraphicCommands)
  private
    fx1,fx2,fy1,fy2: Integer;
    fMirrorX,fMirrorY: Boolean;
    fCorrection: TGraphicResizeCorrector;
  public
    constructor Create(Owner: TComponent); override;
    constructor New(aObjects: TGraphicObjectGroup;ax1,ay1,ax2,ay2: Integer; aImageIndex: Integer=-1);
    destructor Destroy; override;
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
  published
    property x1: Integer read fx1 write fx1 default 0;
    property y1: Integer read fy1 write fy1 default 0;
    property x2: Integer read fx2 write fx2 default 0;
    property y2: Integer read fy2 write fy2 default 0;
    property MirrorX: boolean read fMirrorX write fMirrorX stored fActiveBranch default false;
    property MirrorY: boolean read fMirrorY write fMirrorY stored fActiveBranch default false;
    property Correction: TGraphicResizeCorrector read fCorrection write fCorrection;
end;

TRotateGraphicCommand=class(TGraphicCommands)
  private
    falpha: Real;
    fXcenter,fYcenter: Integer;
    fCorrection: TGraphicResizeCorrector;
  public
    constructor Create(Owner: TComponent); override;
    constructor New(aObjects: TGraphicObjectGroup; aAlpha: Real; aXCenter,aYCenter: Integer; aImageIndex: Integer=-1);
    destructor Destroy; override;
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
  published
    property alpha: Real read falpha write falpha;
    property Xcenter: Integer read fXcenter write fXcenter;
    property Ycenter: Integer read fYcenter write fYcenter;
    property Correction: TGraphicResizeCorrector read fCorrection write fCorrection;
  end;

TGroupGraphicCommand=class(TGraphicCommands)
//объединяет компоненты в группу
//когда создаем команду, указываем компоненты,
//а когда она уже создана - даем ссылочку на группу
  public
    constructor Create(Owner: TComponent); override;  //нарисовать иконку
    constructor New(aGroup: TGraphicObjectGroup); virtual;
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
end;

TUngroupGraphicCommand=class(TGroupGraphicCommand)
  public
    constructor Create(Owner: TComponent); override;
    constructor New(aGroup: TGraphicObjectGroup); override;
    function Execute: boolean; override;
    function Undo: boolean; override;
    function Caption: string; override;
end;

procedure ResizeRect(var aRect: TRect;dx1,dy1,dx2,dy2: Integer);

implementation

uses sysutils,streaming_class_lib,bitstream_lib,math;

procedure ResizeRect(var aRect: TRect; dx1,dy1,dx2,dy2: Integer);
begin
  aRect.Left:=aRect.Left+dx1;
  aRect.Right:=aRect.Right+dx2;
  aRect.Top:=aRect.Top+dy1;
  aRect.Bottom:=aRect.Bottom+dy2;
end;

(*
          TGraphicResizeCorrector
                                          *)
function TGraphicResizeCorrector.NonTrivial: boolean;
var i: Integer;
begin
  Result:=false;
  for i:=0 to fCount-1 do
    if (fstack[i].Left<>0) or (fstack[i].Right<>0) or (fstack[i].Top<>0) or
      (fstack[i].Bottom<>0) then begin
      Result:=true;
      Exit;
    end;
end;

procedure TGraphicResizeCorrector.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('data',ReadData,WriteData,nonTrivial);
end;

procedure TGraphicResizeCorrector.WriteData(stream: TStream);
var minval,maxval,i: Integer;
    bitcount: Byte;
    bitstr: TWriteBitStream;
begin
  minval:=0;
  maxval:=0;
  for i:=0 to fcount-1 do begin
    maxval:=max(maxval,fstack[i].Left);
    maxval:=max(maxval,fstack[i].Right);
    maxval:=max(maxval,fstack[i].Top);
    maxval:=max(maxval,fstack[i].Bottom);
    minval:=min(minval,fstack[i].Left);
    minval:=min(minval,fstack[i].Right);
    minval:=min(minval,fstack[i].Top);
    minval:=min(minval,fstack[i].Bottom);
  end;
  i:=1;
  bitcount:=1;
  while (maxval>i-1) or (minval<-i) do begin
    i:=i shl 1;
    inc(bitcount);
  end;
  bitstr:=TWriteBitStream.Create(stream);
  bitstr.WriteBits(bitcount-1,5); //чтобы был от 0 до 31, а не 1..32 и уместился в 5 бит
  for i:=0 to fcount-1 do begin
    bitstr.WriteBits(fStack[i].Left,bitcount);
    bitstr.WriteBits(fStack[i].Right,bitcount);
    bitstr.WriteBits(fStack[i].Top,bitcount);
    bitstr.WriteBits(fStack[i].Bottom,bitcount);
  end;
  bitstr.Free;
end;

procedure TGraphicResizeCorrector.ReadData(stream: TStream);
var bitcount: Cardinal;
  actread: Cardinal;
  infRect: TRect;
  bitstr: TReadBitStream;
begin
  bitstr:=TReadBitStream.Create(stream);
  bitstr.ReadBitsUnsigned(bitCount,5);
  inc(bitCount); //0 соотв 1 биту, а 31 - 32 битам
  while true do begin
    bitstr.ReadBitsSigned(infRect.Left,bitcount);
    bitstr.ReadBitsSigned(infRect.Right,bitcount);
    bitstr.ReadBitsSigned(infRect.Top,bitcount);
    actread:=bitstr.ReadBitsSigned(infRect.Bottom,bitcount);
    if actread=bitcount then Push(infRect)
    else break;
  end;
  bitstr.Free;
end;

procedure TGraphicResizeCorrector.Push(Rect: TRect);
begin
  inc(fCount);
  if fCount>Length(fstack) then SetLength(fstack,2*fCount);
  fstack[fCount-1]:=Rect;
end;

function TGraphicResizeCorrector.Pop: TRect;
begin
  if fPopCount<fcount then begin
    Result:=fstack[fPopcount];
    inc(fPopcount);
  end
  else Result:=Rect(0,0,0,0);
end;

procedure TGraphicResizeCorrector.Clear;
begin
  SetLength(fstack,0);
  fCount:=0;
  fPopCount:=0;
end;

procedure TGraphicResizeCorrector.SetReverse(value: Boolean);
begin
  fReverse:=value;
  if not fReverse then Clear;
end;


(*
      TGraphicObjectGroup
                            *)
function TGraphicObjectGroup.IItem(index: Integer): IGraphicObject;
begin
  Result:=Item[index] as IGraphicObject;
  Assert(Result<>nil,'wtf');
end;

function TGraphicObjectGroup.Rect: TRect;
var i: Integer;
    tmpRect: TRect;
begin
  assert(Count>0,'TGraphicObjectGroup.Rect: empty group');
  Result:=Iitem(0).Rect;
  With Result do begin
    for i:=1 to Count-1 do begin
      tmpRect:=Iitem(i).Rect;
      Left:=min(Left,tmpRect.Left);
      Right:=max(Right,tmpRect.Right);
      Top:=min(Top,tmpRect.Top);
      Bottom:=max(Bottom,tmpRect.Bottom);
    end;
  end;
end;

function TGraphicObjectGroup.SymbolRect: TRect;
var i: Integer;
    tmpRect: TRect;
begin
  assert(Count>0,'TGraphicObjectGroup.Rect: empty group');
  Result:=Iitem(0).SymbolRect;
  With Result do begin
    for i:=1 to Count-1 do begin
      tmpRect:=Iitem(i).SymbolRect;
      Left:=min(Left,tmpRect.Left);
      Right:=max(Right,tmpRect.Right);
      Top:=min(Top,tmpRect.Top);
      Bottom:=max(Bottom,tmpRect.Bottom);
    end;
  end;
end;

function TGraphicObjectGroup.DistanceTo(ax,ay: Integer): Real;
var i: Integer;
begin
  Result:=Iitem(0).DistanceTo(ax,ay);
  for i:=1 to Count-1 do
    Result:=min(Result,Iitem(i).DistanceTo(ax,ay));
end;

procedure TGraphicObjectGroup.Move(dx,dy: Integer);
var i: Integer;
begin
  for i:=0 to Count-1 do
    Iitem(i).Move(dx,dy);
end;

function TGraphicObjectGroup.ResizableByX: boolean;
begin
  Result:=(Count>0) and ((Count=1) and (Iitem(0).ResizableByX)) or ((Count>1) and (Rect.Left<>Rect.Right))
end;

function TGraphicObjectGroup.ResizableByY: boolean;
begin
  Result:=(Count>0) and ((Count=1) and (Iitem(0).ResizableByY)) or ((Count>1) and (Rect.Top<>Rect.Bottom))
end;

procedure TGraphicObjectGroup.Resize(dx1,dy1,dx2,dy2: Integer;corrector: TGraphicResizeCorrector=nil);
var i: Integer;
    obj: IGraphicObject;
    buRect,tmpRect: TRect;
    bubu: TRect;
    buxcent,buycent,buxlen,buylen: Real;
    cmx,difx,cmy,dify: Real;
    xcent,ycent,xlen,ylen: Real;
    infRect: TRect;
    mirrorX,mirrorY: boolean;
    bdx1,bdx2,bdy1,bdy2: Integer;
begin
  buxlen:=0;
  buxcent:=0;
  buylen:=0;
  buycent:=0; //чтобы компилятор не ругался на неинициал. переменные
  mirrorX:=false;
  mirrorY:=false;
  if Count>0 then begin
    buRect:=Rect; //далее, по мере того как объекты будут двигаться, Rect изменится
    //этот самый Rect мы не знаем при undo - т.е он уже совсем другой
    cmx:=(dx1+dx2)/2.0;
    difx:=(dx2-dx1)/2.0;
    cmy:=(dy1+dy2)/2.0;
    dify:=(dy2-dy1)/2.0;
    xcent:=(buRect.Left+buRect.Right)/2.0;
    ycent:=(buRect.Top+buRect.Bottom)/2.0;
    xlen:=(buRect.Right-buRect.Left)/2.0;
    ylen:=(buRect.Bottom-buRect.Top)/2.0;

    if Assigned(corrector) and not corrector.reverse then begin
      //тяжело в учении - нужно сразу же провернуть обратную операцию
      //и занести в corrector получившуюся разницу
      bubu:=buRect;
      ResizeRect(bubu,dx1,dy1,dx2,dy2);
      mirrorX:=(bubu.Left>bubu.Right);
//      if mirrorX then SwapIntegers(bubu.Left,bubu.Right);
      mirrorY:=(bubu.Top>bubu.Bottom);
//      if mirrorY then SwapIntegers(bubu.Top,bubu.Bottom);

      buxcent:=(bubu.Left+bubu.Right)/2.0;
      buycent:=(bubu.Top+bubu.Bottom)/2.0;
      buxlen:=(bubu.Right-bubu.Left)/2.0;
      buylen:=(bubu.Bottom-bubu.Top)/2.0;
    end;

    for i:=0 to count-1 do begin
      obj:=Iitem(i);
      tmpRect:=obj.Rect;
      if xlen=0 then begin
        dx1:=Round(cmx);
        dx2:=dx1;
      end
      else begin
        dx1:=Round(difx*(tmpRect.Left-xcent)/xlen+cmx);
        dx2:=Round(difx*(tmpRect.Right-xcent)/xlen+cmx);
      end;
      if ylen=0 then begin
        dy1:=Round(cmy);
        dy2:=dy1;
      end
      else begin
        dy1:=Round(dify*(tmpRect.Top-ycent)/ylen+cmy);
        dy2:=Round(dify*(tmpRect.Bottom-ycent)/ylen+cmy);
      end;
      //вот, сейчас еще есть все для обращения процедуры
      if Assigned(Corrector) then
        if corrector.reverse then begin
          infRect:=corrector.Pop; //легко в бою
          dx1:=dx1-infRect.Left;
          dx2:=dx2-infRect.Right;
          dy1:=dy1-infRect.Top;
          dy2:=dy2-infRect.Bottom;
        end
        else begin
          //формируем поправки для undo
          //tmpRect больше не нужен, можно его помучать
          ResizeRect(tmpRect,dx1,dy1,dx2,dy2);
          bdx1:=dx1;
          bdy1:=dy1;
          bdx2:=dx2;
          bdy2:=dy2;
          if MirrorX and (tmpRect.Left<>tmpRect.Right) then begin
            SwapIntegers(tmpRect.Left,tmpRect.Right);
            SwapIntegers(bdx1,bdx2);
          end;
          if MirrorY and (tmpRect.Top<>tmpRect.Bottom) then begin
            SwapIntegers(tmpRect.Top,tmpRect.Bottom);
            SwapIntegers(bdy1,bdy2);
          end;
          if buxlen=0 then begin
            infRect.Left:=Round(-cmx);
            infRect.Right:=infRect.Left;
          end
          else begin
            infRect.Left:=Round(-difx*(tmpRect.Left-buxcent)/buxlen-cmx);
            infRect.Right:=Round(-difx*(tmpRect.Right-buxcent)/buxlen-cmx);
          end;
          if buylen=0 then begin
            infRect.Top:=Round(-cmy);
            infRect.Bottom:=infRect.Top;
          end
          else begin
            infRect.Top:=Round(-dify*(tmpRect.Top-buycent)/buylen-cmy);
            infRect.Bottom:=Round(-dify*(tmpRect.Bottom-buycent)/buylen-cmy);
          end;
          ResizeRect(infRect,bdx1,bdy1,bdx2,bdy2);
          corrector.Push(infRect);
        end;
      obj.Resize(dx1,dy1,dx2,dy2,corrector);
    end;
  end;
end;

procedure TGraphicObjectGroup.Rotate(alpha: Real; XCenter,YCenter: Integer; corrector: TGraphicResizeCorrector);
var i: Integer;
begin
  for i:=0 to count-1 do
    IItem(i).Rotate(alpha,XCenter,YCenter,corrector);
end;

procedure TGraphicObjectGroup.SetSize(x1,y1,x2,y2: Integer);
var buRect: TRect;
begin
  if Count>0 then begin
    buRect:=Rect;
    Resize(x1-buRect.Left,y1-buRect.Top,x2-buRect.Right,y2-buRect.Bottom);
  end;
end;

function TGraphicObjectGroup.isOkToPaste: boolean;
var i: Integer;
    intf: IGraphicObject;
begin
//сейчас он кроваво расправится с теми, кого копировать не стоит
  i:=0;
  while i<Count do begin
    intf:=IItem(i);
    if not intf.isOkToPaste then begin
      intf:=nil;  //счетчик ссылок, что б его!
      Delete(Item[i]);
    end
    else
      inc(i);
  end;
  Result:=(Count>0);
end;

function TGraphicObjectGroup.Implementor: TComponent;
begin
  Result:=self;
end;

procedure TGraphicObjectGroup.Draw(canvas: TCanvas);
var i: Integer;
begin
  for i:=0 to Count-1 do
    Iitem(i).Draw(Canvas);
end;

procedure TGraphicObjectGroup.RegisterProperties(proc: RegisterPropertyProc);
begin

end;

procedure TGraphicObjectGroup.AddTitleAndHint(proc: AddTitleAndHintProc);
begin

end;

procedure TGraphicObjectGroup.UnregisterProperties(proc: UnregisterPropertyProc);
begin
  proc('Tag');
  proc('OwnsObjects');
  proc('UseNotifications');
end;

(*
        TGraphicCommands
                                  *)
procedure TGraphicCommands.ResolveMemory;
begin
  if Assigned(Owner) then
    (Owner as TCommandTree).JumpToBranch(self);
  fObjectNames:=fObjects.NamesAsString;
end;


(*
        TAddGraphicCommand
                                  *)
constructor TAddGraphicCommand.Create(owner: TComponent);
begin
  inherited Create(owner);
  fObjects:=TGraphicObjectGroup.Create(self);
  fObjects.SetSubComponent(true);
end;

procedure TAddGraphicCommand.SetClipbd(value: boolean);
begin
  fFromClipbd:=value;
end;

constructor TAddGraphicCommand.NewGr(aObjects: TGraphicObjectGroup; paste: Boolean=false; aImageIndex: Integer=-1);
var i: Integer;
begin
//нам передают только что созданную группу объектов, которой эти объекты принадлежат
  Create(nil);
  fObjects.Free;
  fObjects:=aObjects;
  fFromClipbd:=paste;
  fImageIndex:=aImageIndex;
  if Assigned(fObjects.Owner) then
    fObjects.Owner.RemoveComponent(fObjects);
  InsertComponent(fObjects);
  fObjects.SetSubComponent(true);
  fObjects.OwnsObjects:=false;
  for i:=0 to fObjects.Count-1 do begin
    if Assigned(fObjects.Item[i].Owner) then
      fObjects.Item[i].Owner.RemoveComponent(fObjects.Item[i]);
    InsertComponent(fObjects.Item[i]);
  end;
  fObjectNames:=fObjects.NamesAsString;
end;

constructor TAddGraphicCommand.NewObj(aObject: IGraphicObject; paste: Boolean=false; aImageIndex: Integer=-1);
begin
  Create(nil);
  fObjects.Add(aObject.Implementor);
  if Assigned(aObject.Implementor.Owner) then
    aObject.Implementor.Owner.RemoveComponent(aObject.Implementor);
  InsertComponent(aObject.Implementor);
  fFromClipbd:=paste;
  fImageIndex:=aImageIndex;
  fObjectNames:=fObjects.NamesAsString;
end;

function TAddGraphicCommand.Execute: Boolean;
var doc: TAbstractDocument;
    i: Integer;
    comp: TComponent;
begin
  //добавляем вектор, указанный выше
  doc:=FindOwner as TAbstractDocument; //выдаст ошибку, если что-то не то
  //иначе бы замолчал, а оно нам не надо, хрен потом разберешься
  for i:=0 to fObjects.Count-1 do begin
    comp:=fObjects.Item[i];
    RemoveComponent(comp);
    doc.InsertComponent(comp);
//    fObjects.Add(comp);
// нам пришлось ее поставить из-за глючного Notification, без него все проще
  end;
  Result:=true;
end;

function TAddGraphicCommand.Undo: Boolean;
var doc: TAbstractDocument;
    i: Integer;
begin
  //нужно переместить вектор в безопасное место - себе под крылышко
  doc:=FindOwner as TAbstractDocument;
  for i:=0 to fObjects.Count-1 do begin
    doc.RemoveComponent(fObjects.Item[i]);
    InsertComponent(fObjects.Item[i]);
  end;
  Result:=true;
end;

function TAddGraphicCommand.Caption: string;
resourcestring
  AddCommandPasteStr = 'Вставить %s';
  AddCommandAddStr = 'Построить %s';
begin
  if FromClipbd then
    Result:=Format(AddCommandPasteStr,[fObjectNames])
  else
    Result:=Format(AddCommandAddStr,[fObjectNames]);
end;

(*
          TBFGDeleteCommand
                                            *)
constructor TDeleteGraphicCommand.Create(owner: TComponent);
begin
  inherited Create(owner);
  fImageIndex:=10;
end;

procedure TDeleteGraphicCommand.SetClipbd(value: boolean);
begin
  inherited SetClipbd(value);
  if value then fImageIndex:=14 else fImageIndex:=10;
end;

constructor TDeleteGraphicCommand.New(aObjects: TGraphicObjectGroup; cut: Boolean=false; aImageIndex: Integer=-1);
begin
  Create(nil);
  FromClipbd:=cut;
  fImageIndex:=aImageIndex;
  fObjects.Assign(aObjects);
  ResolveMemory;
  //не порем горячку, пусть у нас пока сидит только ссылочка и ничего больше!
end;

function TDeleteGraphicCommand.Execute: boolean;
begin
  Result:=inherited Undo;
end;

function TDeleteGraphicCommand.Undo: boolean;
begin
  Result:=inherited Execute;
end;

function TDeleteGraphicCommand.Caption: string;
resourcestring
  DeleteCommandCutStr='Вырезать %s';
  DeleteCommandRemoveStr='Удалить %s';
begin
  if FromClipbd then
    Result:=Format(DeleteCommandCutStr,[fObjectNames])
  else
    Result:=Format(DeleteCommandRemoveStr,[fObjectNames]);
end;


(*
        TBFGMoveCommand
                                        *)

constructor TMoveGraphicCommand.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fImageIndex:=13;
  fObjects:=TGraphicOBjectGroup.Create(self);
  fObjects.SetSubComponent(true);
end;

constructor TMoveGraphicCommand.New(aObjects: TGraphicObjectGroup; x,y: Integer; aImageIndex: Integer=-1);
begin
  Create(nil);
  fImageIndex:=aImageIndex;
  fObjects.Assign(aObjects);
  fObjectNames:=fObjects.NamesAsString;
  fx:=x;
  fy:=y;
end;

constructor TMoveGraphicCommand.New(aObject: IGraphicObject; x,y: Integer; aImageIndex: Integer=-1);
begin
  Create(nil);
  fImageIndex:=aImageIndex;
  fObjects.Add(aObject.Implementor);
  fObjectNames:=fObjects.NamesAsString;
  fx:=x;
  fy:=y;
end;

function TMoveGraphicCommand.Execute: Boolean;
begin
  Result:=(fx<>0) or (fy<>0);
  if Result then begin
    fObjects.Move(fx,fy);
  end;
end;

function TMoveGraphicCommand.Undo: Boolean;
begin
  fObjects.Move(-fx,-fy);
  Result:=true;
end;

function TMoveGraphicCommand.Caption: string;
resourcestring
  MoveCommandCaption = 'Перемещение %s на (%d;%d)';
begin
  Result:=Format(MoveCommandCaption,[fObjectNames,fx,fy]);
end;

(*
          TBFGResizeCommand
                                            *)
constructor TResizeGraphicCommand.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fObjects:=TGraphicObjectGroup.Create(self);
  fObjects.SetSubComponent(true);
  fCorrection:=TGraphicResizeCorrector.Create;
end;

destructor TResizeGraphicCommand.Destroy;
begin
  fCorrection.Free;
  inherited Destroy;
end;

constructor TResizeGraphicCommand.New(aObjects: TGraphicObjectGroup;ax1,ay1,ax2,ay2: Integer; aImageIndex: Integer=-1);
begin
  Create(nil);
  fObjects.Assign(aObjects);
  fObjectNames:=fObjects.NamesAsString;
  fImageIndex:=aImageIndex;
  fx1:=ax1;
  fx2:=ax2;
  fy1:=ay1;
  fy2:=ay2;
end;

function TResizeGraphicCommand.Execute: Boolean;
var tmpRect: TRect;
begin
  Result:=(fx1<>0) or (fy1<>0) or (fx2<>0) or (fy2<>0);
  if Result then begin
    fCorrection.reverse:=false;
    tmpRect:=fObjects.Rect;
    fMirrorX:=(fx2-fx1)<(tmpRect.Left-tmpRect.Right);
    fMirrorY:=(fy2-fy1)<(tmpRect.Top-tmpRect.Bottom);
    fObjects.Resize(fx1,fy1,fx2,fy2,fCorrection);
  end;
end;

function TResizeGraphicCommand.Undo: Boolean;
var dx1,dy1,dx2,dy2: Integer;
begin
  fCorrection.reverse:=true;
  dx1:=fx1; dx2:=fx2; dy1:=fy1; dy2:=fy2;
  if fMirrorX then SwapIntegers(dx1,dx2);
  if fMirrorY then SwapIntegers(dy1,dy2);
  fObjects.Resize(-dx1,-dy1,-dx2,-dy2,fCorrection);
  fCorrection.Clear;
  Result:=true;
end;

function TResizeGraphicCommand.Caption: string;
resourcestring
  ResizeCommandCaption = 'Изменить размеры %s на (%d;%d;%d;%d)';
begin
  Result:=Format(ResizeCommandCaption,[fObjectNames,fx1,fy1,fx2,fy2]);
end;

(*
      TBFGGroupCommand
                              *)
constructor TGroupGraphicCommand.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fImageIndex:=21;
end;

constructor TGroupGraphicCommand.New(aGroup: TGraphicObjectGroup);
begin
  Create(nil);
  fObjects:=aGroup;
  fObjectNames:=aGroup.NamesAsString;
  if Assigned(fObjects.Owner) then fObjects.Owner.RemoveComponent(fObjects);
  InsertComponent(fObjects);
end;

function TGroupGraphicCommand.Execute: boolean;
var doc: TAbstractDocument;
begin
  //сейчас перед нами группа объектов, но объекты ей не принадлежат
  //надо сделать спец. процедуру, видимо
  RemoveComponent(fObjects);
  doc:=FindOwner as TAbstractDocument;
  doc.InsertComponent(fObjects);
  fObjects.OwnsObjects:=true;
  Result:=true;
end;

function TGroupGraphicCommand.Undo: Boolean;
var doc: TAbstractDocument;
begin
  doc:=FindOwner as TAbstractDocument;
  doc.RemoveComponent(fObjects);
  InsertComponent(fObjects);
  fObjects.SetObjectsOwner(doc);
  Result:=true;
end;

function TGroupGraphicCommand.Caption: string;
resourcestring
  GroupCommandCaption = 'Сгруппировать %s';
begin
  Result:=Format(GroupCommandCaption,[fObjectNames]);
end;

(*
    TBFGUngroupCommand
                            *)
constructor TUngroupGraphicCommand.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fImageIndex:=22;
end;

constructor TUngroupGraphicCommand.New(aGroup: TGraphicObjectGroup);
begin
  Create(nil);
  //aGroup - ссылка на имеющуюся группу, которой принадлеж. объекты
  fObjects:=aGroup;
  fObjectNames:=fObjects.NamesAsString;
end;

function TUngroupGraphicCommand.Execute: boolean;
begin
  Result:=inherited Undo;
end;

function TUngroupGraphicCommand.Undo: boolean;
begin
  Result:=inherited Execute;
end;

function TUngroupGraphicCommand.Caption: string;
resourcestring
  UngroupCommandCaption = 'Разгруппировать %s';
begin
  Result:=Format(UngroupCommandCaption,[fObjectNames]);
end;

(*
    TBFGRotateCommand
                          *)
constructor TRotateGraphicCommand.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fObjects:=TGraphicObjectGroup.Create(self);
  fObjects.SetSubComponent(true);
  fCorrection:=TGraphicResizeCorrector.Create;
end;

destructor TRotateGraphicCommand.Destroy;
begin
//  fObjects.Free;
  fCorrection.Free;
  inherited Destroy;
end;

constructor TRotateGraphicCommand.New(aObjects: TGraphicObjectGroup; aalpha: Real; aXcenter,aYcenter: Integer; aImageIndex: Integer=-1);
begin
  Create(nil);
  falpha:=aalpha;
  fXcenter:=aXcenter;
  fYcenter:=aYcenter;
  fObjects.Assign(aObjects);
  fObjectNames:=fObjects.NamesAsString;
  fImageIndex:=aImageIndex;
end;

function TRotateGraphicCommand.Execute: boolean;
begin
  Result:=(alpha<>0);
  if Result then begin
    fCorrection.reverse:=false;
    fObjects.Rotate(alpha,Xcenter,YCenter,fcorrection);
  end;
end;

function TRotateGraphicCommand.Undo: Boolean;
begin
  fCorrection.reverse:=true;
  fObjects.Rotate(-alpha,Xcenter,Ycenter,fcorrection);
  Result:=true;
end;

function TRotateGraphicCommand.Caption: string;
var deg: Real;
resourcestring
  formatstr = 'Повернуть %s на %5.2f градусов';
begin
  deg:=alpha*180/pi;
  Result:=Format(formatstr,[fObjectNames,deg]);
end;


(*
      TDocumentWithImage
                            *)
constructor TDocumentWithImage.Create(owner: TComponent);
begin
  inherited Create(owner);
  fGraphicObjectsIterator:=TAbstractDocumentInterfaceIterator.Create(self,IGraphicObject);
end;

function TDocumentWithImage.XPix2Val(X: Integer): Integer;
begin
  Result:=X;
end;

function TDocumentWithImage.YPix2Val(Y: Integer): Integer;
begin
  Result:=Y;
end;

function TDocumentWithImage.XVal2Pix(X: Integer): Integer;
begin
  Result:=X;
end;

function TDocumentWithImage.YVal2Pix(Y: Integer): Integer;
begin
  Result:=Y;
end;

function TDocumentWithImage.Get_square_size: Integer;
begin
  Result:=10;
end;

function TDocumentWithImage.Get_sensitivity: Integer;
begin
  Result:=15;
end;

function TDocumentWithImage.Get_duplicate_shift_x: Integer;
begin
  Result:=25;
end;

function TDocumentWithImage.Get_duplicate_shift_y: Integer;
begin
  Result:=25;
end;

function TDocumentWithImage.Get_scale: Real;
begin
  Result:=1;
end;


initialization
  RegisterClasses([TAddGraphicCommand,TDeleteGraphicCommand,TMoveGraphicCommand,
  TResizeGraphicCommand,TGroupGraphicCommand,TUngroupGraphicCommand,
  TRotateGraphicCommand,TGraphicObjectGroup]);
end.
