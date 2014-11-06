unit IGraphicObject_tools;

interface

uses Controls,Classes,types,command_class_lib,messages,IGraphicObject_commands,
  strUtils,observer_pattern_interfaces,typInfo,stdCtrls,dialogs,cautious_edit;

type

TGraphicTools=class(TAbstractToolAction)
protected
  function doc: TDocumentWithImage;
end;

TCustomAddLineTool=class(TGraphicTools)
protected
  fMouseDown: boolean;
  fInitX,fInitY: Integer;
  fCurX,fCurY: Integer;
  procedure InverseLine;
public
  procedure Assign(source: TPersistent); override;
  function Select: boolean; override;
  procedure Unselect; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
end;

TPickToolState=(sDeselect,sSizeTopLeft,sSizeTopRight,sSizeBottomLeft,
sSizeBottomRight,sSizeTop,sSizeBottom,sSizeLeft,sSizeRight,sMove,sRotate);
TGraphicPickTool=class(TGraphicTools,IObservable)
private
  fPickedGroup: TGraphicObjectGroup;
  fPwndSelection: TGraphicObjectGroup;
  fNotifier: TObservableImplementor;
  fMouseDown: Boolean;
  fIsRightButton: Boolean;
  fstate: TPickToolState;
  offset_x,offset_y: Integer; //смещение мыши отн. края вектора
  coords: array [0..1,0..1,0..1] of Integer;
  //1-я коорд - лево-право
  //2-я - x/y
  //3-я - сейчас/раньше
  symbolic: array [0..1,0..1,0..1] of Integer;  //для отображения на экране
  center: array [0..1] of Integer;
  fAngle: Real;
  fStartX,fstartY: Integer; //начало рамочки
  fCurX,fCurY: Integer; //второй угол

  procedure DrawSelection;
  procedure DeleteChosen(cut: boolean=false);
  procedure CopyChosen;
  procedure FindSelRect;
  procedure ShowSquares;
  procedure SetState(value: TPickToolState);
  procedure ProcessCorner(x,y: Integer; Shift: TShiftState; i,j: Integer);
  procedure ProcessEdge(x: Integer; Shift: TShiftState; i,j: Integer);
  property state: TPickToolState read fstate write SetState;
public
  constructor Create(owner: TComponent); override;
  destructor Destroy; override;
  procedure Assign(source: TPersistent); override;
  function Select: boolean; override;
  procedure Unselect; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  property Notifier: TObservableImplementor read fNotifier implements IObservable;
published
  property PickedGroup: TGraphicObjectGroup read fPickedGroup write fPickedGroup;
end;

TProcessSelectedButton=class(TAbstractDocumentAction,IObserver)
private
  registered: boolean;
public
  procedure Notification(aComponent: TComponent;operation: TOperation); override;
  function doc: TAbstractDocument;
  function pickTool: TGraphicPickTool;
  function update: boolean; override; //будем делать кнопку серой, если ничего не выделено
  procedure ObserverUpdate; virtual;
end;

TDeleteButtonAction=class(TProcessSelectedButton)
public
  constructor Create(owner: TComponent); override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TCutButtonAction=class(TProcessSelectedButton)
public
  constructor Create(owner: TComponent); override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TCopyButtonAction=class(TProcessSelectedButton)
public
  constructor Create(owner: TComponent); override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TGroupButtonAction=class(TProcessSelectedButton)
public
  constructor Create(owner: TComponent); override;
  function update: boolean; override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TUngroupButtonAction=class(TProcessSelectedButton)
public
  constructor Create(owner: TComponent); override;
  function update: boolean; override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TPropertyEditor=class
  protected
    function DrawLabel(aowner: TWinControl;left,top: Integer; aPropInfo: TAdvPropInfo): TLabel;
    function AddCautiousEditor(aowner: TWinControl; EditorClass: TCautiousEditClass; left,top: Integer; aPropInfo: TAdvPropInfo): TCautiousEdit;
  public
    procedure AddEditor(aowner: TWinControl;left,top: Integer;out right,bottom: Integer;aPropInfo: TAdvPropInfo); virtual; abstract;
end;

TIntegerPropertyEditor=class(TPropertyEditor)
  public
    procedure fChangeProc(Sender: TObject);
    procedure AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo); override;
  end;
TColorPropertyEditor=class(TPropertyEditor)
  private
//    fColorDlg: TColorDialog;
  public
//    constructor Create;
//    destructor Destroy; override;
    procedure fChangeProc(Sender: TObject);
//    procedure fClickProc(Sender: TObject);
    procedure AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo); override;
  end;

TBooleanPropertyEditor=class(TPropertyEditor)
  public
    procedure fChangeProc(Sender: TObject);
    procedure AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo); override;
  end;

TFloatPropertyEditor=class(TPropertyEditor)
  public
    procedure fChangeProc(Sender: TObject);
    procedure AddEditor(aowner: TWinControl; left,top: Integer; out right,bottom: Integer; aPropInfo: TAdvPropInfo); override;
  end;

TStringPropertyEditor=class(TPropertyEditor)
  public
    procedure fChangeProc(Sender: TObject);
    procedure fValidateProc(Sender: TObject); virtual;
    procedure AddEditor(aowner: TWinControl; left,top: Integer; out right,bottom: Integer; aPropInfo: TAdvPropInfo); override;
  end;

TNamePropertyEditor=class(TStringPropertyEditor)
  public
    procedure fvalidateproc(Sender: TObject); override;
  end;

TShowProperties=class(TProcessSelectedButton)
private
  fControl: TWinControl; //куда вывести инфу
  fFullPropList: TStrings; //по кр. мере можно искать по имени
  fPropEditors: TStrings; //редакторы свойств
  fLastObj: TPersistent;
  procedure SetControl(value: TWinControl);  //IDE однако - нужны notification на всяк пожарный
//  procedure RegisterProperty(name,title,hint: string);
  procedure AddTitleAndHint(name,title,hint: string);
  procedure UnregisterProperty(name: string);
public
  constructor Create(Owner: TComponent); override;
  destructor Destroy; override;
  procedure Notification(aComponent: TComponent; operation: TOperation); override;
  procedure ObserverUpdate; override;
  function Update: Boolean; override;
published
  property Control: TWinControl read fControl write SetControl;
end;

TPasteButtonAction=class(TAbstractDocumentAction)
public
  constructor Create(owner: TComponent); override;
  function update: boolean; override;
  procedure ExecuteTarget(Target: TObject); override;
end;

TSelectAllButtonAction=class(TAbstractDocumentAction)
public
  constructor Create(owner: TComponent); override;
  function update: boolean; override;
  procedure ExecuteTarget(Target: TObject); override;
end;

Procedure Register;
Function IsRectInsideRect(small,big: TRect): Boolean;

const //crRotate = crHandPoint;
      crRotate = 5;

implementation

uses actnList,extCtrls,windows,graphics,sysUtils,menus,clipbrd,
  streaming_class_lib,math;

procedure Register;
begin
  RegisterActions('GraphicActions',[TGraphicPickTool,TDeleteButtonAction,
  TCopyButtonAction,TCutButtonAction,TShowProperties,
  TGroupButtonAction,TUngroupButtonAction,TPasteButtonAction,TSelectAllButtonAction],nil);
end;

function IsRectInsideRect(small,big: TRect): Boolean;
begin
  Result:=PtInRect(big,small.TopLeft) and PtInRect(big,small.BottomRight);
end;

(*
    TBFGTools
                    *)
function TGraphicTools.doc: TDocumentWithImage;
begin
  Result:=(owner as TDocumentWithImage);
end;


(*
        TBFGPickTool
                      *)
procedure TGraphicPickTool.DeleteChosen(cut: boolean=false);
begin
  doc.DispatchCommand(TDeleteGraphicCommand.New(fPickedGroup,cut));
end;

procedure TGraphicPickTool.CopyChosen;
var i: Integer;
    s: string;
begin
  for i:=0 to fPickedGroup.Count-1 do
    s:=s+fPickedGroup.Item[i].SaveToString;
  ClipBoard.AsText:=s;
end;

procedure TGraphicPickTool.DrawSelection;
var Canvas: TCanvas;
begin
  Canvas:=doc.get_Image.Canvas;
  with Canvas do begin
    Canvas.Pen.Mode:=pmNotXor;
    Canvas.Pen.Style:=psDot;
    Rectangle(fStartX,fStartY,fCurX,fCurY);
  end;
end;

constructor TGraphicPickTool.Create(owner: TComponent);
begin
  inherited Create(owner);
  GroupIndex:=1;
  ImageIndex:=11;
  AutoCheck:=true;
  Caption:='Выделить';
  Hint:=Caption;
  fPickedGroup:=TGraphicObjectGroup.Create(self);
  fPickedGroup.SetSubComponent(true);
  fPwndSelection:=TGraphicObjectGroup.Create(self);
  fPwndSelection.OwnsObjects:=true;
  fNotifier:=TObservableImplementor.Create;
end;

destructor TGraphicPickTool.Destroy;
begin
//  unselect;
//  fPwndSelection.Free;  //в общем-то сам удалится
  fNotifier.Free;
  inherited Destroy;
end;

procedure TGraphicPickTool.Assign(source: TPersistent);
begin
  if source is TGraphicPickTool then begin
    PickedGroup.Assign(TGraphicPickTool(source).PickedGroup);
  end
  else inherited Assign(source);
end;

function TGraphicPickTool.Select: boolean;
var i: Integer;
  iit: IGraphicObject;
begin
  Result:=true;
  //именно процедура select "берет на себя удар" в плане снять выделение если после undo/redo/jump
  //данного вектора или точки попросту не существует!
  //беда интерфейса в том, что нельзя напрямую выйти на объект
  //точнее можно, но только через задницу

  i:=0;
  while i<fPickedGroup.Count do begin
  //цикл for не годится, поскольку верх. значение вычисляется только 1 раз!
    iit:=fPickedGroup.Iitem(i);
    if iit.implementor.Owner<>doc then
      fPickedGroup.Remove(i)
    else inc(i);
  end;
  //if fPickedGroup.Count=0 then State:=sDeselect;
  state:=sDeselect;


  //по логике вещей, удаляемые объекты должны посылать notification
  //но нет, когда объект извлекается из
  FindSelRect;
  ShowSquares;
  //еще здесь надо бы в строку состояния чего-нибудь умного сообщить
  //но это терпит
  fNotifier.NotifyObservers;  //на всякий случай
end;

procedure TGraphicPickTool.Unselect;
begin
  SetStatusPanel('');
  ShowSquares;  //внутри ShowSquares будет проверка на наличие элем.
  fPickedGroup.Clear;
  fNotifier.NotifyObservers;
end;

procedure TGraphicPickTool.FindSelRect;
var tmpRect: TRect;
begin
  //это координаты для редактирования
  if PickedGroup.Count>0 then begin
    tmpRect:=PickedGroup.Rect;
    coords[0,0,0]:=doc.XVal2Pix(tmpRect.Left);
    coords[1,0,0]:=doc.XVal2Pix(tmpRect.Right);
    coords[0,1,0]:=doc.YVal2Pix(tmpRect.Top);
    coords[1,1,0]:=doc.YVal2Pix(tmpRect.Bottom);
    center[0]:=(coords[0,0,0]+coords[1,0,0]) div 2;
    center[1]:=(coords[0,1,0]+coords[1,1,0]) div 2;
    tmpRect:=PickedGroup.SymbolRect;
    symbolic[0,0,0]:=doc.XVal2Pix(tmpRect.Left);
    symbolic[1,0,0]:=doc.XVal2Pix(tmpRect.Right);
    symbolic[0,1,0]:=doc.YVal2Pix(tmpRect.Top);
    symbolic[1,1,0]:=doc.YVal2Pix(tmpRect.Bottom);
  end;
end;

procedure TGraphicPickTool.ShowSquares;
var canvas: TCanvas;
  s,x3,y3: Integer;
  i,j: Integer;
  empty_rect: TRect;
  si,co: Real;
  Points: array [0..7] of TPoint;
begin
  if (doc=nil) or (PickedGroup.Count=0) then Exit;
  //а вот здесь координаты для отобр.
  s:=doc.get_square_size;
  empty_rect:=Rect(0,0,0,0);
  canvas:=doc.get_Image.Canvas;
  canvas.CopyMode:=cmDstInvert;
  if state=sRotate then j:=1 else j:=0;
  x3:=(symbolic[0,0,j]+symbolic[1,0,j]) div 2;
  y3:=(symbolic[0,1,j]+symbolic[1,1,j]) div 2;
  if not (PickedGroup.ResizableByX xor PickedGroup.ResizableByY) then begin
    points[0]:=Point(symbolic[0,0,j]-s,symbolic[0,1,j]-s);
    points[1]:=Point(symbolic[1,0,j],symbolic[0,1,j]-s);
    points[2]:=Point(symbolic[0,0,j]-s,symbolic[1,1,j]);
    points[3]:=Point(symbolic[1,0,j],symbolic[1,1,j]);
    i:=4;
  end
  else i:=0;
  if PickedGroup.ResizableByY then begin
    points[i]:=Point(x3-s div 2,symbolic[0,1,j]-s);
    points[i+1]:=Point(x3-s div 2,symbolic[1,1,j]);
    inc(i,2);
  end;
  if PickedGroup.ResizableByX then begin
    points[i]:=Point(symbolic[0,0,j]-s,y3-s div 2);
    points[i+1]:=Point(symbolic[1,0,j],y3-s div 2);
    inc(i,2);
  end;

  if state=sRotate then begin
    si:=sin(fAngle);
    co:=cos(fAngle);
    for j:=0 to i-1 do begin
      x3:=points[j].X-center[0];
      y3:=points[j].Y-center[1];
      points[j].X:=center[0]+Round(x3*co-y3*si);
      points[j].Y:=center[1]+Round(x3*si+y3*co);
    end;
  end;
  for j:=0 to i-1 do
    canvas.CopyRect(Rect(Points[j].X,points[j].Y,Points[j].X+s,points[j].Y+s),canvas,empty_rect);
end;

procedure TGraphicPickTool.SetState(value: TPickToolState);
var img: TImage;
    s: string;
resourcestring
  PickToolResizeStr='Изменение размера';
  PickToolRotateStr='Вращение';
  PickToolMoveStr='Перемещение';
  PickToolDeselectStr='Снять выделение';
begin
  img:=doc.get_image;
  fstate:=value;
  s:=PickToolResizeStr;
  with img do begin
    case value of
      sSizeTopLeft,sSizeBottomRight: Cursor:=crSizeNWSE;
      sSizeBottomLeft,sSizeTopRight: Cursor:=crSizeNESW;
      sSizeLeft,sSizeRight: Cursor:=crSizeWE;
      sSizeTop,sSizeBottom: Cursor:=crSizeNS;
      sRotate:
        begin
          Cursor:=crRotate;
          s:=PickToolRotateStr;
        end;
      sMove:
        begin
        Cursor:=crSizeAll;
        s:=PickToolMoveStr;
        end;
      sDeselect:
        begin
        Cursor:=crDefault;
        s:=PickToolDeselectStr;
        end;
    end;
  end;
  SetStatusPanel(s);
end;

procedure TGraphicPickTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var i,j: Integer;
begin
  fMouseDown:=true;
  fIsRightButton:=(Button=mbRight);
  if state=sDeselect then begin
    //начинаем рисовать пунктирную рамочку, чтобы выделить все что в нее попадает
    fStartX:=X;
    fStartY:=Y;
    fCurX:=X;
    fCurY:=Y;
  end
  else begin
    for i:=0 to 1 do
      for j:=0 to 1 do begin
        coords[i,j,1]:=coords[i,j,0];
        symbolic[i,j,1]:=symbolic[i,j,0]; //на всяк. случай
      end;
    fangle:=0;
    //и сейчас создадим копию всех выделенных объектов, чтобы их ПОМУЧАТЬ
    fPwndSelection.TakeFromList(fPickedGroup);
    with doc.get_image.Canvas.pen do begin
      Mode:=pmNotXor;
      Style:=psDot;
    end;
    fPwndSelection.Draw(doc.get_image.Canvas);
  end;
  //выделить можем только при отпускании мыши
end;

procedure TGraphicPickTool.ProcessCorner(x,y: Integer; Shift: TShiftState; i,j: Integer);
var a: Real;
begin
  //i - x-коорд угла, j-y.
  coords[i,0,0]:=x-offset_x;
  coords[j,1,0]:=y-offset_y;
  if ssCtrl in Shift then begin
    a:=((coords[i,0,0]-coords[i,0,1])*(coords[1-i,0,1]-coords[i,0,1])+(coords[j,1,0]-coords[j,1,1])*(coords[1-j,1,1]-coords[j,1,1]))/(Sqr(coords[1-i,0,1]-coords[i,0,1])+Sqr(coords[1-j,1,1]-coords[j,1,1]));
    coords[i,0,0]:=coords[i,0,1]+Round(a*(coords[1-i,0,1]-coords[i,0,1]));
    coords[j,1,0]:=coords[j,1,1]+Round(a*(coords[1-j,1,1]-coords[j,1,1]));
  end;
  if ssShift in Shift then begin
    coords[1-i,0,0]:=center[0]+(center[0]-coords[i,0,0]);
    coords[1-j,1,0]:=center[1]+(center[1]-coords[j,1,0]);
  end
  else begin
    coords[1-i,0,0]:=coords[1-i,0,1];
    coords[1-j,1,0]:=coords[1-j,1,1];
  end;
end;

procedure TGraphicPickTool.ProcessEdge(x: Integer; Shift: TShiftState; i,j: Integer);
var a: Real;
begin
  //x-координата, по которой мы двигаем, это может быть и x, и y
  //offset вычитается при вызове процедуры, это проще
  //i=0 - по горизонтали, 1-по вертикали
  //j=0 - лево/вверх, 1 - вправо/вниз
  coords[j,i,0]:=x;
  if ssShift in Shift then
    coords[1-j,i,0]:=center[i]+(center[i]-coords[j,i,0])
  else
    coords[1-j,i,0]:=coords[1-j,i,1];
  if ssCtrl in Shift then begin
    a:=(coords[j,i,0]-coords[1-j,i,0])/(coords[j,i,1]-coords[1-j,i,1]);
    coords[j,1-i,0]:=center[1-i]+Round(a*(coords[j,1-i,1]-center[1-i]));
    coords[1-j,1-i,0]:=center[1-i]+Round(a*(coords[1-j,1-i,1]-center[1-i]));
  end
  else begin
    coords[0,1-i,0]:=coords[0,1-i,1];
    coords[1,1-i,0]:=coords[1,1-i,1];
  end;
end;

procedure TGraphicPickTool.MouseMove(Shift: TShiftState; X,Y: Integer);
var x1,y1,x2,y2,s: Integer;
    tmpRect: TRect;
begin
  if fMouseDown then begin
    if fIsRightButton then state:=sRotate;
    //двигаем рамочку по хитрым правилам

    //если мы лишь рисовали пункт. рамочку, то нарисуем по одним коорд. 2 раза
    //с инверсией - ничего страшного
    if state<>sDeselect then begin
      ShowSquares; //убираем старую
      fPwndSelection.Draw(doc.get_image.Canvas);
    end;
    case state of
      sSizeTopLeft: ProcessCorner(x,y,Shift,0,0);
      sSizeTopRight: ProcessCorner(x,y,Shift,1,0);
      sSizeBottomLeft: ProcessCorner(x,y,Shift,0,1);
      sSizeBottomRight: ProcessCorner(x,y,Shift,1,1);
      sSizeTop: ProcessEdge(y-offset_y,Shift,1,0);
      sSizeBottom: ProcessEdge(y-offset_y,Shift,1,1);
      sSizeLeft: ProcessEdge(x-offset_x,Shift,0,0);
      sSizeRight: ProcessEdge(x-offset_x,Shift,0,1);
      sMove: begin
        coords[0,0,0]:=coords[0,0,1]+x-offset_x;
        coords[1,0,0]:=coords[1,0,1]+x-offset_x;
        coords[0,1,0]:=coords[0,1,1]+y-offset_y;
        coords[1,1,0]:=coords[1,1,1]+y-offset_y;
      end;
      sDeselect: begin  //рисуем рамочку
        DrawSelection;
        fCurX:=X;
        fCurY:=Y;
        DrawSelection;
      end;
      sRotate: begin
        fangle:=arctan2(Y-center[1],X-center[0]);
        fangle:=fangle-arctan2(offset_y-center[1],offset_x-center[0]);
      end;
    end;
    if state<>sDeselect then begin
      fPwndSelection.TakeFromList(fPickedGroup);
      if state=sRotate then
        fPwndSelection.Rotate(fangle,doc.XPix2Val((coords[0,0,1]+coords[1,0,1]) div 2),doc.YPix2Val((coords[0,1,1]+coords[1,1,1]) div 2))
      else
        fPwndSelection.SetSize(doc.XPix2Val(coords[0,0,0]),doc.YPix2Val(coords[0,1,0]),doc.XPix2Val(coords[1,0,0]),doc.YPix2Val(coords[1,1,0]));

      tmpRect:=fPwndSelection.Rect;
      if coords[0,0,0]>coords[1,0,0] then SwapIntegers(tmpRect.Left,tmpRect.Right);
      if coords[0,1,0]>coords[1,1,0] then SwapIntegers(tmpRect.Top,tmpRect.Bottom);
      coords[0,0,0]:=doc.XVal2Pix(tmpRect.Left);
      coords[0,1,0]:=doc.YVal2Pix(tmpRect.Top);
      coords[1,0,0]:=doc.XVal2Pix(tmpRect.Right);
      coords[1,1,0]:=doc.YVal2Pix(tmpRect.Bottom);

      tmpRect:=fPwndSelection.SymbolRect;
      symbolic[0,0,0]:=doc.XVal2Pix(tmpRect.Left);
      symbolic[0,1,0]:=doc.YVal2Pix(tmpRect.Top);
      symbolic[1,0,0]:=doc.XVal2Pix(tmpRect.Right);
      symbolic[1,1,0]:=doc.YVal2Pix(tmpRect.Bottom);
      ShowSquares;  //рисуем новую
      fPwndSelection.Draw(doc.get_image.Canvas);
    end;
  end
  else if PickedGroup.Count>0 then begin
    FindSelRect;
    s:=doc.get_square_size;
    x1:=symbolic[0,0,0]-s div 2;
    x2:=symbolic[1,0,0]+s div 2;
    y1:=symbolic[0,1,0]-s div 2;
    y2:=symbolic[1,1,0]+s div 2;
//    s:=s*2; //увеличиваем зону чувств
    if (X>x1+s div 2) and (X<x2-s div 2) and (Y>y1+s div 2) and (Y<y2-s div 2) then begin
      state:=sMove;
      offset_x:=x;
      offset_y:=y;
    end
    else begin
      if (abs(X-x1)<=s) and fPickedGroup.ResizableByX then begin
        offset_x:=X-coords[0,0,0];
        if (abs(Y-y1)<=s) and fPickedGroup.ResizableByY then begin
          state:=sSizeTopLeft;
          offset_y:=Y-coords[0,1,0];
        end
        else if (abs(Y-y2)<=s) and fPickedGroup.ResizableByY then begin
          state:=sSizeBottomleft;
          offset_y:=Y-coords[1,1,0];
        end
        else if abs(Y-center[1])<=s then begin
          state:=sSizeLeft;
          offset_y:=Y-center[1];
        end
        else state:=sDeselect;
      end
      else if (abs(X-x2)<=s) and fPickedGroup.ResizableByX then begin
        offset_x:=X-coords[1,0,0];
        if (abs(Y-y1)<=s) and fPickedGroup.ResizableByY then begin
          state:=sSizeTopRight;
          offset_y:=Y-coords[0,1,0];
        end
        else if (abs(Y-y2)<=s) and fPickedGroup.ResizableByY then begin
          state:=sSizeBottomRight;
          offset_y:=Y-coords[1,1,0];
        end
        else if abs(Y-center[1])<=s then begin
          state:=sSizeRight;
          offset_y:=Y-center[1];
        end
        else state:=sDeselect;
      end
      else if (abs(X-center[0])<=s) and fPickedGroup.ResizableByY then begin
        offset_x:=X-center[0];
        if abs(Y-y1)<=s then begin
          state:=sSizeTop;
          offset_y:=Y-coords[0,1,0];
        end
        else if abs(Y-y2)<=s then begin
          state:=sSizeBottom;
          offset_y:=Y-coords[1,1,0];
        end
        else state:=sDeselect;
      end
      else state:=sDeselect;
    end;
    if state=sDeselect then begin
      x1:=x1-s div 2;
      y1:=y1-s div 2;
      x2:=x2+s div 2;
      y2:=y2+s div 2;
      if ((abs(X-x1)<=s) or (abs(X-x2)<=s)) and ((abs(Y-y1)<=s) or (abs(Y-y2)<=s)) then begin
        state:=sRotate;
        offset_x:=X;
        offset_y:=Y;
      end
      else if (X>x1) and (X<x2) and (Y>y1) and (Y<y2) then begin
        state:=sMove;
        offset_x:=x;
        offset_y:=y;
      end;
    end;
  end;
end;

procedure TGraphicPickTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var x1,x2,y1,y2,sens,i,chosen: Integer;
    dist,tmp: Real;
    selRect: TRect;
    intf: IGraphicObject;
    needsNotify: boolean;
begin
  needsNotify:=false;
  fMouseDown:=false;
  sens:=doc.get_sensitivity;
  selRect:=Rect(doc.XPix2Val(fStartX),doc.YPix2Val(fStartY),doc.XPix2Val(fCurX),doc.YPix2Val(fCurY));
  if selRect.Left>selRect.Right then SwapIntegers(selRect.Left,selRect.Right);
  if selRect.Top>selRect.Bottom then SwapIntegers(selRect.Top,selRect.Bottom);
// в данный момент это нормально, что рамка может перевернуться отн. начального положения
  case state of
    sDeselect: begin
      DrawSelection;  //сняли пунктирную рамочку
      ShowSquares;  //сняли 8 квадратиков пред. выделения (если они были)
      if not (ssShift in Shift) then begin
        fPickedGroup.Clear;
        needsNotify:=true;
      end;
      //то ли выбираем объекты в рамочке, то ли ближ. к курсору
      //вполне работающий вариант, но получается - после того, как появилась рамочка выделения, объекты
      //внутри нее выбрать уже не получится (режим не тот)
      if (abs(fStartX-fCurX)<sens) and (abs(fStartY-fCurY)<sens) then begin
        chosen:=-1;
        dist:=sens/doc.get_Scale;
        for i:=0 to doc.ComponentCount-1 do
          if doc.Components[i].GetInterface(IGraphicObject,intf) then begin
            tmp:=intf.DistanceTo(doc.XPix2Val(X),doc.YPix2Val(Y));
            if tmp<dist then begin
              chosen:=i;
              dist:=tmp;
            end;
          end;
        if chosen>=0 then begin
          fPickedGroup.XAdd(doc.Components[chosen]);
          needsNotify:=true;
        end;
      end
      else begin
        for i:=0 to doc.ComponentCount-1 do
          if doc.Components[i].GetInterface(IGraphicObject,intf) and IsRectInsideRect(intf.Rect,selRect) then begin
            fPickedGroup.XAdd(doc.Components[i]);
            needsNotify:=true;
          end;
      end;
      //кое-чего, возможно, добавили, пора перерисовать
      FindSelRect;
      ShowSquares;
    end;
    sMove: begin
      x1:=Round((coords[0,0,0]-coords[0,0,1])/doc.get_Scale);
      y1:=Round((coords[0,1,0]-coords[0,1,1])/doc.get_Scale);
      if not doc.DispatchCommand(TMoveGraphicCommand.New(fPickedGroup,x1,y1)) then
      //если не задалось, то не будет обновления - снова пунктирная рамка. Надо ее убрать
        fPwndSelection.Draw(doc.get_image.Canvas)
      else
        NeedsNotify:=true;
    end;
    sRotate: begin
      selRect:=fPickedGroup.Rect;
      x1:=(selRect.Left+selRect.Right) div 2;
      y1:=(selRect.Bottom+selRect.Top) div 2;
      if not doc.DispatchCommand(TRotateGraphicCommand.New(fPickedGroup,fangle,x1,y1)) then
        fPwndSelection.Draw(doc.get_image.Canvas)
      else
        NeedsNotify:=true;
    end;
    else begin
//      fPwndSelection.saveFormat:=fCyr;
//      fPwndSelection.SaveToFile('group.txt');
    //то или иное изм. размеров
      x1:=Round((coords[0,0,0]-coords[0,0,1])/doc.get_Scale);
      y1:=Round((coords[0,1,0]-coords[0,1,1])/doc.get_Scale);
      x2:=Round((coords[1,0,0]-coords[1,0,1])/doc.get_Scale);
      y2:=Round((coords[1,1,0]-coords[1,1,1])/doc.get_Scale);
      if not doc.DispatchCommand(TResizeGraphicCommand.New(fPickedGroup,x1,y1,x2,y2)) then
        fPwndSelection.Draw(doc.get_image.Canvas)
      else
        NeedsNotify:=true;
    end;
  end;
  if NeedsNotify then
    fNotifier.NotifyObservers;  //на всякий случай
  //нужно перерисовку панели свойств делать только при необходимости, иначе тормозит
end;

(*
      TCustomAddLineTool
                            *)
procedure TCustomAddLineTool.Assign(source: TPersistent);
begin
  if not (source is TCustomAddLineTool) then
    inherited Assign(source);
end;

function TCustomAddLineTool.Select: boolean;
resourcestring
  CustomAddLineToolStr = 'Построение линии';
begin
  SetStatusPanel(CustomAddLineToolStr);
  Result:=true;
end;

procedure TCustomAddLineTool.Unselect;
begin
  SetStatusPanel('');
end;

procedure TCustomAddLineTool.InverseLine;
begin
  doc.get_image.Canvas.MoveTo(fInitX,fInitY);
  doc.get_image.Canvas.LineTo(fCurX,fCurY);
end;

procedure TCustomAddLineTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMouseDown:=true;
  fInitX:=X;
  fInitY:=Y;
  fCurX:=X;
  fCurY:=Y;
  doc.get_image.Canvas.Pen.Mode:=pmNotXor;
  doc.get_image.Canvas.Pen.Color:=clBlack;
end;

procedure TCustomAddLineTool.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if fMouseDown then begin
    InverseLine;
    fCurX:=X;
    fCurY:=Y;
    InverseLine;
  end;
end;

procedure TCustomAddLineTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMouseDown:=false;
  InverseLine;
end;

(*
      TProcessSelectedButton
                                *)
function TProcessSelectedButton.doc: TAbstractDocument;
begin
  Result:=GetDoc as TAbstractDocument;
end;

function TProcessSelectedButton.pickTool: TGraphicPickTool;
begin
  if Assigned(doc.Tool) and (doc.Tool is TGraphicPickTool) then
    Result:=TGraphicPickTool(doc.Tool)
  else Result:=nil;
end;

procedure TProcessSelectedButton.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (aComponent is TGraphicPickTool) and (operation=opRemove) then
    Registered:=false;
  inherited Notification(aComponent,operation);
end;

function TProcessSelectedButton.update: boolean;
begin
  if (not Registered) and (pickTool<>nil) then begin
    pickTool.Notifier.AddObserver(self);  //теперь он сам будет сообщать об изм.
    pickTool.FreeNotification(self);
    Registered:=true;
  end;
  Result:=true;
end;

procedure TProcessSelectedButton.ObserverUpdate;
begin
  enabled:=pickTool.fPickedGroup.Count>0;
end;

(*
      TBFGDeleteButton
                            *)
constructor TDeleteButtonAction.Create(owner: TComponent);
resourcestring
  DeleteButtonCaption = 'Удалить';
  DeleteButtonHint = 'Удалить выбранный объект';
begin
  inherited Create(owner);
//  imageIndex:=11;
  Caption:=DeleteButtonCaption;
  Hint:=DeleteButtonHint;
  ShortCut:=Menus.ShortCut(VK_DELETE,[]);
end;

procedure TDeleteButtonAction.ExecuteTarget(target: TObject);
begin
  PickTool.DeleteChosen;
end;

(*
      TBFGCutButton
                        *)
constructor TCutButtonAction.Create(owner: TCOmponent);
resourcestring
  CutButtonCaption = 'Вырезать';
  CutButtonHint = 'Вырезать выбранный объект';
begin
  inherited Create(owner);
//imageIndex=11;
  Caption:=CutButtonCaption;
  Hint:=CutButtonHint;
  ShortCut:=Menus.ShortCut(Word('X'),[ssCtrl]);
end;

procedure TCutButtonAction.ExecuteTarget(Target: TObject);
begin
  PickTool.CopyChosen;
  PickTool.DeleteChosen(true);
end;

(*
      TBFGCopyButton
                        *)
constructor TCopyButtonAction.Create(owner: TCOmponent);
resourcestring
  CopyButtonCaption = 'Копировать';
  CopyButtonHint = 'Копировать выбранный объект';
begin
  inherited Create(owner);
//imageIndex=11;
  Caption:=CopyButtonCaption;
  Hint:=CopyButtonHint;
  ShortCut:=Menus.ShortCut(Word('C'),[ssCtrl]);
end;

procedure TCopyButtonAction.ExecuteTarget(Target: TObject);
begin
  PickTool.CopyChosen;
end;

(*
      TBFGGroupButton
                            *)
constructor TGroupButtonAction.Create(owner: TComponent);
resourcestring
  GroupButtonCaption = 'Группировать';
  GroupButtonHint = 'Группировать выбранные объекты';
begin
  inherited Create(owner);
  ImageIndex:=21;
  Caption:=GroupButtonCaption;
  Hint:=GroupButtonHint;
  ShortCut:=TextToShortcut('Ctrl+G');
end;

function TGroupButtonAction.update: boolean;
begin
  enabled:=(PickTool<>nil) and (PickTool.fPickedGroup.Count>1);
  Result:=true;
end;

procedure TGroupButtonAction.ExecuteTarget(Target: TObject);
var gr: TGraphicObjectGroup;
begin
  gr:=TGraphicObjectGroup.Create(nil);
  gr.ensureCorrectName('gr',doc);
  gr.TakeFromList(PickTool.PickedGroup);
  doc.DispatchCommand(TGroupGraphicCommand.New(gr));
  //после выполнения команды будет вызвано tool.select, но правильнее теперь
  //думать о них как о группе
  //возможно, gr уже сдох, если команда такая уже была
  gr:=(doc.UndoTree.Current as TGroupGraphicCommand).Objects;
  PickTool.PickedGroup.Add(gr);
  doc.Change;
end;

(*
    TBFGUngroupButton
                              *)
constructor TUngroupButtonAction.Create(owner: TComponent);
resourcestring
  UngroupButtonCaption = 'Разгруппировать';
  UngroupButtonHint = 'Разгруппировать выбранные объекты';
begin
  inherited Create(owner);
  ImageIndex:=22;
  Caption:=UngroupButtonCaption;
  Hint:=UngroupButtonHint;
  ShortCut:=TextToShortcut('Ctrl+Shift+G');
end;

function TUngroupButtonAction.update: Boolean;
begin
  enabled:=(PickTool<>nil) and (PickTool.fPickedGroup.Count=1) and (PickTool.PickedGroup.Item[0] is TGraphicObjectGroup);
  Result:=true;
end;

procedure TUngroupButtonAction.ExecuteTarget(Target: TObject);
var gr: TGraphicObjectGroup;
begin
  gr:=PickTool.PickedGroup.Item[0] as TGraphicObjectGroup;
  doc.DispatchCommand(TUngroupGraphicCommand.New(gr));
  //команда выполнилась, группа "распалась", выделение сбилось
  //надо его вернуть
  PickTool.PickedGroup.Clear;
  PickTool.PickedGroup.TakeFromList(gr);
  doc.Change;
end;

(*
      TPropertyEditor
                              *)
function TPropertyEditor.DrawLabel(aowner: TWinControl;Left,Top: Integer;aPropInfo: TAdvPropInfo): TLabel;
var comp: TComponent absolute Result;
begin
  comp:=aowner.FindComponent(aPropInfo.Name+'label');
  if not (comp is TLabel) then begin
    Result:=TLabel.Create(aowner);
    Result.Name:=aPropInfo.Name+'label';
    Result.Left:=left;
    result.Top:=top;
    if aPropInfo.title<>'' then Result.Caption:=aPropInfo.title
    else Result.Caption:=aPropInfo.Name;
    Result.Hint:=aPropInfo.hint;
    Result.Parent:=aowner;
  end;
end;

function TPropertyEditor.AddCautiousEditor(aowner: TWinControl; EditorClass: TCautiousEditClass; left,top: Integer; apropinfo: TAdvPropInfo): TCautiousEdit;
var comp: TComponent absolute Result;
begin
  comp:=aowner.FindComponent(aPropInfo.Name);
  if not (comp is EditorClass) then begin
    Result:=EditorClass.Create(aowner);
    Result.Name:=aPropInfo.Name;
    Result.Parent:=aowner;
    Result.Left:=left;
    Result.Top:=top;
    Result.ExpressionRootComponent:=aPropInfo.doc;
  end;
  Result.Tag:=Integer(aPropInfo);
end;

(*
      TIntegerPropertyEditor
                                *)
procedure TIntegerPropertyEditor.AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo);
var lab: TLabel;
    intedit: TIntegerEdit;
begin
  lab:=DrawLabel(aowner,left,top+4,aPropInfo);
  if Assigned(aPropInfo.SetProc) then begin
    left:=left+lab.Width+5;

    intedit:=AddCautiousEditor(aowner,TIntegerEdit,Left,Top,aPropInfo) as TIntegerEdit;
    intedit.OnExit:=fChangeProc;
    intedit.value:=GetOrdProp(aPropInfo.instance,aPropInfo.Name);

    left:=left+intedit.Width+5;
  end
  else begin
    //только для чтения
    lab.Caption:=lab.Caption+'='+IntToStr(GetOrdProp(aPropInfo.instance,aPropInfo.Name));
    left:=left+lab.Width+5;
  end;
  right:=left;
  bottom:=top+lab.Height;
end;

procedure TIntegerPropertyEditor.fChangeProc(Sender: TObject);
var edit: TIntegerEdit;
    aProp: TAdvPropInfo;
begin
  edit:=Sender as TIntegerEdit;
  if edit.isValid then begin
    aProp:=TAdvPropInfo(Edit.Tag);
    aProp.doc.DispatchCommand(TChangeIntegerCommand.Create(aProp.instance as TStreamingClass,aProp.Name,Edit.value));
  end;
end;

(*
      TFLoatPropertyEditor
                                *)
procedure TFloatPropertyEditor.AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo);
var lab: TLabel;
    fltedit: TFloatEdit;
begin
  lab:=DrawLabel(aowner,left,top+4,aPropInfo);
  if Assigned(aPropInfo.SetProc) then begin
    left:=left+lab.Width+5;

    fltEdit:=AddCautiousEditor(aowner, TFloatEdit,left,top,apropinfo) as TFloatEdit;
    fltEdit.OnExit:=fChangeProc;
    fltEdit.value:=GetFloatProp(aPropInfo.instance,aPropInfo.Name);

    left:=left+fltEdit.Width+5;
  end
  else begin
    lab.Caption:=lab.Caption+'='+FloatToStr(GetFloatProp(aPropInfo.instance,aPropInfo.Name));
    left:=left+lab.Width+5;
  end;
  right:=left;
  bottom:=top+lab.Height;
end;

procedure TFloatPropertyEditor.fChangeProc(Sender: TObject);
var aProp: TAdvPropInfo;
    edit: TFloatEdit;
begin
  edit:=Sender as TFloatEdit;
  if edit.isValid then begin
    aProp:=TAdvPropInfo(edit.Tag);
    aProp.doc.DispatchCommand(TChangeFloatCommand.Create(aProp.Instance as TStreamingClass,aProp.name,edit.value));
  end;
end;

(*
      TColorPropertyEditor
                            *)
(*
constructor TColorPropertyEditor.Create;
begin
  inherited Create;
  fColorDlg:=TColorDialog.Create(nil);
end;

destructor TColorPropertyEditor.Destroy;
begin
  fColorDlg.Free;
  inherited Destroy;
end;
*)
procedure TColorPropertyEditor.AddEditor(aowner: TWinControl; left,top: Integer; out right,bottom: Integer; aPropInfo: TAdvPropInfo);
var lab: TLabel;
    colorEd: TColorBox;
    comp: TComponent absolute colorEd;
//    button: TButton;
begin
  lab:=DrawLabel(aowner,left,top+4,aPropInfo);
  if Assigned(aPropInfo.SetProc) then begin
    left:=left+lab.Width+5;
    comp:=aowner.FindComponent(aPropInfo.Name);
    if not (comp is TColorBox) then begin
      colorEd:=TColorBox.Create(aowner);
      colorEd.Name:=aPropInfo.Name;
      colorEd.Parent:=aowner;
      colorEd.Left:=left;
      colorEd.Top:=top;
      colorEd.Style:=colorEd.Style+[cbPrettyNames,cbCustomColor]-[cbSystemColors];
  //    Include(colorEd.Style,cbPrettyNames);
      colorEd.Tag:=Integer(aPropInfo);
      colorEd.OnChange:=fChangeProc;
    end;
    left:=left+colorEd.Width+5;
    colorEd.Selected:=GetOrdProp(aPropInfo.instance,aPropInfo.Name);
(*
    button:=TButton.Create(aowner);
    button.Parent:=aowner;
    button.Left:=left;
    button.Top:=top;
    button.Caption:='Другие цвета';
    button.Tag:=Integer(aPropInfo);
    button.OnClick:=fClickProc;
    left:=left+button.Width+5;
*)
  end
  else begin

  end;


  right:=left;
  bottom:=top+lab.Height;
end;

procedure TColorPropertyEditor.fChangeProc(Sender: TObject);
var aProp: TAdvPropInfo;
begin
  aProp:=TAdvPropInfo((Sender as TColorBox).Tag);
  aProp.doc.DispatchCommand(TChangeIntegerCommand.Create(aProp.Instance as TStreamingClass,aProp.Name,(Sender as TColorBox).Selected));
end;

(*
procedure TColorPropertyEditor.fClickProc(Sender: TObject);
var aProp: TAdvPropInfo;
begin
  aProp:=TAdvPropInfo((Sender as TButton).Tag);
  fColorDlg.Color:=GetOrdProp(aProp.instance,aProp.Name);
  if fColorDlg.Execute then begin
    aProp.doc.DispatchCommand(TChangeIntegerCommand.Create(aProp.instance as TStreamingClass,aProp.Name,fColorDlg.Color));
  end;
end;
*)
(*
    TBooleanPropertyEditor
                            *)
procedure TBooleanPropertyEditor.AddEditor(aowner: TWinControl; left,top: Integer; out right,bottom: Integer; aPropInfo:TAdvPropInfo);
var chk: TCheckBox;
    comp: TComponent absolute chk;
    btmp: TBitmap;
begin
//  lab:=DrawLabel(aowner,left,top,aPropInfo);
  comp:=aowner.FindComponent(aPropInfo.Name);
  if not (comp is TCheckBox) then begin
    chk:=TCheckBox.Create(aowner);
    chk.Name:=aPropInfo.Name;
    chk.Parent:=aowner;
    chk.Left:=left;
    chk.Top:=top+2;
    if aPropInfo.title='' then chk.Caption:=aPropInfo.Name
    else chk.Caption:=aPropInfo.title;
    chk.Hint:=aPropInfo.hint;

    chk.Tag:=Integer(aPropInfo);
    //хитрючий план, чтобы посчитать требуемый размер chk
    btmp:=TBitmap.Create;
    btmp.Canvas.Font:=chk.Font;
    chk.Width:=20+btmp.Canvas.TextWidth(chk.Caption);
    btmp.Free;
    chk.OnClick:=fChangeProc;
  end;
  chk.Checked:=Boolean(GetOrdProp(aPropInfo.instance,aPropInfo.Name));

  right:=left+chk.Width+5;
  bottom:=top+chk.Height;
end;

procedure TBooleanPropertyEditor.fChangeProc(Sender: TObject);
var aProp: TAdvPropInfo;
begin
  aProp:=TAdvPropInfo((Sender as TCheckBox).tag);
  aProp.doc.DispatchCommand(TChangeBoolCommand.Create(aProp.instance as TStreamingClass,aProp.Name,(Sender as TCheckBox).Checked));
end;

(*
      TStringPropertyEditor
                            *)
procedure TStringPropertyEditor.AddEditor(aowner: TWinControl;left,top: Integer; out right,bottom: Integer;aPropInfo: TAdvPropInfo);
var lab: TLabel;
    edit: TCautiousEdit;
begin
  lab:=DrawLabel(aowner,left,top+4,aPropInfo);
  if Assigned(aPropInfo.SetProc) then begin
    left:=left+lab.Width+5;
    edit:=AddCautiousEditor(aowner,TCautiousEdit,left,top,aPropInfo);
    edit.OnExit:=fChangeProc;
    edit.OnValidateResult:=fValidateProc;

    edit.Text:=GetStrProp(aPropInfo.instance,aPropInfo.Name);
    left:=left+edit.Width+5;
  end
  else begin
    //только для чтения
    lab.Caption:=lab.Caption+'='+IntToStr(GetOrdProp(aPropInfo.instance,aPropInfo.Name));
    left:=left+lab.Width+5;
  end;
  right:=left;
  bottom:=top+lab.Height;
end;

procedure TStringPropertyEditor.fChangeProc(Sender: TObject);
var edit: TCautiousEdit;
    aProp: TAdvPropInfo;
begin
  edit:=Sender as TCautiousEdit;
  if edit.SeemsNormal then begin
    aProp:=TAdvPropInfo(Edit.Tag);
    aProp.doc.DispatchCommand(TChangeStringCommand.Create(aProp.instance as TStreamingClass,aProp.Name,Edit.text));
  end;
end;

procedure TStringPropertyEditor.fValidateProc(Sender: TObject);
begin

end;

(*
      TNamePropertyEditor
                            *)
procedure TNamePropertyEditor.fvalidateproc(Sender: TObject);
var edit: TCautiousEdit;
  aProp: TAdvPropInfo;
  comp: TComponent;
resourcestring
  NamePropertyEditorDuplicateNameStr = 'Объект с данным именем уже существует';
  NamePropertyEditorInvalidNameStr = 'Имя должно состоять начинаться с латинской буквы или прочерка и состоять только из латинских букв, прочерков и цифр';
begin
  edit:=Sender as TCautiousEdit;
  if IsValidIdent(edit.Text) then begin
    aProp:=TAdvPropInfo(Edit.Tag);
    comp:=(aProp.instance as TComponent).Owner.FindComponent(Edit.Text);
    if Assigned(comp) and (comp<>aProp.instance) then
      edit.TurnRed(NamePropertyEditorDuplicateNameStr)
    else
      edit.ReturnToNormal;
  end
  else edit.TurnRed(NamePropertyEditorInvalidNameStr);
end;


(*
      TBFGShowProperties
                            *)
constructor TShowProperties.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fFullPropList:=TStringList.Create;
  fPropEditors:=TStringList.Create;
  fPropEditors.AddObject('Integer',TIntegerPropertyEditor.Create);
  fPropEditors.AddObject('TColor',TColorPropertyEditor.Create);
  fPropEditors.AddObject('Boolean',TBooleanPropertyEditor.Create);
  fPropEditors.AddObject('Real',TFloatPropertyEditor.Create);
  fPropEditors.AddObject('String',TStringPropertyEditor.Create);
  fPropEditors.AddObject('TComponentName',TNamePropertyEditor.Create);
end;

destructor TShowProperties.Destroy;
var i: Integer;
begin
  for i:=0 to fFullPropList.Count-1 do
    fFullPropList.Objects[i].Free;
  fFullPropList.Free;
  for i:=0 to fPropEditors.Count-1 do
    fPropEditors.Objects[i].Free;
  fPropEditors.Free;
  inherited Destroy;
end;

procedure TShowProperties.SetControl(value: TWinControl);
begin
  if Assigned(fControl) then
    fControl.RemoveFreeNotification(self);
  fControl:=value;
  if Assigned(fControl) then
    fControl.FreeNotification(self);
end;

procedure TShowProperties.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (aComponent=fControl) and (operation=opRemove) then
    fControl:=nil;
  inherited Notification(aComponent,operation);
end;

function TShowProperties.Update: Boolean;
begin
  Result:=inherited Update;
end;

procedure TShowProperties.AddTitleAndHint(name,title,hint: string);
var i: Integer;
  aprop: TAdvPropInfo;
begin
  i:=fFullPropList.IndexOf(name);
  if i>=0 then begin
    aprop:=TAdvPropInfo(fFullPropList.Objects[i]);
    aprop.title:=title;
    aprop.hint:=hint;
  end;
end;

procedure TShowProperties.UnregisterProperty(name: string);
var i: Integer;
begin
  i:=fFullPropList.IndexOf(name);
  if i>=0 then begin
    fFullPropList.Objects[i].Free;
    fFullPropList.Delete(i);
  end;
end;

procedure TShowProperties.ObserverUpdate;
const
  tkSupported=[tkEnumeration, tkInteger, tkChar, tkSet, tkWChar,tkFloat,tkString,tkLString,tkWString];
var i,j,count: Integer;
    curPos: Integer;
    PropList: PPropList;
    obj: TPersistent;
    advprop: IAdvancedProperties;
    aProp: TAdvPropInfo;
    propEditor: TPropertyEditor;
begin
  if fControl=nil then Exit;
  //свойство enabled не особенно актуально, можно и не вызывать inherited;



  //на первых порах будем играть только с одиночными компонентами
  curPos:=4;

  if PickTool.fPickedGroup.Count=1 then begin
    obj:=PickTool.fPickedGroup.Item[0];
    if obj<>fLastObj then fControl.DestroyComponents;

    for i:=0 to fFullPropList.Count-1 do
      fFullPropList.Objects[i].Free;
    fFullPropList.Clear;  //не потерять бы память... пока не теряем

    fLastObj:=obj;
    Count:= GetPropList(PTypeInfo(obj.ClassInfo),tkSupported,nil,false);
    GetMem(PropList,Count*SizeOf(PPropInfo));
    try
      GetPropList(PTypeInfo(obj.ClassInfo),tkSupported,PropList,false);
      for i:=0 to Count-1 do begin
        aProp:=TAdvPropInfo.Create;
        aProp.Name:=PropList[i].Name;
        aProp.PropType:=PropList[i].PropType;
        aProp.GetProc:=PropList[i].GetProc;
        aProp.SetProc:=PropList[i].SetProc;
        aProp.StoredProc:=PropList[i].StoredProc;
        aProp.Index:=PropList[i].Index;
        aProp.Default:=PropList[i].Default;
        aProp.NameIndex:=PropList[i].NameIndex;
        aProp.instance:=obj;
        aProp.doc:=doc;
        fFullPropList.AddObject(aProp.Name,aProp);
      end;
    finally
      FreeMem(PropList);
    end;
    if obj.GetInterface(IAdvancedProperties,advprop) then begin
      advprop.AddTitleAndHint(AddTitleAndHint);
      advprop.UnregisterProperties(UnregisterProperty);
    end;
  end
  else begin
    fControl.DestroyComponents;

    for i:=0 to fFullPropList.Count-1 do
      fFullPropList.Objects[i].Free;
    fFullPropList.Clear;  //не потерять бы память... пока не теряем
  end;

  for i:=0 to fFullPropList.Count-1 do begin
    aProp:=TAdvPropInfo(fFullPropList.Objects[i]);
    j:=fPropEditors.IndexOf(aProp.PropType^.Name);
    if j>=0 then begin
      propEditor:=TPropertyEditor(fPropEditors.Objects[j]);
      propEditor.AddEditor(fControl,curPos,4,curPos,j,aProp);
    end;

  end;

end;


(*
      TBFGPasteButton
                            *)
constructor TPasteButtonAction.Create(owner: TComponent);
resourcestring
  PasteButtonCaption = 'Вставить';
  PasteButtonHint = 'Вставить из буфера обмена';
begin
  inherited Create(owner);
  //imageIndex:=11;
  Caption:=PasteButtonCaption;
  Hint:=PasteButtonHint;
  ShortCut:=Menus.ShortCut(Word('V'),[ssCtrl]);
end;

function TPasteButtonAction.update: Boolean;
var v: TComponent;
  intf: IGraphicObject;
  e: boolean;
begin
  v:=nil;
  e:=false;
  if Clipboard.HasFormat(CF_TEXT) then begin
    if Uppercase(LeftStr(Clipboard.AsText,7))='OBJECT ' then
      try
        v:=TStreamingClass.LoadComponentFromString(Clipboard.AsText);
        e:=(v.GetInterface(IGraphicObject,intf));
//почему-то эта строчка вызывает access violation при открытии History (!!!)
//больше нет, прояснилась ситуация
      finally
        intf:=nil;  //выполняется v._Release, если не сделать этого явно,
        //будет Access violation
        v.Free;
      end;
  end;
  enabled:=e;
  Result:=true;
end;

procedure TPasteButtonAction.ExecuteTarget(Target: TObject);
var c: TComponent;
  intf: IGraphicObject;
  i,j: Integer;
  doc: TDocumentWithImage;
  strStream: TStringStream;
  BinStream: TMemoryStream;
  gr: TGraphicObjectGroup;
begin
  doc:=GetDoc as TDocumentWithImage;
  doc.criticalSection.Acquire;
  gr:=TGraphicObjectGroup.Create(doc);
  gr.OwnsObjects:=true;
  BinStream:=TMemoryStream.Create;
  StrStream:=TStringStream.Create(Clipboard.AsText);
  while StrStream.Position<StrStream.Size do
    ObjectTextToBinary(StrStream,BinStream);
  BinStream.Seek(0, soFromBeginning);
  while BinStream.Position<BinStream.Size do begin
    c:=BinStream.ReadComponent(nil);
    if c.GetInterface(IGraphicObject,intf) then begin
      (c as TStreamingClass).ensureCorrectNames(doc);
      doc.InsertComponent(c);
      if intf.isOkToPaste then gr.Add(c)
      else begin
        intf:=nil;  //иначе он вызовет _release на выходе из функции, на несущ. объект
        c.Free;
      end;
    end;
  end;
  BinStream.Free;
  StrStream.Free;

  i:=0;
  while i<doc.ComponentCount do begin
    if doc.Components[i].GetInterface(IGraphicObject,intf) and (doc.Components[i]<>gr) then
      for j:=0 to gr.Count-1 do
        if EqualRect(intf.Rect,gr.iitem(j).Rect) then begin
          gr.Move(doc.get_duplicate_shift_x,doc.get_duplicate_shift_y);
          i:=0; //надо проверить, может вот теперь дублируется
          break;
        end;
      inc(i);
  end;
  doc.RemoveComponent(gr);
  doc.CriticalSection.Release;  //перестали в свое удовольствие возиться с документом
  if gr.Count>0 then
    doc.DispatchCommand(TAddGraphicCommand.NewGr(gr,true))
  else
    gr.Free;
end;

(*
      TBFGSelectAllButton
                              *)
constructor TSelectAllButtonAction.Create(owner: TComponent);
resourcestring
  SelectAllCaption = 'Выбрать все';
  SelectAllHint = 'Выбрать все';
begin
  inherited Create(Owner);
  ImageIndex:=-1; //это пока что, потом может и придумаем чего
  Caption:=SelectAllCaption;
  Hint:=SelectAllHint;
  ShortCut:=TextToShortcut('Ctrl+A');
end;

function TSelectAllButtonAction.update: boolean;
var intf: IGraphicObject;
begin
  (GetDoc as TDocumentWithImage).GraphicObjectsIterator.First(intf);
  enabled:=(intf<>nil); //должен быть хотя бы один!
  //мы здесь не удаляем объекты, поэтому пусть intf сам вызовет _release
  //выходя за область видимости
  Result:=true;
end;

procedure TSelectAllButtonAction.ExecuteTarget(Target: TObject);
var pick: TGraphicPickTool;
    doc: TAbstractDocument;
    i: Integer;
    intf: IGraphicObject;
begin
  pick:=nil;
  for i:=0 to ActionList.ActionCount-1 do
    if ActionList.Actions[i] is TGraphicPickTool then begin
      pick:=TGraphicPickTool(ActionList.Actions[i]);
      break;
    end;
  if Assigned(pick) then begin
    pick.Execute;
    doc:=GetDoc as TAbstractDocument;
    pick:=doc.tool as TGraphicPickTool;
    pick.fPickedGroup.Clear;
    for i:=0 to doc.ComponentCount-1 do
      if doc.Components[i].GetInterface(IGraphicObject,intf) then
        pick.fPickedGroup.Add(intf.Implementor);
    doc.Change;
  end;
end;

initialization
RegisterClasses([TGraphicPickTool]);
RegisterClasses([TCutButtonAction,TCopyButtonAction,TDeleteButtonAction,TPasteButtonAction]);
end.
