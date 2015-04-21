unit IGraphicObject_primitives;

interface

uses IGraphicObject_commands,streaming_class_lib,types,classes,graphics;

type

TLineGraphicObject = class (TStreamingClass, IGraphicObject)
  protected
    fx1,fy1,fx2,fy2: Integer;
  public
    function Rect: TRect; virtual; //реальные размеры
    function SymbolRect: TRect; virtual; //как оно отобр. на экране
    function DistanceSquaredFromPoint(x,y: Integer): Real; virtual;
    function DistanceTo(ax,ay: Integer): Real;
    procedure Move(dx,dy: Integer); virtual;
    procedure Resize(dx1,dy1,dx2,dy2: Integer;Corrector: TGraphicResizeCorrector=nil); virtual;
    procedure Rotate(alpha: Real; Xcenter,Ycenter: Integer; corrector: TGraphicResizeCorrector=nil); virtual;
    function ResizableByX: boolean; virtual;
    function ResizableByY: boolean; virtual;
    function isOkToPaste: boolean; virtual;
    function Implementor: TComponent; virtual;
    procedure Draw(canvas: TCanvas); virtual;
end;

implementation

function TLineGraphicObject.Rect: TRect;
begin
  //что важно, должно быть top<bottom и left<right
  Result:=types.Rect(fx1,fy1,fx2,fy2);
  if Result.Left>result.Right then swapIntegers(Result.left,Result.right);
  if Result.Top>result.Bottom then swapIntegers(Result.Top,Result.bottom);
end;

function TLineGraphicObject.SymbolRect: TRect;
begin
  Result:=Rect; //дальнейшее сделают производные классы, если захотят
end;

function TLineGraphicObject.DistanceSquaredFromPoint(x,y: Integer): Real;
var ax,ay,bx,by,cx,cy,k: Real;
begin
  ax:=fx2-fx1;
  ay:=fy2-fy1;
  bx:=x-fx1;
  by:=y-fy1;
  cx:=fx2-x;
  cy:=fy2-y;
  if (fx2=fx1) and (fy2=fy1) then Result:=bx*bx+by*by
  else begin
    k:=(ax*bx+ay*by)/(ax*ax+ay*ay);
    if k<0 then Result:=bx*bx+by*by
    else if k>1 then Result:=cx*cx+cy*cy
    else begin
      bx:=bx-k*ax;
      by:=by-k*ay;
      Result:=bx*bx+by*by;
    end;
  end;
end;

function TLineGraphicObject.DistanceTo(ax,ay: Integer): Real;
begin
  Result:=sqrt(DistanceSquaredFromPoint(ax,ay));
end;

procedure TLineGraphicObject.Move(dx,dy: Integer);
begin
  fx1:=fx1+dx;
  fy1:=fy1+dy;
  fx2:=fx2+dx;
  fy2:=fy2+dy;
end;

procedure TLineGraphicObject.Resize(dx1,dy1,dx2,dy2: Integer;Corrector: TGraphicResizeCorrector=nil);
var infrect: TRect;
    mirrx,mirry: boolean;
begin
  mirrx:=(fx1>fx2);
  if mirrx then SwapIntegers(dx1,dx2);
  mirry:=(fy1>fy2);
  if mirry then SwapIntegers(dy1,dy2);
  if Assigned(corrector) and corrector.reverse and ((fx1=fx2) or (fy1=fy2)) then begin
    infrect:=corrector.Pop;
    if infrect.Left=1 then SwapIntegers(dx1,dx2);
    if infrect.Top=1 then SwapIntegers(dy1,dy2);
  end;
  fx1:=fx1+dx1;
  fy1:=fy1+dy1;
  fx2:=fx2+dx2;
  fy2:=fy2+dy2;
  if Assigned(corrector) and (not corrector.reverse) and ((fx1=fx2) or (fy1=fy2)) then begin
    infrect:=Types.Rect(0,0,0,0);
    if (fx1=fx2) and mirrx then infrect.Left:=1;
    if (fy1=fy2) and mirry then infrect.Top:=1;
    corrector.Push(infrect);
  end;
end;

procedure TLineGraphicObject.Rotate(alpha: Real; Xcenter,Ycenter: Integer; corrector: TGraphicResizeCorrector=nil);
var dx1,dy1,dx2,dy2: Integer;
    bux1,buy1,bux2,buy2: Integer;
    si,co: Real;
    corRect: TRect;
begin
  dx1:=fx1-XCenter;
  dy1:=fy1-YCenter;
  dx2:=fx2-XCenter;
  dy2:=fy2-YCenter;
  bux1:=fx1;
  buy1:=fy1;
  bux2:=fx2;
  buy2:=fy2;
  si:=sin(alpha);
  co:=cos(alpha);
  fx1:=XCenter+Round(dx1*co-dy1*si);
  fy1:=YCenter+Round(dx1*si+dy1*co);
  fx2:=XCenter+Round(dx2*co-dy2*si);
  fy2:=YCenter+Round(dx2*si+dy2*co);
  if Assigned(corrector) then
    if corrector.reverse then begin
      corRect:=corrector.Pop;
      fx1:=fx1+corRect.Left;
      fy1:=fy1+corRect.Top;
      fx2:=fx2+corRect.Right;
      fy2:=fy2+corRect.Bottom;
    end
    else begin
      dx1:=fx1-XCenter;
      dy1:=fy1-YCenter;
      dx2:=fx2-XCenter;
      dy2:=fy2-YCenter;
      alpha:=-alpha;
      si:=sin(alpha);
      co:=cos(alpha);
      corRect.Left:=XCenter+Round(dx1*co-dy1*si);
      corRect.Top:=YCenter+Round(dx1*si+dy1*co);
      corRect.Right:=XCenter+Round(dx2*co-dy2*si);
      corRect.Bottom:=YCenter+Round(dx2*si+dy2*co);
      corRect.Left:=bux1-corRect.Left;
      corRect.Top:=buy1-corRect.Top;
      corRect.Right:=bux2-corRect.Right;
      corRect.Bottom:=buy2-corRect.Bottom;
      corrector.Push(corRect);
    end;
end;

function TLineGraphicObject.ResizableByX: Boolean;
begin
  Result:=(fx1<>fx2);
end;

function TLineGraphicObject.ResizableByY: Boolean;
begin
  Result:=(fy1<>fy2);
end;

function TLineGraphicObject.isOkToPaste: Boolean;
begin
  Result:=true; //производные классы наверняка решат по-другому
end;

function TLineGraphicObject.Implementor: TComponent;
begin
  Result:=self;
end;

procedure TLineGraphicObject.Draw(canvas: TCanvas);
begin
  //весьма условно - линия с теми параметрами pen, какие сидели по умолч.
  //в системе коорд., в которой заданы fx1,fy1 и пр.
  with Canvas do begin
    Move(fx1,fy1);
    LineTo(fx2,fy2);
  end;
end;

end.
