unit IGraphicObject_primitives;

interface

uses IGraphicObject_commands,streaming_class_lib,types,classes,graphics;

type

TLineGraphicObject = class (TStreamingClass, IGraphicObject)
  protected
    fc: array [0..3] of Integer;  //0:x1, 1:y1, 2: x2, 3:y2
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
  Result:=types.Rect(fc[0],fc[1],fc[2],fc[3]);
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
  ax:=fc[2]-fc[0];
  ay:=fc[3]-fc[1];
  bx:=x-fc[0];
  by:=y-fc[1];
  cx:=fc[2]-x;
  cy:=fc[3]-y;
  if (ax=0) and (ay=0) then Result:=bx*bx+by*by
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
  inc(fc[0],dx);
  inc(fc[1],dy);
  inc(fc[2],dx);
  inc(fc[3],dy);
end;

procedure TLineGraphicObject.Resize(dx1,dy1,dx2,dy2: Integer;Corrector: TGraphicResizeCorrector=nil);
var infrect: TRect;
    mirrx,mirry: boolean;
begin
  mirrx:=(fc[0]>fc[2]);
  if mirrx then SwapIntegers(dx1,dx2);
  mirry:=(fc[1]>fc[3]);
  if mirry then SwapIntegers(dy1,dy2);
  if Assigned(corrector) and corrector.reverse and ((fc[0]=fc[2]) or (fc[1]=fc[3])) then begin
    infrect:=corrector.Pop;
    if infrect.Left=1 then SwapIntegers(dx1,dx2);
    if infrect.Top=1 then SwapIntegers(dy1,dy2);
  end;
  inc(fc[0],dx1);
  inc(fc[1],dy1);
  inc(fc[2],dx2);
  inc(fc[3],dy2);
  if Assigned(corrector) and (not corrector.reverse) and ((fc[0]=fc[2]) or (fc[1]=fc[3])) then begin
    infrect:=Types.Rect(0,0,0,0);
    if (fc[0]=fc[2]) and mirrx then infrect.Left:=1;
    if (fc[1]=fc[3]) and mirry then infrect.Top:=1;
    corrector.Push(infrect);
  end;
end;

procedure TLineGraphicObject.Rotate(alpha: Real; Xcenter,Ycenter: Integer; corrector: TGraphicResizeCorrector=nil);
var dx1,dy1,dx2,dy2: Integer;
    bux1,buy1,bux2,buy2: Integer;
    si,co: Real;
    corRect: TRect;
begin
  dx1:=fc[0]-XCenter;
  dy1:=fc[1]-YCenter;
  dx2:=fc[2]-XCenter;
  dy2:=fc[3]-YCenter;
  bux1:=fc[0];
  buy1:=fc[1];
  bux2:=fc[2];
  buy2:=fc[3];
  si:=sin(alpha);
  co:=cos(alpha);
  fc[0]:=XCenter+Round(dx1*co-dy1*si);
  fc[1]:=YCenter+Round(dx1*si+dy1*co);
  fc[2]:=XCenter+Round(dx2*co-dy2*si);
  fc[3]:=YCenter+Round(dx2*si+dy2*co);
  if Assigned(corrector) then
    if corrector.reverse then begin
      corRect:=corrector.Pop;
      inc(fc[0],corRect.Left);
      inc(fc[1],corRect.Top);
      inc(fc[2],corRect.Right);
      inc(fc[3],corRect.Bottom);
    end
    else begin
      dx1:=fc[0]-XCenter;
      dy1:=fc[1]-YCenter;
      dx2:=fc[2]-XCenter;
      dy2:=fc[3]-YCenter;
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
  Result:=(fc[0]<>fc[2]);
end;

function TLineGraphicObject.ResizableByY: Boolean;
begin
  Result:=(fc[1]<>fc[3]);
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
var doc: TDocumentWithImage;
begin
  doc:=FindOwner as TDocumentWithImage;
  //весьма условно - линия с теми параметрами pen, какие сидели по умолч.
  //в системе коорд., в которой заданы fx1,fy1 и пр.
  with Canvas do begin
    MoveTo(doc.XVal2Pix(fc[0]),doc.YVal2Pix(fc[1]));
    LineTo(doc.XVal2Pix(fc[2]),doc.YVal2Pix(fc[3]));
  end;
end;

end.
