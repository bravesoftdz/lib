unit my_bloody_chart;

interface
uses ExtCtrls,graphics,SysUtils;
type
  Talign=(alLeft,alCenter,alRight);
  Tvalign=(valTop,valCenter,valBottom);
  bloody_chart=class
    public
    x_offset,y_offset: Integer; //������� ������ � �����
    grid_number: Integer; //������� ����� ������� ����� �����
    labels_number: Integer; //������� ����-��������
    Font_size: Integer; //������ ������ ��� ��������
    scale: Real; //������� (�� ����� ���� ���� - ��� ��� � ����)
    image: TImage; //���� ��� ��������
    xmin,xmax,ymin,ymax: Real; //��� � ���� �������� ������� �� ����
    x_range, y_range: Integer;
    grid_color: TColor;
    major_grid_color: TColor;
    major_grid_width: Integer;
    lines_width: Integer;
    procedure point(x: Real; y: Real; color: TColor);
    procedure circle(x: Real; y: Real; r: Integer);
    procedure LineTo(x: Real; y:Real);
    procedure MoveTo(x: Real; y:Real);
    procedure TextOut(x: Real; y:Real; s:string; align: TAlign; valign: TValign);
    procedure init;
    constructor Create;
  end;
implementation

constructor bloody_chart.Create;
begin
  inherited Create;
  x_offset:=40;
  y_offset:=40;
  Font_size:=25;
  grid_number:=9;
  labels_number:=3;
  grid_color:=clGray;
  major_grid_color:=clBlack;
  major_grid_width:=2;
end;

procedure bloody_chart.init;
var xscale,yscale: Real;
    grid_x,ticks: Real;
    i: Integer;
    s: string;
begin
  yscale:=(image.ClientHeight-2*y_offset)/(ymax-ymin);
  xscale:=(image.ClientWidth-2*x_offset)/(xmax-xmin);
  image.Picture.Bitmap.Height:=image.ClientHeight;
  image.Picture.Bitmap.Width:=image.ClientWidth;
  if yscale>xscale then begin
    //����� ������� ������
    x_range:=image.ClientWidth-2*x_offset;
    y_range:=Round(xscale/yscale*(image.ClientHeight-2*y_offset));
    grid_x:=(ymax-ymin)/(grid_number-1);
    ticks:=(ymax-ymin)/(labels_number-1);
    scale:=xscale;
  end
  else begin
    y_range:=image.ClientHeight-2*y_offset;
    x_range:=Round(yscale/xscale*(image.ClientWidth-2*x_offset));
    grid_x:=(xmax-xmin)/(grid_number-1);
    ticks:=(xmax-xmin)/(labels_number-1);
    scale:=yscale;
  end;
  image.Canvas.Brush.Color:=clWhite;
  image.Canvas.FillRect(image.ClientRect);
  image.Canvas.Pen.Color:=clBlack;
  image.Canvas.Rectangle(x_offset,y_offset,x_offset+x_range,y_offset+y_range);
  //���������� ������� �������. ������: �����
  image.Canvas.Pen.Color:=grid_color;
  image.Canvas.Pen.Width:=1;
  i:=1;
  repeat
    image.Canvas.MoveTo(x_offset+round(i*grid_x*scale),y_offset);
    image.Canvas.LineTo(x_offset+round(i*grid_x*scale),y_offset+y_range);
    inc(i)
  until i*grid_x*scale>=x_range;

  i:=1;
  repeat
    image.Canvas.MoveTo(x_offset,y_offset+y_range-round(i*grid_x*scale));
    image.Canvas.LineTo(x_offset+x_range,y_offset+y_range-round(i*grid_x*scale));
    inc(i)
  until i*grid_x*scale>=y_range;
  //������ ������� � "����"
  Image.Canvas.Pen.Width:=major_grid_width;
  Image.Canvas.Pen.Color:=major_grid_color;
  Image.Canvas.Font.Size:=font_size;
  i:=0;
  repeat
    image.Canvas.MoveTo(x_offset+round(i*ticks*scale),y_offset);
    image.Canvas.LineTo(x_offset+round(i*ticks*scale),y_offset+y_range+5);
    s:=FloatToStrF(xmin+i*ticks,ffFixed,5,0);
    image.Canvas.TextOut(x_offset+round(i*ticks*scale)-(image.Canvas.TextWidth(s) div 2),y_offset+y_range+15,s);
    inc(i)
  until i*ticks*scale-x_range>1;

  i:=0;
  repeat
    image.Canvas.MoveTo(x_offset+x_range,y_offset+y_range-round(i*ticks*scale));
    image.Canvas.LineTo(x_offset-5,y_offset+y_range-round(i*ticks*scale));
    s:=FloatToStrF(ymin+i*ticks,ffFixed,5,0);
    image.Canvas.TextOut(x_offset-5-image.Canvas.TextWidth(s),y_offset+y_range-round(i*ticks*scale)-(image.Canvas.TextHeight(s) div 2),s);
    inc(i)
  until i*ticks*scale-y_range>1;

  Image.Canvas.Pen.Width:=1;

end;

procedure  bloody_chart.point(x: Real; y:Real;color: TColor);
begin
if (x<xmax) and (x>xmin) and (y<ymax) and (y>ymin) then
  image.Canvas.Pixels[x_offset+Round((x-xmin)*scale),y_offset+y_range-Round((y-ymin)*scale)]:=color;
end;


procedure bloody_chart.circle(x: Real; y: Real; r:Integer);
begin
  image.Canvas.Brush.Color:=clBlack;
  image.Canvas.Pen.Color:=clBlack;
  if (x<xmax) and (x>xmin) and (y<ymax) and (y>ymin) then
    image.Canvas.Ellipse(x_offset+Round((x-xmin)*scale)-r,y_offset+y_range-Round((y-ymin)*scale)-r,x_offset+Round((x-xmin)*scale)+r,y_offset+y_range-Round((y-ymin)*scale)+r);
end;

procedure bloody_chart.LineTo(x: Real; y:Real);
begin
  image.Canvas.Pen.Width:=lines_Width;
  if (x<=xmax) and (x>=xmin) and (y<=ymax) and (y>=ymin) then
    image.Canvas.LineTo(x_offset+Round((x-xmin)*scale),y_offset+y_range-Round((y-ymin)*scale));
end;

procedure bloody_chart.MoveTo(x:Real; y:Real);
begin
  if (x<=xmax) and (x>=xmin) and (y<=ymax) and (y>=ymin) then
    image.Canvas.MoveTo(x_offset+Round((x-xmin)*scale),y_offset+y_range-Round((y-ymin)*scale));
end;

procedure bloody_chart.TextOut(x: Real; y:Real; s:string; align: TAlign; valign: TValign);
var px,py: Integer;
begin
  if (x<xmax) and (x>xmin) and (y<ymax) and (y>ymin) then begin
    px:=x_offset+Round((x-xmin)*scale);
    if align=alCenter then px:=px-(image.Canvas.TextWidth(s) div 2)
    else if align=alRight then px:=px-image.Canvas.TextWidth(s);
    py:=y_offset+y_range-Round((y-ymin)*scale);
//    image.Canvas.Brush.Color:=clWhite;
    image.Canvas.Brush.Style:=bsClear;
    image.Canvas.TextOut(px,py,s);
  end;
end;

end.
 