unit colorimetry_lib;
(* версия 0.08

изменения в v. 0.08
- новый класс TSpectrum
- можно непосредственно узнать полную мощность излучения, световой поток и кол-во квантов
- а также нормировать графики по ним
- и еще, средняя длина волны

изменения в v. 0.07
- таблицы xt,yt,zt перемещены в data/colorimetry/

изменения в v. 0.06
- таблицы xt,yt,zt стали публичными
- новая функция color_from_YxY - чтобы получить цвет из переменных Y,x,y (яркость и 2 коэф. цвета)
- gamma содержит защиту от переполнения
- новая функция color_from_normedXYZ - чтобы получить цвет+яркость, причем Y=1 соотв. макс. яркости, отобр. на экране
- функция spectrum2XYZ - пересчитывает спектр в коорд. CIE1931.

изменения в v. 0.05
- вроде бы 2 параллельные версии 0.04 наконец объединены в одну
- новая функция color_of_rgb_sum - чтобы найти суммарный цвет от спектра

изменения в v. 0.04
- добавлены функции color_from_WL (цвет - из длины волны, т.е из чистой спектральной линии)
и color_from_Spectrum (цвет - из заданного спектра)
- Add_CCT поменялась - теперь там учитывается зависимость пятой степени макс. интенсивности от температуры
- gamma_func стала публичной, чтобы гамму-корекцию можно было использовать в программе
- добавлена gamma - та же гамма-коррекция, но уже домножена на 255 и приведена к типу Integer

изменения в v. 0.03
- добавлены переменные sat и desat, диктующие, как работать
с отрицательными коэф. r,g,b и со слишком большими.
sat=1 означает нормировку по макс. яркости,
меньшие значения - компрессия дин. диапазона.
desat=0 - отр. значения обрезаются до 0
=1 - прибавляется столько белого, чтобы нулевых знач. не осталось

- добавлена функция Clear, чтобы очистить таблицы r,g,b
- добавлена гамма-коррекция

изменения в v 0.02
- добавлены табличные функции r,g,b, чтобы в рамках класса построить
цветовую полосу и затем при необходимости отнормировать
- функции для добавления новой точки (r,g,b) - или конкретных значений,
или соотв. монохроматическому излучению заданной длины волны, или
соотв. излучению АЧТ заданной температуры.
- параметр canvas, XLeft,XRight,YTop,YBottom - для рисования этой полосы на экране
функцией draw.



*)


interface

uses
table_func_lib,graphics,math,windows,chart,TeeProcs,Series;

type
RGBf=record
  R,G,B: Real;
end;
CIE1931=record
  X,Y,Z: Real;
end;

TSpectrum=class(table_func)
  public
    procedure NormalizeByPower(pow: Real);
    procedure NormalizeByLuminance(lum: Real);
    procedure NormalizeByQuants(amount: Real); //этих квантов может быть настолько много, что лучше так
    function radiant_power: Real;
    function luminance: Real;
    function average_wavelength: Real;
    function quants: Real;
  end;


colorimetry_funcs=class
  private
    rt,gt,bt :table_func;
    function XYZ2RGB(x:Real;y:Real;z:Real): RGBf;
    function XYZ2RGB_no_limit(x:Real;y:Real;z:Real): RGBf;
  public
    xt,yt,zt: table_func;
    canvas: TCanvas;
    XLeft,XRight,YTop,YBottom :Integer;
    desat :Real; //насколько мы "забеляем" спектр ради точности
    //воспроизведения
    sat: Real; //насколько мы увеличиваем яркость цветов
    procedure draw;
    function gamma_func(x: Real): Real;
    function gamma(x:Real): Integer;
    function ColorFromCCT(T: Real) :TColor;
    function ColorFromWL(wl: Real) :TColor;
    function ColorFromSpectrum(sp: table_func) :TColor;
    function ColorFromYxY(L,x,y: Real): TColor;
    function ColorFromNormedXYZ(x,y,z: Real): TColor;
    function Spectrum2XYZ(sp: table_func): CIE1931;

    procedure AddRGB(X: Real; r:Real; g:Real; b:Real);
    procedure AddMonochr(X:Real; lambda: Real); overload;
    procedure AddMonochr(X,lambda,coeff: Real); overload;
    procedure AddCCT(X:Real; CCT: Real);
    function ColorFromTable(X: Real): TColor;
    function ColorOfRGBSum: TColor;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
end;

var
  Xt,Yt,Zt: table_func;

implementation

function TSpectrum.radiant_power: Real;
begin
  Result:=integrate;
end;

function TSpectrum.luminance: Real;
var tmp: table_func;
begin
  tmp:=table_func.Create;
  tmp.assign(self);
  tmp.multiply(yt);
  Result:=683*tmp.integrate;
  tmp.Free;
end;

function TSpectrum.average_wavelength: Real;
var tmp: table_func;
    i: Integer;
begin
  tmp:=table_func.Create;
  tmp.assign(self);
  for i:=0 to Length(tmp.X)-1 do
    tmp.Y[i]:=tmp.Y[i]*tmp.X[i];
  Result:=tmp.integrate/integrate;
  tmp.Free;
end;

function TSpectrum.quants: Real;
begin
  Result:=radiant_power*average_wavelength*5.30786e15;
end;

procedure TSpectrum.NormalizeByPower(pow: Real);
begin
  multiply(pow/radiant_power);
end;

procedure TSpectrum.NormalizeByLuminance(lum: Real);
begin
  multiply(lum/luminance);
end;

procedure TSpectrum.NormalizeByQuants(amount: Real);
begin
  multiply(amount/quants);
end;



constructor colorimetry_funcs.Create;
begin
  inherited Create;
  xt:=table_func.Create('data/colorimetry/x_vs_lambda.txt');
  yt:=table_func.Create('data/colorimetry/y_vs_lambda.txt');
  zt:=table_func.Create('data/colorimetry/z_vs_lambda.txt');
  rt:=table_func.Create;
  bt:=table_func.Create;
  gt:=table_func.Create;
  desat:=0;
  sat:=1;
end;

destructor colorimetry_funcs.Destroy;
begin
  xt.Free;
  yt.Free;
  zt.Free;
  rt.Free;
  gt.Free;
  bt.Free;
  inherited Destroy;
end;

function colorimetry_funcs.gamma_func(x: Real):Real;
begin
  if x<0.0031308 then gamma_func:=x*12.92
  else gamma_func:=(1+0.055)*power(x,1/2.4)-0.055;
end;

function colorimetry_funcs.gamma(x: Real): Integer;
begin
  if x<0 then gamma:=0
  else if x<0.0031308 then gamma:=Round(x*12.92*255)
    else
      if x<=1 then gamma:=Round(((1+0.055)*power(x,1/2.4)-0.055)*255)
      else gamma:=255;
end;

function colorimetry_funcs.XYZ2RGB(x:Real;y:Real;z:Real): RGBf;
var t:Real;
begin
  t:=3.063*x-1.393*y-0.476*z;
  if t<0 then t:=0;
  XYZ2RGB.R:=t;
  t:=-0.969*x+1.876*y+0.042*z;
  if t<0 then t:=0;
  XYZ2RGB.G:=t;
  t:=0.068*x-0.229*y+1.069*z;
  if t<0 then t:=0;
  XYZ2RGB.B:=t;
end;

function colorimetry_funcs.XYZ2RGB_no_limit(x:Real;y:Real;z:Real): RGBf;
begin
  XYZ2RGB_no_limit.R:=3.063*x-1.393*y-0.476*z;
  XYZ2RGB_no_limit.G:=-0.969*x+1.876*y+0.042*z;
  XYZ2RGB_no_limit.B:=0.068*x-0.229*y+1.069*z;
end;

function colorimetry_funcs.ColorFromCCT(T: Real) :TColor;
var maxlength,xrel: Real;
  xi,yi,zi: Real;
  sum,val: Real;
  i: Integer;
  col: RGBf;
begin
  maxlength:=2896000/T;
  xi:=0;
  yi:=0;
  zi:=0;
  for i:=380 to 780 do begin
    xrel:=i/maxlength;
    val:=142.32*power(xrel,-5)/(exp(4.9651/xrel)-1);
    xi:=xi+val*xt[i];
    yi:=yi+val*yt[i];
    zi:=zi+val*zt[i];
  end;
  sum:=xi+yi+zi;
//  col:=XYZ2RGB(xi,yi,zi);
  xi:=xi/sum;
  yi:=yi/sum;
//это нормировка по максимуму

  col:=XYZ2RGB(xi,yi,1-xi-yi);
  sum:=max(col.R,max(col.G,col.B));

//    sum:=col.R+col.G+col.B;

  ColorFromCCT:=RGB(Round(col.R/sum*255),Round(col.G/sum*255),Round(col.B/sum*255));

end;

function colorimetry_funcs.ColorFromWL(wl: Real) :TColor;
var temp: RGBf;
    m: Real;
begin
//я уже не помню, что это за хрень...
//ааа, понял: просто из длины волны (wavelength)
//ладно, напишем тогда
temp:=XYZ2RGB(xt[wl],yt[wl],zt[wl]);
//все, вспомнил: в этих функциях не было гамма-функции и параметров забеления/насыщения.
//ща добавим
m:=max(temp.R,max(temp.G,temp.B));
ColorFromWL:=RGB(gamma(temp.R/m),gamma(temp.G/m),gamma(temp.B/m));
end;


function colorimetry_funcs.ColorFromSpectrum(sp: table_func) :TColor;
var temp: table_func;
    x,y,z,m: Real;
    t_rgb: rgbf;
begin
//из заданного спектра получаем цвет
//воспользуемся умножением и интегрированием функций из table_func_lib
temp:=table_func.Create;
temp.assign(xt);
temp.multiply(sp);
x:=temp.integrate;

temp.Clear;
temp.assign(yt);
temp.multiply(sp);
y:=temp.integrate;

temp.Clear;
temp.assign(zt);
temp.multiply(sp);
z:=temp.integrate;
temp.Free;

m:=x+y+z;
if m<=0 then ColorFromSpectrum:=clBlack
else begin
  x:=x/m;
  y:=y/m;
  t_rgb:=XYZ2RGB(x,y,1-x-y);
  m:=max(t_rgb.R,max(t_rgb.G,t_rgb.B));
  ColorFromSpectrum:=RGB(gamma(t_rgb.R/m),gamma(t_rgb.G/m),gamma(t_rgb.B/m));
end;
end;

procedure colorimetry_funcs.AddRGB(X:Real; r:Real; g:Real; b:Real);
begin
  rt.addpoint(X,r);
  gt.addpoint(X,g);
  bt.addpoint(X,b);
end;

procedure colorimetry_funcs.AddMonochr(x,lambda: Real);
var temp: RGBf;
begin
  temp:=XYZ2RGB_no_limit(xt[lambda],yt[lambda],zt[lambda]);
  rt.addpoint(X,temp.R);
  gt.addpoint(X,temp.G);
  bt.addpoint(X,temp.B);
end;


procedure colorimetry_funcs.AddMonochr(X,lambda,coeff: Real);
var temp: RGBf;
begin
  temp:=XYZ2RGB_no_limit(coeff*xt[lambda],coeff*yt[lambda],coeff*zt[lambda]);
  rt.addpoint(X,temp.R);
  gt.addpoint(X,temp.G);
  bt.addpoint(X,temp.B);
end;

procedure colorimetry_funcs.AddCCT(X: Real;CCT:Real);
var maxlength,xrel: Real;
  xi,yi,zi: Real;
  val: Real;
  i: Integer;
  col: RGBf;
  multiplier: Real;
begin
  maxlength:=2896000/CCT;
  //можно еще умножить на 142.32 и на 1.04e-11 вт/см^3. Но зачем...
  multiplier:=power(CCT,5);
  xi:=0;
  yi:=0;
  zi:=0;
  for i:=380 to 780 do begin
    xrel:=i/maxlength;
    val:=multiplier*power(xrel,-5)/(exp(4.9651/xrel)-1);
    xi:=xi+val*xt[i];
    yi:=yi+val*yt[i];
    zi:=zi+val*zt[i];
  end;
//  sum:=xi+yi+zi;
//  xi:=xi/sum;
//  yi:=yi/sum;
  col:=XYZ2RGB_no_limit(xi,yi,zi);
//  col:=XYZ2RGB_no_limit(xi,yi,1-xi-yi);
  rt.addpoint(X,col.R);
  gt.addpoint(X,col.G);
  bt.addpoint(X,col.B);
end;

procedure colorimetry_funcs.draw;
var i,l: Integer;
    r0,g0,b0: Real;
    mi,ma: Real;
    arg: Real;
begin
  l:=XRight-XLeft;
  mi:=-min(rt.ymin,min(gt.ymin,bt.ymin))*desat;
  ma:=max(rt.ymax,max(gt.ymax,bt.ymax))*sat+mi;
  for i:=0 to l do begin
    arg:=rt.xmin+i*(rt.xmax-rt.xmin)/l;
    r0:=rt[arg]+mi;
    if r0<0 then r0:=0;
    g0:=gt[arg]+mi;
    if g0<0 then g0:=0;
    b0:=bt[arg]+mi;
    if b0<0 then b0:=0;
    r0:=gamma_func(r0/ma)*255;
    g0:=gamma_func(g0/ma)*255;
    b0:=gamma_func(b0/ma)*255;
    if r0>255 then begin g0:=g0*255/r0; b0:=b0*255/r0; r0:=255; end;
    if g0>255 then begin r0:=r0*255/g0; b0:=b0*255/g0; g0:=255; end;
    if b0>255 then begin r0:=r0*255/b0; g0:=g0*255/b0; b0:=255; end;

    canvas.Pen.Color:=RGB(round(r0),round(g0),round(b0));
    canvas.MoveTo(i+XLeft,YTop);
    canvas.LineTo(i+xleft,YBottom);
  end;
end;

function colorimetry_funcs.ColorFromTable(X: Real): TColor;
var r0,g0,b0: Real;
    mi,ma: Real;
begin
  mi:=-min(rt.ymin,min(gt.ymin,bt.ymin))*desat;
  ma:=max(rt.ymax,max(gt.ymax,bt.ymax))*sat+mi;
    r0:=rt[x]+mi;
    if r0<0 then r0:=0;
    g0:=gt[x]+mi;
    if g0<0 then g0:=0;
    b0:=bt[x]+mi;
    if b0<0 then b0:=0;
    r0:=gamma_func(r0/ma)*255;
    g0:=gamma_func(g0/ma)*255;
    b0:=gamma_func(b0/ma)*255;
    if r0>255 then begin g0:=g0*255/r0; b0:=b0*255/r0; r0:=255; end;
    if g0>255 then begin r0:=r0*255/g0; b0:=b0*255/g0; g0:=255; end;
    if b0>255 then begin r0:=r0*255/b0; g0:=g0*255/b0; b0:=255; end;
    ColorFromTable:=RGB(round(r0),round(g0),round(b0));


end;

procedure colorimetry_funcs.Clear;
begin
  rt.Clear;
  gt.Clear;
  bt.Clear;
end;

function colorimetry_funcs.ColorOfRGBSum: TColor;
var r,g,b,m: Real;
    ri,gi,bi: Integer;
begin
  r:=rt.integrate;
  g:=gt.integrate;
  b:=bt.integrate;
  if r<0 then r:=0;
  if g<0 then g:=0;
  if b<0 then b:=0;
  m:=max(r,max(g,b));
  ri:=gamma(r/m);
  gi:=gamma(g/m);
  bi:=gamma(b/m);
  ColorOfRGBSum:=RGB(ri,gi,bi);
end;

function colorimetry_funcs.ColorFromYxy(L,x,y: Real): TColor;
var t_rgb: rgbf;
    m: Real;
begin
  t_rgb:=XYZ2RGB(x,y,1-x-y);
  m:=max(t_rgb.R,max(t_rgb.G,t_rgb.B));
  ColorFromYxy:=RGB(gamma(t_rgb.R/m),gamma(t_rgb.G/m),gamma(t_rgb.B/m));

end;

function colorimetry_funcs.ColorFromNormedXYZ(X,Y,Z: Real): TColor;
//y=1 должно преобр в 255
var t_rgb: rgbf;
begin
  t_rgb:=XYZ2RGB(X,Y,Z);
  ColorFromNormedXYZ:=RGB(gamma(t_rgb.R),gamma(t_rgb.G),gamma(t_rgb.B));
end;

function colorimetry_funcs.Spectrum2XYZ(sp: table_func): CIE1931;
var temp: table_func;
begin
//из заданного спектра получаем цвет
//воспользуемся умножением и интегрированием функций из table_func_lib
temp:=table_func.Create;
temp.assign(xt);
temp.multiply(sp);
Spectrum2XYZ.X:=temp.integrate;

temp.Clear;
temp.assign(yt);
temp.multiply(sp);
spectrum2XYZ.Y:=temp.integrate;

temp.Clear;
temp.assign(zt);
temp.multiply(sp);
spectrum2XYZ.Z:=temp.integrate;
temp.Free;
end;


initialization
  xt:=table_func.Create('data/colorimetry/x_vs_lambda.txt');
  yt:=table_func.Create('data/colorimetry/y_vs_lambda.txt');
  zt:=table_func.Create('data/colorimetry/z_vs_lambda.txt');
finalization
  xt.Free;
  yt.Free;
  zt.Free;
end.
