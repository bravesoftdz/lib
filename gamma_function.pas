unit gamma_function;

interface
uses math,graphics,windows;

type
RGBColor=record
    R: byte;
    G: byte;
    B: byte;
    pad: byte;
end;

function Real_from_monochrome (c: TColor): Real;
function monochrome_from_Real (x: Real): TColor;
function gamma(x: Real): Integer;
function inverse_gamma(x: Integer): Real;

function ColorFromReals(R,G,B: Real): TColor;

implementation
//для преобр. числа от 0 до 1 (1 - макс интенсивность) в целое от 0 до 255 - яркость пиксела
function gamma(x: Real): Integer;
begin
  if x<0 then gamma:=0
  else if x<0.0031308 then gamma:=Round(x*12.92*255)
    else
      if x<=1 then gamma:=Round(((1+0.055)*power(x,1/2.4)-0.055)*255)
      else gamma:=255;
end;

function inverse_gamma(x: Integer): Real;
begin
  assert(x>=0);
  assert(x<=255);
  if x<11 then inverse_gamma:=x*0.000302341
    else inverse_gamma:=power((x/256+0.055)/1.055,2.4);
end;

function monochrome_from_Real (x:Real): TColor;
var i: Integer;
begin
  i:=gamma(x);
  result:=RGB(i,i,i);
end;

function real_from_monochrome (c: TColor): Real;
var i: Integer;
begin
  i:=RGBColor(c).G;
  result:=inverse_gamma(i);
end;

function ColorFromReals(R,G,B: Real): TColor;
begin
  RGBColor(Result).R:=gamma(R);
  RGBColor(Result).G:=gamma(G);
  RGBColor(Result).B:=gamma(B);
end;


end.
