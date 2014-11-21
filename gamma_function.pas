unit gamma_function;

interface
uses math,graphics,windows;

type

RGBColor=record
  case Integer of
  0: (pad: byte;
      B: byte;
      G: byte;
      R: byte);
  1: (Color: TColor);
end;

PRGBScanline = ^TRGBScanline;
TRGBScanline=array [0..32767] of RGBColor;

PColorScanline = ^TColorScanline;
TColorScanline=array [0..32767] of TColor;

function Real_from_monochrome (c: TColor): Real;
function monochrome_from_Real (x: Real): TColor;
function gamma(x: Real): Integer;
//function inverse_gamma(x: Integer): Real;

function ColorFromReals(R,G,B: Real): TColor;

var inverseGammaTable: Array [0..255] of Real;

implementation

var gamma_table: Array [0..3333] of Integer;

//для преобр. числа от 0 до 1 (1 - макс интенсивность) в целое от 0 до 255 - яркость пиксела
function gamma(x: Real): Integer;
begin
  if x>0 then begin
    if x<=1 then begin
(*
      if x>0.0031308 then Result:=Round(269.025*Exp(0.4166666667*Ln(x))-14.025)
      else Result:=Round(x*3294.6);
      *)
      Result:=gamma_table[Round(x/0.0003)];
    end
    else Result:=255;
  end
  else Result:=0;
end;

function honest_gamma(x: Real): Integer;
begin
  if x>0 then begin
    if x<=1 then begin
      if x>0.0031308 then Result:=Round(269.025*Exp(0.4166666667*Ln(x))-14.025)
      else Result:=Round(x*3294.6);
    end
    else Result:=255;
  end
  else Result:=0;
end;


function honest_inverse_gamma(x: Integer): Real;
begin
  assert(x>=0);
  assert(x<=255);
  if x<11 then Result:=x*0.000302341
    else Result:=power((x/256+0.055)/1.055,2.4);
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
  result:=InverseGammaTable[i];
end;

function ColorFromReals(R,G,B: Real): TColor;
begin
  RGBColor(Result).R:=gamma(R);
  RGBColor(Result).G:=gamma(G);
  RGBColor(Result).B:=gamma(B);
end;

procedure ComputeTable;
var i: Integer;

begin
  for i:=0 to 3333 do
    gamma_table[i]:=honest_gamma(i*0.0003);
  for i:=0 to 255 do
    InverseGammaTable[i]:=honest_inverse_gamma(i);
end;

initialization
  ComputeTable;


end.
