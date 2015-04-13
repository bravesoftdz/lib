unit table_func_with_units;

interface

uses table_func_lib,ConvUtils;

type

TVariantTableFunc = class (table_func)
  private
    fXUnitConv,fYUnitConv: TConvType;
    function GetVariantValue(xi: Variant): Variant;
    function GetInverseVariantValue(yi: Variant): Variant;
    procedure SetXUnit(value: string);
    procedure SetYUnit(value: string);
  public
    function GetInverse: TVariantTableFunc; reintroduce; overload;
    property value[xi: Variant]: Variant read GetVariantValue; default;
    property inverse[yi: Variant]: Variant read GetInverseVariantValue;
  published
    property XUnit: string read fXUnit write SetXUnit;
    property YUnit: string read fYUnit write SetYUnit;
end;

implementation
uses phys_units_lib,variants,sysUtils;

function TVariantTableFunc.GetVariantValue(xi: Variant): Variant;
var axi: Real;
    tmp: Variant;
begin
  tmp:=VarWithUnitGetNumberIn(xi,fXUnitConv);
  if VarIsNumeric(tmp) then
    axi:=tmp
  else Raise Exception.Create('argument of table function should be real');
  Result:=VarWithUnitCreateFromVariant(GetValue(axi),fYUnitConv);
end;

function TVariantTableFunc.GetInverseVariantValue(yi: Variant): Variant;
var ayi,ax,ay: Real;
    tmp: Variant;
    i: Integer;
begin
  tmp:=VarWithUnitGetNumberIn(yi,fYUnitConv);
  if VarIsNumeric(tmp) then
    ayi:=tmp
  else Raise Exception.Create('right now we work with real-valued table functions');
  //первым делом интервал ищем

  Result:=VarWithUnitCreateFromVariant(InverseValue(ayi),fXUnitConv);
(*
  for i:=0 to count-1 do
    if Y[i]>=ayi then begin
      Result:=VarWithUnitCreateFromVariant(X[i],fXUnitConv);
      Exit;
    end;
  Raise Exception.CreateFMT('didn''t find value %s in table',[yi]);
  *)
end;

//  Result:=VarWithUnitCreateFrom


procedure TVariantTableFunc.SetXUnit(value: string);
begin
  fXUnit:=value;
  fXUnitConv:=StrToConvType(value);
end;

procedure TVariantTableFunc.SetYUnit(value: string);
begin
  fYUnit:=value;
  fYUnitConv:=StrToConvType(value);
end;

function TVariantTableFunc.GetInverse: TVariantTableFunc;
var i: Integer;
begin
  Result:=TVariantTableFunc.Create;
  Result.zero_out_of_bounds:=zero_out_of_bounds;
  Result.order:=order;
  Result.XUnit:=YUnit;
  Result.YUnit:=XUnit;
  for i:=0 to count-1 do
    Result.addpoint(Y[i],X[i]);
end;

end.
