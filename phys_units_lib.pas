unit phys_units_lib;

//вводим здесь единицы измерения, причем различаем базовые и производные,
//масштабируемые (почти все) и с нулевой точкой (температуры),
//а также нелинейные (децибелы разные и пр)

//хотелось бы сохранить связь с ConvUtils, чтобы по-новой столько не вводить
//разных единиц.
//по сути, проводим связи между TConvFamily, т.е регистрируем каждую из них с
//"формулой" типа M*L/T^2 (это сила, напр. кг*м/с^2), а потом по формуле можем найти
//нужное, на лету создать новое семейство (если не было таких) и конкр. юнит.

interface

uses classes,VariantWrapper,ConvUtils;

type

TBaseFamilyEntry=record
  ConvFamily: TConvFamily;  //ссылка на description здесь же
  letter: string;
end;  //неужели больше ничего не нужно?

  //а теперь одна из конкретных реализаций

  TUnitsWithExponent = class;
  TUnitsWithExponentMergeProc = function (value: TUnitsWithExponent; i,j: Integer) : Real of object;
  TShowName = function (const value: TConvType): string;
  TUnitTypes = array of TConvType;  //из TConvType всегда получим TConvFamily
  TExponents = array of Real;

  TUnitsWithExponent = class(TPersistent)  //хватило бы и record'а и указателей и GetMem/FreeMem,
    private
      UnitTypes: TUnitTypes;
      Exponents: TExponents;
      fMultiplier: Real;
      procedure Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
      function MergeMul(value: TUnitsWithExponent; i,j: Integer): Real;
      function MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
      function ShowSomething(proc: TShowName): string;
    public
      procedure Assign(formula: string); reintroduce; overload;
      procedure Assign(source: TPersistent); overload; override;
      function SameFamily(value: TUnitsWithExponent): Boolean;
      function FindMultiplier(value: TUnitsWithExponent): Real;
      function Multiply(value: TUnitsWithExponent): Real;
      function Divide(right: TUnitsWithExponent): Real;
      function AsString: string;
      function ShowFormula: string;
  end;
//пока что здесь - конкретный тип, для представления размерности данных
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      Units: TUnitsWithExponent;
    protected
      instance: Variant; //та переменная, которую мы оборачиваем
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(source: TPersistent); override;
      procedure Negate; override; //взять обратный знак
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoSubtract(Right: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoDivide(Right: TAbstractWrapperData); override;
  end;

  TDerivedFamilyEntry=class
  public
    ConvFamily: TConvFamily;  //всем нужна семья!
    formula: TUnitsWithExponent;
    constructor Create;
    destructor Destroy; override;
  end;

procedure RegisterBaseConversionFamily(Family: TConvFamily; letter: string);
//уже была TConvFamily (например, из StdConvs), хотим занести ее в наш реестр
//procedure RegisterBaseConversionFamily(
procedure RegisterDerivedConversionFamily(Family: TConvFamily; formula: string); overload;
//этот вариант для регистрации вручную, formula - что-то вроде 'M*L/T^2'
procedure RegisterDerivedConversionFamily(formula: TUnitsWithExponent); overload;

function ConvTypeToBaseFamilyLetter(const value: TConvType): string;

implementation

uses StdConvs,streamable_conv_units,sysUtils,math,simple_parser_lib;

var BaseFamilyEntries: array of TBaseFamilyEntry;
    DerivedFamilyEntries: array of TDerivedFamilyEntry;

procedure RegisterBaseConversionFamily(Family: TConvFamily; letter: string);
var L: Integer;
begin
  L:=Length(BaseFamilyEntries);
  SetLength(BaseFamilyEntries,L+1);
  BaseFamilyEntries[L].ConvFamily:=Family;
  BaseFamilyEntries[L].letter:=letter;
end;

function IndexOfBaseFamily(Family: TConvFamily): Integer;
var i: Integer;
begin
  for i:=0 to Length(BaseFamilyEntries)-1 do
    if BaseFamilyEntries[i].ConvFamily=Family then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function ConvTypeToBaseFamilyLetter(const value: TConvType): string;
var i: Integer;
begin
  i:=IndexOfBaseFamily(ConvTypeToFamily(value));
  if i>=0 then
    Result:=BaseFamilyEntries[i].letter
  else
    Raise Exception.CreateFMT('ConvTypeToBaseFamilyLetter: family %s not found among base families',[ConvFamilyToDescription(ConvTypeToFamily(value))]);
end;

function BaseFamilyLetterToConvFamily(letter: string): TConvFamily;
var i: Integer;
begin
  for i:=0 to Length(BaseFamilyEntries)-1 do
    if BaseFamilyEntries[i].letter=letter then begin
      Result:=BaseFamilyEntries[i].ConvFamily;
      Exit;
    end;
  Raise Exception.CreateFmt('BaseFamilyLetterToConvFamily: letter %s not found',[letter]);
end;

procedure RegisterDerivedConversionFamily(Family: TConvFamily; formula: string);
var L: Integer;
begin
  L:=Length(DerivedFamilyEntries);
  SetLength(DerivedFamilyEntries,L+1);
  DerivedFamilyEntries[L]:=TDerivedFamilyEntry.Create;
  DerivedFamilyEntries[L].formula.Assign(formula);
  DerivedFamilyEntries[L].ConvFamily:=Family;
end;

procedure RegisterDerivedConversionFamily(formula: TUnitsWithExponent);
var L: Integer;
begin
  L:=Length(DerivedFamilyEntries);
  SetLength(DerivedFamilyEntries,L+1);
  DerivedFamilyEntries[L]:=TDerivedFamilyEntry.Create;
  DerivedFamilyEntries[L].ConvFamily:=RegisterConversionFamily(formula.ShowFormula);
  DerivedFamilyEntries[L].formula.Assign(formula);
end;


(*
  TDerivedFamilyEntry
                        *)
constructor TDerivedFamilyEntry.Create;
begin
  formula:=TUnitsWithExponent.Create;
end;

destructor TDerivedFamilyEntry.Destroy;
begin
  formula.Free;
  inherited Destroy;
end;

(*
      TUnitsWithExponent
                        *)
procedure TUnitsWithExponent.Assign(formula: string);
var p: TSimpleParser;
    i: Integer;
    s: string;
    ch: char;
    pow: Integer;
begin
  p:=TSimpleParser.Create(formula);
  try
    i:=0;
    while not p.eof do begin
      s:=p.getIdent;
      pow:=1;
      if not p.eof then begin
        ch:=p.getChar;
        if ch='^' then begin
          pow:=p.getInt;
          if (not p.eof) and (p.getChar<>'*') then Raise Exception.CreateFmt('Syntax error in dimension formula %s',[formula]);
        end;
      end;
      inc(i);
      if Length(Exponents)<i then begin
        SetLength(Exponents,Length(Exponents)*2+1);
        SetLength(UnitTypes,Length(UnitTypes)*2+1);
      end;
      UnitTypes[i-1]:=BaseFamilyLetterToConvFamily(s);
      Exponents[i-1]:=pow;
    end;
    SetLength(UnitTypes,i);
    SetLength(Exponents,i);
  finally
    p.Free;
  end;
end;

procedure TUnitsWithExponent.Assign(source: TPersistent);
var s: TUnitsWithExponent absolute source;
begin
  if source is TUnitsWithExponent then begin
    Exponents:=Copy(s.Exponents);
    UnitTypes:=Copy(s.UnitTypes);
  end
  else inherited Assign(source);
end;

function TUnitsWithExponent.SameFamily(value: TUnitsWithExponent): Boolean;
var i: Integer;
begin
  if Length(Exponents)=Length(value.Exponents) then begin
    Result:=true;
    for i:=0 to Length(Exponents)-1 do
      if (ConvTypeToFamily(UnitTypes[i])<>ConvTypeToFamily(value.UnitTypes[i])) or (Exponents[i]<>value.Exponents[i]) then begin
        Result:=false;
        Exit;
      end;
  end
  else Result:=false;
end;

function TUnitsWithExponent.FindMultiplier(value: TUnitsWithExponent): Real;
var i: Integer;
begin
  //мы уже знаем, что семейство одно и то же
  Result:=1;
  for i:=0 to Length(Exponents)-1 do
    if (UnitTypes[i]<>value.UnitTypes[i]) then
      Result:=Result*Convert(1,value.UnitTypes[i],UnitTypes[i]);
end;

procedure TUnitsWithExponent.Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
var i,j,k: Integer;
    L1,L2: Integer;
    ResultUnits: TUnitTypes;
    ResultExponents: TExponents;
    first,second: TConvFamily;
begin
  //оба списка отсортированы по нарастанию unitTypes
  //точнее, TConvFamily соотв. unitTypes
  //по сути, осуществляем слияние
  i:=0;
  j:=0;
  k:=0;
  L1:=Length(Exponents);
  L2:=Length(value.Exponents);
  SetLength(ResultUnits,L1+L2); //наихудший сценарий
  while (i<Length(Exponents)) and (j<Length(value.Exponents)) do begin
    first:=ConvTypeToFamily(UnitTypes[i]);
    second:=ConvTypeToFamily(value.UnitTypes[j]);
    if first=second then begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=proc(value,i,j);
      inc(i);
      inc(j);
      inc(k);
    end
    else if UnitTypes[i]>value.UnitTypes[j] then begin
      //UnitTypes[j] не было в исх. списке - надо добавить на подх. место
      ResultUnits[k]:=value.UnitTypes[j];
      ResultExponents[k]:=value.Exponents[j];
      inc(j);
      inc(k);
    end
    else begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=Exponents[i];
      inc(i);
      inc(k);
    end;
  end;
  SetLength(ResultUnits,k);
  SetLength(ResultExponents,k);
  UnitTypes:=Copy(ResultUnits);
  Exponents:=Copy(ResultExponents);
end;

function TUnitsWithExponent.MergeMul(value: TUnitsWithExponent; i,j: Integer): Real;
begin
  if UnitTypes[i]<>value.UnitTypes[j] then
    fMultiplier:=fMultiplier*Power(Convert(1,value.UnitTypes[j],UnitTypes[i]),value.Exponents[j]);
  Result:=Exponents[i]+value.exponents[j];
end;

function TUnitsWithExponent.MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
begin
  if UnitTypes[i]<>value.UnitTypes[j] then
    fMultiplier:=fMultiplier*Power(Convert(1,value.UnitTypes[j],UnitTypes[i]),-value.Exponents[j]);
  Result:=Exponents[i]-value.exponents[j];
end;

function TUnitsWithExponent.Multiply(value: TUnitsWithExponent): Real;
begin
  fMultiplier:=1;
  Merge(value,MergeMul);
  Result:=fMultiplier;
end;

function TUnitsWithExponent.Divide(right: TUnitsWithExponent): Real;
begin
  fMultiplier:=1;
  Merge(right,MergeDiv);
  Result:=fMultiplier;
end;

function TUnitsWithExponent.ShowSomething(proc: TShowName): string;
var i: Integer;
begin
  Result:='';
  for i:=0 to Length(Exponents)-1 do begin
    if Exponents[i]=1 then
      Result:=Result+proc(UnitTypes[i])
    else begin
      Result:=Result+'('+proc(UnitTypes[i])+')^';
      if Exponents[i]<0 then
        Result:=Result+'('+FloatToStr(Exponents[i])+')'
      else
        Result:=Result+FloatToStr(Exponents[i]);
    end;
    if i<Length(Exponents)-1 then Result:=Result+'*';
  end;
end;

function TUnitsWithExponent.ShowFormula: string;
begin
  Result:=ShowSomething(ConvTypeToBaseFamilyLetter);
end;

function TUnitsWithExponent.AsString: string;
begin
  result:=ShowSomething(ConvTypeToDescription);
end;

(*
      TVariantWithUnit
                          *)
constructor TVariantWithUnit.Create;
begin
  units:=TUnitsWithExponent.Create;
end;

destructor TVariantWithUnit.Destroy;
begin
  units.Free;
  inherited Destroy;
end;

procedure TVariantWithUnit.Assign(source: TPersistent);
var s: TVariantWithUnit absolute source;
begin
  if source is TVariantWithUnit then begin
    instance:=s.instance; //variant'ы - они умные, скопируются
    units.Assign(s.Units);
  end
  else inherited Assign(source);
end;

procedure TVariantWithUnit.Negate;
begin
  //на размерность не влияет вообще
  instance:=-instance;
end;

procedure TVariantWithUnit.DoAdd(value: TAbstractWrapperData);
var v: TVariantWithUnit absolute value;
begin
  if value is TVariantWithUnit then begin
    //размерности должны строго совпадать
    //видимо, самое простое - держать массивы отсортированными по TConvType
    if units.SameFamily(v.Units) then
      instance:=instance+v.instance*units.FindMultiplier(v.Units)
    else
      Raise Exception.CreateFMT('TVariantWithUnit.DoAdd: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited; //не знаю пока, пригодится ли эта строчка хоть раз?
end;

procedure TVariantWithUnit.DoSubtract(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then begin
    if units.SameFamily(v.Units) then
      instance:=instance-v.instance*units.FindMultiplier(v.Units)
    else
      Raise Exception.CreateFmt('TVariantWithUnit.DoSub: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited;
end;

procedure TVariantWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
    instance:=instance*v.instance*units.Multiply(v.Units)
  else inherited;
end;

procedure TVariantWithUnit.DoDivide(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
    instance:=instance/v.instance*units.Divide(v.Units)
  else inherited;
end;



(*
    Initialization
                        *)

procedure RegisterStandartUnits;
begin
  RegisterBaseConversionFamily(cbDistance,'L');
  RegisterBaseConversionFamily(cbMass,'M');
  RegisterBaseConversionFamily(cbTime,'T');
  RegisterDerivedConversionFamily(cbArea,'L^2');
  RegisterDerivedConversionFamily(cbVolume,'L^3');
  RegisterDerivedConversionFamily(cbPressure,'M*L^-1*T^-2');
  RegisterDerivedConversionFamily(cbVolumetricFlowRate,'L^3*T^-1');
  RegisterDerivedConversionFamily(cbFrequency,'T^-1');
  RegisterDerivedConversionFamily(cbPower,'M*L^2*T^-3');
  //напряжения и токи сюда же
end;

procedure FreeUnits;
var i: Integer;
begin
  for i:=0 to Length(DerivedFamilyEntries)-1 do
    DerivedFamilyEntries[i].Free;
end;


initialization
  RegisterStandartUnits;

finalization
  FreeUnits;


end.
