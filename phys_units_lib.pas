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

uses classes,VariantWrapper,ConvUtils,sysUtils;

type

TBaseFamilyEntry=record
  ConvFamily: TConvFamily;  //ссылка на description здесь же
  BaseConvType: TConvType;  //а все из-за базовой ед. изм времени 1 сутки в StdConvs
  isAffine: boolean;  //false: векторная величина, true: афинная
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
      fCount: Integer;
      fMultiplier: Real;
      fIsAffine: Boolean;
      procedure Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
      function MergeMul(value: TUnitsWithExponent; i,j: Integer): Real;
      function MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
      function ShowSomething(proc: TShowName): string;
    protected
      procedure AddBaseUnit(ConvType: TConvType; Exponent: Real);
      function AddArbitraryUnit(ConvType: TConvType; Exponent: Real): Real;
    public
      procedure Clear;
      procedure Assign(source: TPersistent); override;
      
      function TakeFromString(formula: string): Real;
      procedure DoPower(Exponent: Real);
      function SameFamily(value: TUnitsWithExponent): Boolean;
      function FindMultiplier(value: TUnitsWithExponent): Real;
      function Multiply(value: TUnitsWithExponent): Real;
      function Divide(right: TUnitsWithExponent): Real;
      function AsString: string;
      function ShowFormula: string;
      property isAffine: Boolean read fIsAffine;
  end;
//пока что здесь - конкретный тип, для представления размерности данных
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TConvType;
    protected
      instance: Variant; //та переменная, которую мы оборачиваем
    public
      constructor Create(text: string); overload;
      procedure Assign(source: TPersistent); overload; override;
      procedure Assign(str: string); reintroduce; overload;
      procedure Negate; override; //взять обратный знак
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoSubtract(Right: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoDivide(Right: TAbstractWrapperData); override;
      function AsString: string; override;
  end;

  TDerivedFamilyEntry=class
  public
    ConvFamily: TConvFamily;  //всем нужна семья!
    BaseConvType: TConvType;  //увы, в библиотеке ConvUtils нельзя из Family сослаться на базовую ед. измерения
    formula: TUnitsWithExponent;
    constructor Create;
    destructor Destroy; override;
  end;

  TVariantWithUnitType = class (TAbstractWrapperVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

  EPhysUnitError = class (Exception);

procedure RegisterBaseConversionFamily(Family: TConvFamily; BaseType: TConvType; letter: string; isAffine: boolean=false);
//уже была TConvFamily (например, из StdConvs), хотим занести ее в наш реестр
//procedure RegisterBaseConversionFamily(
procedure RegisterDerivedConversionFamily(Family: TConvFamily; BaseType: TConvType; formula: string); overload;
//этот вариант для регистрации вручную, formula - что-то вроде 'M*L/T^2'
function RegisterDerivedConversionFamily(formula: TUnitsWithExponent): TDerivedFamilyEntry; overload;

function ConvTypeToBaseFamilyLetter(const value: TConvType): string;

//тип VariantWithUnit
function VariantWithUnit: TVarType;

//конструкторы
procedure VarWithUnitCreateInto(var ADest: Variant; const AData: TVariantWithUnit);
function VarWithUnitCreate(text: string): Variant;
function TryVarWithUnitCreate(text: string; out Res: Variant): boolean;



implementation

uses StdConvs,streamable_conv_units,math,simple_parser_lib,VarCmplx,strUtils;

var BaseFamilyEntries: array of TBaseFamilyEntry;
    DerivedFamilyEntries: array of TDerivedFamilyEntry;
    VarWithUnitType: TVariantWithUnitType;

procedure RegisterBaseConversionFamily(Family: TConvFamily; BaseType: TConvType; letter: string; isAffine: boolean=false);
var L: Integer;
begin
  L:=Length(BaseFamilyEntries);
  SetLength(BaseFamilyEntries,L+1);
  BaseFamilyEntries[L].ConvFamily:=Family;
  BaseFamilyEntries[L].BaseConvType:=BaseType;
  BaseFamilyEntries[L].letter:=letter;
  BaseFamilyEntries[L].isAffine:=isAffine;
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
    Raise EPhysUnitError.CreateFMT('ConvTypeToBaseFamilyLetter: family %s not found among base families',[ConvFamilyToDescription(ConvTypeToFamily(value))]);
end;

function BaseFamilyLetterToConvFamily(letter: string): TConvFamily;
var i: Integer;
begin
  for i:=0 to Length(BaseFamilyEntries)-1 do
    if BaseFamilyEntries[i].letter=letter then begin
      Result:=BaseFamilyEntries[i].ConvFamily;
      Exit;
    end;
  Raise EPhysUnitError.CreateFmt('BaseFamilyLetterToConvFamily: letter %s not found',[letter]);
end;

procedure RegisterDerivedConversionFamily(Family: TConvFamily; BaseType: TConvType; formula: string);
var L: Integer;
begin
  L:=Length(DerivedFamilyEntries);
  SetLength(DerivedFamilyEntries,L+1);
  DerivedFamilyEntries[L]:=TDerivedFamilyEntry.Create;
  DerivedFamilyEntries[L].formula.TakeFromString(formula);
  DerivedFamilyEntries[L].ConvFamily:=Family;
  DerivedFamilyEntries[L].BaseConvType:=BaseType;
end;

function RegisterDerivedConversionFamily(formula: TUnitsWithExponent): TDerivedFamilyEntry;
var L: Integer;
begin
  L:=Length(DerivedFamilyEntries);
  SetLength(DerivedFamilyEntries,L+1);
  Result:=TDerivedFamilyEntry.Create;
  Result.ConvFamily:=RegisterConversionFamily(formula.ShowFormula);
  Result.BaseConvType:=RegisterConversionType(Result.ConvFamily,formula.AsString,1);
  Result.formula.Assign(formula);
  DerivedFamilyEntries[L]:=Result;
end;

function IndexOfDerivedFamily(Family: TConvFamily): Integer;
var i: Integer;
begin
  for i:=0 to Length(DerivedFamilyEntries)-1 do
    if DerivedFamilyEntries[i].ConvFamily=Family then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function FindPhysUnit(ConvType: TConvType): TUnitsWithExponent;
var i: Integer;
    ConvFamily: TConvFamily;
begin
  Result:=TUnitsWithExponent.Create;
  ConvFamily:=ConvTypeToFamily(ConvType);
  for i:=0 to Length(BaseFamilyEntries)-1 do
    if BaseFamilyEntries[i].ConvFamily=ConvFamily then begin
      Result.AddBaseUnit(ConvType,1);
      Exit;
    end;
  //среди базовых не нашли, поищем в производных
  for i:=0 to Length(DerivedFamilyEntries)-1 do
    if DerivedFamilyEntries[i].ConvFamily=ConvFamily then begin
      Result.Assign(DerivedFamilyEntries[i].formula);
      Exit;
    end;
  Raise EPhysUnitError.CreateFMT('Couldn''t find unit %s',[ConvTypeToDescription(ConvType)]);
end;

function FormulaToConvType(formula: TUnitsWithExponent): TConvType;
var i: Integer;
begin
  if (formula.fCount=1) and (formula.Exponents[0]=1) then begin  //базовая величина
    for i:=0 to Length(BaseFamilyEntries) do
      if BaseFamilyEntries[i].ConvFamily=ConvTypeToFamily(formula.UnitTypes[0]) then begin
        Result:=BaseFamilyEntries[i].BaseConvType;
        Exit;
      end
  end
  else
    for i:=0 to Length(DerivedFamilyEntries)-1 do
      if DerivedFamilyEntries[i].formula.SameFamily(formula) then begin
        Result:=DerivedFamilyEntries[i].BaseConvType;
        Exit;
      end;
  //если не найдено подх. семейства - мы создадим такое семейство и такой юнит!
  Result:=RegisterDerivedConversionFamily(formula).BaseConvType;
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
procedure TUnitsWithExponent.Clear;
begin
  fCount:=0;
  SetLength(UnitTypes,0);
  SetLength(Exponents,0);
end;

procedure TUnitsWithExponent.AddBaseUnit(ConvType: TConvType; Exponent: Real);
begin
  inc(fCount);
  if Length(UnitTypes)<fCount then begin
    SetLength(UnitTypes,Length(UnitTypes)*2+1);
    SetLength(Exponents,Length(Exponents)*2+1);
  end;
  UnitTypes[fCount-1]:=ConvType;
  Exponents[fCount-1]:=Exponent;
end;

function TUnitsWithExponent.AddArbitraryUnit(ConvType: TConvType; Exponent: Real): Real;
var f: TConvFamily;
    i: Integer;
    u: TUnitsWithExponent;
begin
  f:=ConvTypeToFamily(ConvType);
  i:=IndexOfBaseFamily(f);
  if i>=0 then begin
    u:=TUnitsWithExponent.Create;
    try
      u.AddBaseUnit(BaseFamilyEntries[i].BaseConvType,Exponent);
      Multiply(u);
      if ConvType=BaseFamilyEntries[i].BaseConvType then
        Result:=1
      else
        Result:=Power(Convert(1,ConvType,BaseFamilyEntries[i].BaseConvType),Exponent);
    finally
      u.Free;
    end;
  end
  else begin
    //скребем по derived, находим и помножаем на его unit
    i:=IndexOfDerivedFamily(f);
    if i=-1 then Raise EPhysUnitError.CreateFmt('Family %s is neither base nor derived',[ConvFamilyToDescription(f)]);
    u:=TUnitsWithExponent.Create;
    try
      u.Assign(DerivedFamilyEntries[i].formula);
      u.DoPower(Exponent); //возвести в нужную степень
      Multiply(u);  //в этих формулах не должны появляться новые множители
      if ConvType=DerivedFamilyEntries[i].BaseConvType then
        Result:=1
      else
        Result:=Power(Convert(1,ConvType,DerivedFamilyEntries[i].BaseConvType),Exponent);
    finally
      u.Free;
    end;
  end;
end;

procedure TUnitsWithExponent.DoPower(Exponent: Real);
var i: Integer;
begin
  for i:=0 to fcount-1 do
    Exponents[i]:=Exponents[i]*Exponent;
end;

function TUnitsWithExponent.TakeFromString(formula: string): Real;
var p: TSimpleParser;
    term: string;
    convType: TConvType;
    ch: char;
    pow: Real;
begin
  Clear;
  p:=TSimpleParser.Create(formula);
  Result:=1;
  try
    while not p.eof do begin
      term:=p.getIdent;
      if not DescriptionToConvType(term,convType) then
        Raise EPhysUnitError.CreateFmt('%s is not correct unit',[term]);
      pow:=1;
      if not p.eof then begin
        ch:=p.getChar;
        if ch='^' then begin
          pow:=p.getFloat;
          if (not p.eof) and (p.getChar<>'*') then Raise EPhysUnitError.CreateFmt('Syntax error in unit %s',[formula]);
        end;
      end;
      Result:=Result*AddArbitraryUnit(ConvType,pow);
//      чтобы он к примеру Джоули преобр. в кг*м^2/с^2
    end;
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
    fCount:=s.fCount;
  end
  else inherited Assign(source);
end;

function TUnitsWithExponent.SameFamily(value: TUnitsWithExponent): Boolean;
var i: Integer;
begin
  if fcount=value.fCount then begin
    Result:=true;
    for i:=0 to fCount-1 do
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
  for i:=0 to fCount-1 do
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
  L1:=fcount;
  L2:=value.fCount;
  SetLength(ResultUnits,L1+L2); //наихудший сценарий
  SetLength(ResultExponents,L1+L2);
  while (i<L1) and (j<L2) do begin
    first:=ConvTypeToFamily(UnitTypes[i]);
    second:=ConvTypeToFamily(value.UnitTypes[j]);
    if first=second then begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=proc(value,i,j);
      inc(i);
      inc(j);
      inc(k);
    end
    else if first>second then begin
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
  while i<L1 do begin //хвосты доделываем
    ResultUnits[k]:=UnitTypes[i];
    ResultExponents[k]:=Exponents[i];
    inc(i);
    inc(k);
  end;
  while j<L2 do begin //и еще один хвост
    ResultUnits[k]:=value.UnitTypes[j];
    ResultExponents[k]:=value.Exponents[j];
    inc(j);
    inc(k);
  end;
  SetLength(ResultUnits,k);
  SetLength(ResultExponents,k);
  UnitTypes:=Copy(ResultUnits);
  Exponents:=Copy(ResultExponents);
  fCount:=k;
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
  for i:=0 to fCount-1 do begin
    if Exponents[i]=1 then
      Result:=Result+proc(UnitTypes[i])
    else begin
      Result:=Result+'('+proc(UnitTypes[i])+')^';
      if Exponents[i]<0 then
        Result:=Result+'('+FloatToStr(Exponents[i])+')'
      else
        Result:=Result+FloatToStr(Exponents[i]);
    end;
    if i<fCount-1 then Result:=Result+'*';
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
constructor TVariantWithUnit.Create(text: string);
begin
  Create;
  Assign(text);
end;

procedure TVariantWithUnit.Assign(source: TPersistent);
var s: TVariantWithUnit absolute source;
begin
  if source is TVariantWithUnit then begin
    instance:=s.instance; //variant'ы - они умные, скопируются
    ConvType:=s.ConvType;
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
    uR,uL: TUnitsWithExponent;
begin
  if value is TVariantWithUnit then
    if ConvType=v.ConvType then
      instance:=instance+v.instance
    else begin
      uL:=FindPhysUnit(ConvType);
      uR:=FindPhysUnit(v.ConvType);
      if CompatibleConversionTypes(v.ConvType,ConvType) then
        if uR.isAffine and uL.isAffine then
          Raise EPhysUnitError.Create('Operations with affine units under construction')
        else
          instance:=instance+v.instance*Convert(1,v.ConvType,ConvType)
      else
        Raise EPhysUnitError.Create('Sorry, implicit unit conversion is also under construction');
    end
  else inherited; //не знаю пока, пригодится ли эта строчка хоть раз?
end;

procedure TVariantWithUnit.DoSubtract(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
    uR,uL: TUnitsWithExponent;
begin
  if Right is TVariantWithUnit then
    if ConvType=v.ConvType then //самое частое все-таки)
      instance:=instance-v.instance
    else begin
      //ищем соотв. запись
      uL:=FindPhysUnit(ConvType);
      uR:=FindPhysUnit(v.ConvType);
      if CompatibleConversionTypes(v.ConvType,ConvType) then
        if uR.isAffine and uL.isAffine then
          Raise EPhysUnitError.Create('Operations with affine units under construction')
        else
          instance:=instance-v.instance*Convert(1,v.ConvType,ConvType)
      else
        Raise EPhysUnitError.Create('Sorry, implicit unit conversion is also under construction');
    end
  else inherited;
end;

procedure TVariantWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
//    instance:=instance*v.instance*units.Multiply(v.Units)
  else inherited;
end;

procedure TVariantWithUnit.DoDivide(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
//    instance:=instance/v.instance*units.Divide(v.Units)
  else inherited;
end;

procedure TVariantWithUnit.Assign(str: string);
var unitStr: string;
    i: Integer;
    dimension: TUnitsWithExponent;
    PrefConvType: TConvType;
begin
  i:=Length(str);
  while (i>=1) and (str[i]<>' ') do dec(i);
  if i=0 then begin
    //либо только ед. измерения, без числа,
    //либо тот или иной Variant, но мы заранее не знаем, какой.
    //в этом проблема всех этих произволов. Попробуем в комплексную вел. преобразовать что ль
    instance:=VarComplexCreate(str);
    instance:=VarComplexSimplify(instance);
    ConvType:=duUnity;  //безразм.
  end
  else begin
    //посерединке пробел, делим напополам
    instance:=VarComplexCreate(LeftStr(str,i-1));
    instance:=VarComplexSimplify(instance);
    unitStr:=RightStr(str,Length(str)-i);
    if not DescriptionToConvType(unitStr,ConvType) then begin
      //сложное выражение, нужно создать экземпляр TUnitsWithExponent,
      //скормить ему размерность, он "выплюнет" множитель, домножаем на него
      dimension:=TUnitsWithExponent.Create;
      instance:=instance*dimension.TakeFromString(unitStr);
      //а также ConvType - тот, что является базовым для данного семейства
      //возможно, семейство придется создать на ходу
      ConvType:=FormulaToConvType(dimension);
    end;
    if Assigned(preferredUnits) then begin
      PrefConvType:=preferredUnits.GetPreferredType(ConvTypeToFamily(ConvType));
      if PrefConvType<>CIllegalConvType then begin
        Instance:=Instance*Convert(1,ConvType,PrefConvType);
        ConvType:=PrefConvType;
      end;
    end;
  end;
end;

function TVariantWithUnit.AsString: string;
begin
  Result:=instance;
  Result:=Result+' '+ConvTypeToDescription(ConvType);
end;
(*
    TVariantWithUnitType
                            *)
procedure TVariantWithUnitType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  //строку преобразуем по всем правилам, а любые другие Variant'ы "оборачиваем" безразм.
  VarDataClear(Dest);
  if VarDataIsStr(Source) then
    TWrapperVarData(Dest).Data:=TVariantWithUnit.Create(VarDataToStr(Source))
  else begin
    with TWrapperVarData(Dest).Data as TVariantWithUnit do begin
      Create;
      instance:=Variant(source);
//      Units.Clear;  //безразм. величина
    end;
  end;
  Dest.VType:=VariantWithUnit;
end;

procedure TVariantWithUnitType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
  if Source.VType = VarType then begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TWrapperVarData(Source).data.AsString);
      varString:
        VarDataFromStr(Dest, TWrapperVarData(Source).data.AsString);
      else
        with TWrapperVarData(Source).Data as TVariantWithUnit do begin
//          if Units.fCount>0 then Raise Exception.CreateFmt('%s: can''t convert value with unit to non-string type',[AsString]);
          VarDataCastTo(Dest,TVarData(instance),AVarType);
        end;
    end;
  end
  else inherited; //нам дали пустой variant скорее всего
end;

(*
    Initialization
                        *)

procedure RegisterStandartUnits;
begin
  RegisterBaseConversionFamily(cbMass,muShortKilograms,'M');
  RegisterBaseConversionFamily(cbDistance,duShortMeters,'L');
  RegisterBaseConversionFamily(cbTime,tuShortSeconds,'T');
  RegisterBaseConversionFamily(cbTemperature,tuShortKelvin,'Temp',true);
  RegisterDerivedConversionFamily(cbDimensionless,duUnity,'');
  RegisterDerivedConversionFamily(cbArea,auShortSqMeters,'m^2');
  RegisterDerivedConversionFamily(cbVolume,vuShortCubicMeters,'m^3');
  RegisterDerivedConversionFamily(cbPressure,puPa,'kg*m^-1*s^-2');
  RegisterDerivedConversionFamily(cbVolumetricFlowRate,vcuM3PerSec,'m^3*s^-1');
  RegisterDerivedConversionFamily(cbFrequency,fuHz,'s^-1');
  RegisterDerivedConversionFamily(cbPower,powWatt,'kg*m^2*s^-3');
  //напряжения и токи сюда же
end;

procedure FreeUnits;
var i: Integer;
begin
  for i:=0 to Length(DerivedFamilyEntries)-1 do
    DerivedFamilyEntries[i].Free;
end;

function VariantWithUnit: TVarType;
begin
  Result:=VarWithUnitType.VarType;
end;

(*
    конструкторы
                  *)
procedure VarWithUnitCreateInto(var ADest: Variant; const Adata: TVariantWithUnit);
begin
  VarClear(ADest);
  TWrapperVarData(ADest).VType:=VariantWithUnit;
  TWrapperVarData(ADest).Data:=Adata;
end;

function VarWithUnitCreate(text: string): Variant;
begin
  VarWithUnitCreateInto(Result,TVariantWithUnit.Create(text));
end;

function TryVarWithUnitCreate(text: string; out Res: Variant): boolean;
begin
  Result:=false;
  try
    VarWithUnitCreateInto(Res,TVariantWithUnit.Create(text));
    Result:=true;
  except
    on E: Exception do
      Res:=E.Message;
  end;
end;


initialization
  RegisterStandartUnits;
  VarWithUnitType:=TVariantWithUnitType.Create;

finalization
  FreeAndNil(VarWithUnitType);
  FreeUnits;


end.
