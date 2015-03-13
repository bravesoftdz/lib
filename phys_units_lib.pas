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

uses classes,VariantWrapper,ConvUtils,sysUtils,linear_eq,streaming_class_lib;

type
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

      function TakeFromString(formula: string; out modifier: string): Real;
      procedure DoPower(Exponent: Real);
      function SameFamily(value: TUnitsWithExponent): Boolean;
      procedure Multiply(value: TUnitsWithExponent);
      procedure Divide(right: TUnitsWithExponent);
      function AsString: string;
      function ShowFormula: string;
      function isAffine: Boolean;
  end;

  TAbstractSavedConvFamily=class(TStreamingClass)
  private
    flang: string;
    fStrBaseUnit: string;  //вообще, TConvType умеет записываться символьно,
    //однако на момент считывания BaseUnit мы еще можем не знать такого юнита
    fBaseConvType: TConvType;
    fConvFamily: TConvFamily;
    procedure ReadUnits(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property Lang: string write flang;
    property BaseUnit: string write fStrBaseUnit;
  end;

  TBaseConvFamily=class(TAbstractSavedConvFamily)
  private
    fLetter: string;
    fAffine: boolean;
  published
    property letter: string write fLetter;
    property IsAffine: Boolean read fAffine write fAffine;
  end;

  TDerivedConvFamily=class(TAbstractSavedConvFamily)
  private
    fStrFormula: string;
    fFormula: TUnitsWithExponent;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property formula: string write fStrFormula;
  end;

//пока что здесь - конкретный тип, для представления размерности данных
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TConvType;
      function IsAffine(out i: Integer): Boolean; //удобнее всего
    public
      instance: Variant; //та переменная, которую мы оборачиваем
      constructor Create(text: string); overload;
      constructor CreateFromVariant(source: Variant; aConvType: TConvType);
      procedure Assign(source: TPersistent); overload; override;
      procedure Assign(str: string); reintroduce; overload;
      procedure Negate; override; //взять обратный знак
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoInverse;
      procedure DoDivide(Right: TAbstractWrapperData); override;
      procedure DoPower(pow: Real);
      function AsString: string; override;
      procedure Conversion(DestConv: TConvType);
  end;

  TVariantWithUnitType = class (TAbstractWrapperVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

  TVariantWithUnitVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TVariantWithUnit;
    Reserved4: LongInt;
  end;

  TaffineUnitEntry = record
    ConvType: TConvType;
    BaseConvType: TConvType;
    multiplier: Real;
  end;

  TFundamentalPhysConstants = class(TComponent)
    private
      fNames: array of string;
      fDescriptions: array of string;
      fValues: array of Variant;
      fenabled: array of Boolean;
      fcount: Integer;
      fsolver: IAbstractSLEQ;
      fActuallyUsedCount: Integer;
      fActualIndex: array of Integer;
      fLang: string;
      function getName(i: Integer): string;
      function getDescription(i: Integer): string;
      function getEnabled(i: Integer): boolean;
      procedure setEnabled(i: Integer; avalue: boolean);
      function Recalculate: Boolean;
      procedure ReadData(reader: TReader);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create(Owner: TComponent); override;
      procedure Add(aname,adescr: string; aValue: Variant; aEnabled: boolean=false);
      function GetVar(i: Integer): Variant;
      function GetActualIndex(i: Integer): Integer;
      property Count: Integer read fCount;
      property Names[i: Integer]: string read getName;
      property Descriptions[i: Integer]: string read getDescription;
      property Enabled[i: Integer]: Boolean read getEnabled write setEnabled;
      property Solver: IAbstractSLEQ read fsolver;
      property ActuallyUsedCount: Integer read fActuallyUsedCount;
    published
      property Lang: string read fLang write fLang;
  end;

  TConvTypeAffine = class(TConvTypeInfo)
  private
    fOffset,fFactor: Real;
  public
    constructor Create(const AConvFamily: TConvFamily; const ADescription: string; offset,factor: Real); //reintroduce;
    function ToCommon(const AValue: Double): Double; override;
    function FromCommon(const AValue: Double): Double; override;
  end;

  EPhysUnitError = class (Exception);


  PUnitMultipliersArray = ^TUnitMultipliersArray;
  TUnitMultipliersArray = array [0..255] of Real;

  TLogConversionDetailsProc = procedure (line: string); //цвет сами придумаем

//procedure RegisterDerivedConversionFamily(Family: TConvFamily; BaseType: TConvType; formula: string); overload;
//этот вариант для регистрации вручную, formula - что-то вроде 'M*L/T^2'
function RegisterDerivedConversionFamily(formula: TUnitsWithExponent): TDerivedConvFamily;

function ConvTypeToBaseFamilyLetter(const value: TConvType): string;

//тип VariantWithUnit
function VariantWithUnit: TVarType;

//конструкторы и пр.
procedure VarWithUnitCreateInto(var ADest: Variant; const AData: TVariantWithUnit);
function VarWithUnitCreate(text: string): Variant;
function VarWithUnitCreateFromVariant(source: Variant; ConvType: TConvType): Variant;
function IsVarWithUnit(V: Variant): Boolean;
function IsDimensionless(V: Variant): boolean;
function TryVarWithUnitCreate(text: string; out Res: Variant): boolean;
function VarWithUnitConvert(source: Variant; DestConvType: TConvType): Variant; overload;
function VarWithUnitConvert(source: Variant; UnitName: string): Variant; overload;
function StrToConvType(str: string): TConvType;
function StrToConvFamily(str: string): TConvFamily;
function PrefixDescrToConvType(str: string; out CType: TConvType): boolean;
function VarWithUnitPower(source: Variant; pow: Real): Variant;

function NameToFamily(const Ident: string; var Int: Longint): Boolean;
function FamilyToName(Int: LongInt; var Ident: string): Boolean;
function NameToConv(const Ident: string; var Int: LongInt): Boolean;
function ConvToName(Int: LongInt; var Ident: string): Boolean;

procedure InitializePhysUnitLib;
procedure FinalizePhysUnitLib;

var UnityPhysConstants: TFundamentalPhysConstants;
    LogConversionDetailsProc: TLogConversionDetailsProc;
    cbDimensionless,cbAngle: TConvFamily;
    duUnity,auDMS,auRadian: TConvType;
    PhysUnitLanguage: string;
implementation

uses StdConvs,math,simple_parser_lib,VarCmplx,strUtils,variants,
  set_english_locale_if_not_sure,expression_lib,Contnrs;

var BaseFamilyEntries: TObjectList;
    DerivedFamilyEntries: TObjectList;
    VarWithUnitType: TVariantWithUnitType;
    UnitMultiplier: TUnitMultipliersArray;
    AffineUnits: array of TAffineUnitEntry;
    default_dir: string;

resourcestring
  ConvFamilyNeitherBaseNorDerived = 'Размерность %s не является ни базовой, ни производной';


function IndexOfBaseFamily(Family: TConvFamily): Integer;
var i: Integer;
begin
  for i:=0 to BaseFamilyEntries.Count-1 do
    if (BaseFamilyEntries[i] as TBaseConvFamily).fConvFamily=Family then begin
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
    Result:=TBaseConvFamily(BaseFamilyEntries[i]).fLetter
  else
    Raise EPhysUnitError.CreateFMT('ConvTypeToBaseFamilyLetter: family %s not found among base families',[ConvFamilyToDescription(ConvTypeToFamily(value))]);
  //этот код вообще не используется пока
end;

function RegisterDerivedConversionFamily(formula: TUnitsWithExponent): TDerivedConvFamily;
begin
  Result:=TDerivedConvFamily.Create(nil);
  Result.fConvFamily:=RegisterConversionFamily(formula.ShowFormula);
  Result.fBaseConvType:=RegisterConversionType(Result.fConvFamily,formula.AsString,1);
  Result.fFormula.Assign(formula);
  DerivedFamilyEntries.Add(Result);
end;

function IndexOfDerivedFamily(Family: TConvFamily): Integer;
var i: Integer;
begin
  for i:=0 to DerivedFamilyEntries.Count-1 do
    if TDerivedConvFamily(DerivedFamilyEntries[i]).fConvFamily=Family then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function PrefixDescrToConvType(str: string; out CType: TConvType): boolean;
begin
  Result:=(Length(str)>2) and (LeftStr(str,2)='мк') and DescriptionToConvType(RightStr(str,Length(str)-2),CType);
  if not Result then
    Result:=(Length(str)>1) and (UnitMultiplier[Integer(str[1])]<>0) and DescriptionToConvType(Rightstr(str,Length(str)-1),CType);
    if not Result then
      Result:=DescriptionToConvType(str,CType);
end;

function FindPhysUnit(ConvType: TConvType): TUnitsWithExponent;
var i: Integer;
    ConvFamily: TConvFamily;
begin
  Result:=TUnitsWithExponent.Create;
  ConvFamily:=ConvTypeToFamily(ConvType);
  for i:=0 to BaseFamilyEntries.Count-1 do
    if TBaseConvFamily(BaseFamilyEntries[i]).fConvFamily=ConvFamily then begin
//      Result.AddBaseUnit(ConvType,1); //а почему ConvType, а не BaseConvType?
      Result.AddBaseUnit(TBaseConvFamily(BaseFamilyEntries[i]).fBaseConvType,1);
      Exit;
    end;
  //среди базовых не нашли, поищем в производных
  for i:=0 to DerivedFamilyEntries.Count-1 do
    if TDerivedConvFamily(DerivedFamilyEntries[i]).fConvFamily=ConvFamily then begin
      Result.Assign(TDerivedConvFamily(DerivedFamilyEntries[i]).fformula);
      Exit;
    end;
  Raise EPhysUnitError.CreateFMT(ConvFamilyNeitherBaseNorDerived,[ConvTypeToDescription(ConvType)]);
  //так будет, если одну из станд. разм. не объявить в текст. файле.
end;

function FormulaToConvType(formula: TUnitsWithExponent): TConvType;
var i: Integer;
begin
  if (formula.fCount=1) and (formula.Exponents[0]=1) then begin  //базовая величина
    for i:=0 to BaseFamilyEntries.Count-1 do
      if TBaseConvFamily(BaseFamilyEntries[i]).fConvFamily=ConvTypeToFamily(formula.UnitTypes[0]) then begin
        Result:=TBaseConvFamily(BaseFamilyEntries[i]).fBaseConvType;
        Exit;
      end
  end
  else
    for i:=0 to DerivedFamilyEntries.Count-1 do
      if TDerivedConvFamily(DerivedFamilyEntries[i]).fformula.SameFamily(formula) then begin
        Result:=TDerivedConvFamily(DerivedFamilyEntries[i]).fBaseConvType;
        Exit;
      end;
  //если не найдено подх. семейства - мы создадим такое семейство и такой юнит!
  Result:=RegisterDerivedConversionFamily(formula).fBaseConvType;
end;

function StrToConvType(str: string): TConvType;
var f: TUnitsWithExponent;
    multiplier: Real;
    BaseConvType: TConvType;
    modifier: string;
begin
  f:=TUnitsWithExponent.Create;
  try
    multiplier:=f.TakeFromString(str,modifier);
    BaseConvType:=FormulaToConvType(f); //эта штука может создать на лету новую единицу
    if not DescriptionToConvType(str+modifier,Result) then begin
      multiplier:=multiplier*ConvertFrom(BaseConvType,1);
      Result:=RegisterConversionType(ConvTypeToFamily(BaseConvType),str+modifier,multiplier);
    end;
  finally
    f.Free;
  end;
//end
end;

function StrToConvFamily(str: string): TConvFamily;
begin
  if not DescriptionToConvFamily(str,Result) then
    Raise EPhysUnitError.CreateFmt('Couldn''t find conversion family %s',[str]);
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
      u.AddBaseUnit(TBaseConvFamily(BaseFamilyEntries[i]).fBaseConvType,Exponent);
      Multiply(u);
      if ConvType=TBaseConvFamily(BaseFamilyEntries[i]).fBaseConvType then
        Result:=1
      else
        Result:=Power(Convert(1,ConvType,TBaseConvFamily(BaseFamilyEntries[i]).fBaseConvType),Exponent);
    finally
      u.Free;
    end;
  end
  else begin
    //скребем по derived, находим и помножаем на его unit
    i:=IndexOfDerivedFamily(f);
    if i=-1 then Raise EPhysUnitError.CreateFmt(ConvFamilyNeitherBaseNorDerived,[ConvFamilyToDescription(f)]);
    u:=TUnitsWithExponent.Create;
    try
      u.Assign(TDerivedConvFamily(DerivedFamilyEntries[i]).fformula);
      u.DoPower(Exponent); //возвести в нужную степень (может даже нулевую)
      Multiply(u);  //в этих формулах не должны появляться новые множители
      if ConvType=TDerivedConvFamily(DerivedFamilyEntries[i]).fBaseConvType then
        Result:=1
      else
        Result:=Power(Convert(1,ConvType,TDerivedConvFamily(DerivedFamilyEntries[i]).fBaseConvType),Exponent);
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

function TUnitsWithExponent.TakeFromString(formula: string; out modifier: string): Real;
var p: TSimpleParser;
    term: string;
    convType: TConvType;
    ch: char;
    pow: Real;
    isDivide: Boolean;
    nextDivide: Boolean;
    mul: Real;
resourcestring
  IsNotCorrectUnit = '%s не является единицей измерения';
begin
  Clear;
  modifier:='';
  p:=TSimpleParser.Create(formula);
  Result:=1;
  isDivide:=false;
  nextDivide:=false;
  try
    while not p.eof do begin
      term:=p.getPhysUnitIdent;
      mul:=1;
      if (Length(term)>2) and DescriptionToConvType(RightStr(term,Length(term)-2),ConvType) and (LeftStr(term,2)='мк') then
        Mul:=1e-6
      else if (Length(term)>1) and DescriptionToConvType(RightStr(term,Length(term)-1),ConvType) and (UnitMultiplier[Integer(term[1])]<>0) then begin
        Mul:=UnitMultiplier[Integer(term[1])];
        case term[1] of
          'm': Modifier:=Modifier+'l';
          'M': Modifier:=Modifier+'u';
        end;
      end
      else if not DescriptionToConvType(term,convType) then
        Raise EPhysUnitError.CreateFmt(IsNotCorrectUnit,[term]);
      pow:=1;
      if not p.eof then begin
        ch:=p.getChar;
        if ch='^' then begin
          pow:=p.getFloat;
          if (not p.eof) then begin
            if (p.NextChar<>'*') and (p.NextChar<>'/')  then Raise EPhysUnitError.CreateFmt('Syntax error in unit %s',[formula]);
            nextDivide:=(p.getChar='/');
          end;
        end
        else nextDivide:=(ch='/');
      end;
      if IsDivide then pow:=-pow;
      Result:=Result*AddArbitraryUnit(ConvType,pow)*Power(mul,pow);
      IsDivide:=nextDivide;

//      чтобы он к примеру Джоули преобр. в кг*м^2/с^2
    end;
  finally
    p.Free;
    if modifier<>'' then modifier:='\'+modifier;
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
      if ResultExponents[k]<>0 then inc(k);
    end
    else if IndexOfBaseFamily(first)>IndexOfBaseFamily(second) then begin
      //UnitTypes[j] не было в исх. списке - надо добавить на подх. место
      ResultUnits[k]:=value.UnitTypes[j];
      ResultExponents[k]:=proc(value,-1,j);
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
    ResultExponents[k]:=proc(value,-1,j);
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
  if i=-1 then Result:=value.exponents[j]
  else Result:=Exponents[i]+value.exponents[j];
end;

function TUnitsWithExponent.MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
begin
  if i=-1 then Result:=-value.exponents[j]
  else Result:=Exponents[i]-value.exponents[j];
end;

procedure TUnitsWithExponent.Multiply(value: TUnitsWithExponent);
begin
  Merge(value,MergeMul);
end;

procedure TUnitsWithExponent.Divide(right: TUnitsWithExponent);
begin
  Merge(right,MergeDiv);
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

function TUnitsWithExponent.isAffine: Boolean;
var i,j: Integer;
begin
  Result:=false;
  for i:=0 to fCount-1 do begin
    j:=IndexOfBaseFamily(ConvTypeToFamily(UnitTypes[i]));
    Result:=Result or TBaseConvFamily(BaseFamilyEntries[j]).isAffine;
    if Result=true then break;
  end;
end;

(*
      TVariantWithUnit
                          *)
constructor TVariantWithUnit.Create(text: string);
begin
  Create;
  Assign(text);
end;

constructor TVariantWithUnit.CreateFromVariant(source: Variant; aConvType: TConvType);
begin
  Create;
  instance:=source;
  ConvType:=aConvType;
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

function GetAffineWithMultiplier(BaseConvType: TConvType; mul: Real): TConvType;
var i,L: Integer;
    str: string;
begin
  L:=Length(AffineUnits);
  for i:=0 to L-1 do
    if (AffineUnits[i].BaseConvType=BaseConvType) and (AffineUnits[i].multiplier=mul) then begin
      Result:=AffineUnits[i].ConvType;
      Exit;
    end;
  //раз нет, придется создать
  SetLength(AffineUnits,L+1);
  str:=ConvTypeToDescription(BaseConvType);
  if mul<>0 then
    str:=Format('%s{%g}',[str,mul])
  else
    str:=str+'{dif}';
  Result:=RegisterConversionType(ConvTypeToFamily(BaseConvType),str,1.0/0.0);
  //никогда мы не должны пользоваться ConversionFactor'ом получившейся единицы!
  AffineUnits[L].ConvType:=Result;
  AffineUnits[L].BaseConvType:=BaseConvType;
  AffineUnits[L].multiplier:=mul;
end;

procedure TVariantWithUnit.Conversion(DestConv: TConvType);
var j,i,b: Integer;              //inst - это мы
    offset,k,mul: Real;     //dest - это тоже мы, но позже
    formula: TUnitsWithExponent;
    new_formula: TUnitsWithExponent;
    addition: Variant;
    pow: Real;
    buConvType: TConvType;
    s: string;
resourcestring
  ToConvertFromAToB = 'Для преобразования из [%s] в [%s] выражение домножено на %s';
  IncorrectUnitConversion = 'Некорректное приведение типов: [%s] в [%s]';
begin
  if IsAffine(j) then begin
    if CompatibleConversionTypes(ConvType,DestConv) then begin
      offset:=Convert(0,AffineUnits[j].BaseConvType,DestConv);
      k:=Convert(1,AffineUnits[j].BaseConvType,DestConv)-offset;
      mul:=AffineUnits[j].multiplier;
      instance:=instance*k+offset*mul;
      ConvType:=DestConv;
      IsAffine(j);
      ConvType:=GetAffineWithMultiplier(AffineUnits[j].BaseConvType,mul);
      Exit;
    end
    else begin
      new_formula:=FindPhysUnit(ConvType);
      Conversion(new_formula.UnitTypes[0]);  //проще говоря, в кельвины
      new_formula.Free;
    end;
  end;
  if (ConvType<>DestConv) then begin
    if CompatibleConversionTypes(ConvType,DestConv) then begin
      instance:=instance*Convert(1,ConvType,DestConv);
      ConvType:=DestConv;
    end
    else begin
    //попробуем выразить, зная, что c=1, h=1 и т.д.
      buConvType:=ConvType;
      new_formula:=nil;
      formula:=FindPhysUnit(ConvType);  //это наша родная
      try
        Conversion(FormulaToConvType(formula)); //всяческие электронвольты приводим к СИ
        try
          new_formula:=FindPhysUnit(DestConv);
          formula.Divide(new_formula);
          addition:=0.0;
          for i:=0 to formula.fCount-1 do begin
            j:=IndexOfBaseFamily(ConvTypeToFamily(formula.UnitTypes[i]));
            addition:=addition+UnityPhysConstants.GetVar(j)*formula.Exponents[i];
          end;
          if VarManySolutionsIsNumber(addition) then begin  //успех!
            instance:=instance*Exp(-addition);
            ConvType:=FormulaToConvType(new_formula);
            Conversion(DestConv); //теперь сделается как надо!
            if Assigned(LogConversionDetailsProc) then begin
//разоблачение фокуса - на что мы помножили и поделили
              for i:=0 to UnityPhysConstants.ActuallyUsedCount-1 do begin
                pow:=0.0;
                for j:=0 to formula.fCount-1 do begin
                  b:=IndexOfBaseFamily(ConvTypeToFamily(formula.UnitTypes[j]));
                  pow:=pow-UnityPhysConstants.Solver.getInvMatrix(i,b)*formula.Exponents[j];
                end;
                if abs(pow)>0.001 then begin
                  if Length(s)>0 then s:=s+'*';
                  if abs(pow-1)>1e-19 then
                    s:=s+'('+UnityPhysConstants.Names[UnityPhysConstants.GetActualIndex(i)]+')^'+Format('%2.2g' ,[pow])
                  else
                    s:=s+UnityPhysConstants.Names[UnityPhysConstants.GetActualIndex(i)];
                end;
              end;
              LogConversionDetailsProc(Format(ToConvertFromAToB,[ConvTypeToDescription(buConvType),ConvTypeToDescription(DestConv),s]));
            end;
          end
          else Raise EphysUnitError.CreateFmt(IncorrectUnitConversion,[ConvTypeToDescription(buConvType),ConvTypeToDescription(DestConv)]);
        finally
          new_formula.Free;
        end;
      finally
      formula.Free;
      end;
    end;
  end;
end;

procedure TVariantWithUnit.Negate;
var i: Integer;
    tmp: Variant;
begin
  tmp:=instance; //все-таки счетчик ссылок- не самая надежная вещь, при
  instance:=-tmp; //instance:=-instance течет память
  if IsAffine(i) then
    ConvType:=GetAffineWithMultiplier(AffineUnits[i].BaseConvType,-AffineUnits[i].multiplier);
end;

function TVariantWithUnit.IsAffine (out i: Integer): Boolean;
var findex: Integer;
begin
  findex:=indexOfBaseFamily(ConvTypeToFamily(convType));
  Result:=(findex>=0) and TBaseConvFamily(BaseFamilyEntries[findex]).isAffine;
  if Result then begin
    Result:=false;
    for findex:=0 to Length(AffineUnits)-1 do
      if AffineUnits[findex].ConvType=convType then begin
        Result:=true;
        i:=findex;
        Exit;
      end;
  end;
end;

procedure TVariantWithUnit.DoAdd(value: TAbstractWrapperData);
var v: TVariantWithUnit absolute value;
    j,k: Integer;
    offset,mul: Real;
begin
  if value is TVariantWithUnit then
    if CompatibleConversionTypes(v.ConvType,ConvType) then
    //'этим я хочу сказать, что темп. и разность темп. принадлежит одной семье,
    //но это разные типы и isAffine-это свойство типа, а не семьи!
    //K и gradC - афинные, а Kdif и Cdif - нет!
      if IsAffine(j) and v.IsAffine(k) then begin
        //преобр правую часть в тип левой части
          offset:=Convert(0,AffineUnits[k].BaseConvType,AffineUnits[j].BaseConvType);
          mul:=Convert(1,AffineUnits[k].BaseConvType,AffineUnits[j].BaseConvType)-offset;
          instance:=instance+mul*v.instance+offset*AffineUnits[k].multiplier;
          mul:=AffineUnits[j].multiplier+AffineUnits[k].multiplier; //получивш. множитель
          ConvType:=GetAffineWithMultiplier(AffineUnits[j].BaseConvType,mul);
      end
      else
        instance:=instance+v.instance*Convert(1,v.ConvType,ConvType)
    else begin
      v.Conversion(ConvType);
      instance:=instance+v.instance;
    end
  else inherited; //не знаю пока, пригодится ли эта строчка хоть раз?
end;

procedure TVariantWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
  UL,UR: TUnitsWithExponent;
  j,k: Integer;
begin
  if Right is TVariantWithUnit then begin
    UL:=FindPhysUnit(ConvType);
    try
      UR:=FindPhysUnit(v.ConvType);
      try
        if IsAffine(j) then
          if v.ConvType=duUnity then begin
            instance:=instance*v.instance;
            ConvType:=GetAffineWithMultiplier(AffineUnits[j].BaseConvType,AffineUnits[j].multiplier*v.instance);
          end
          else begin //разрушаем эту афинную иддилию (где здесь сдвоенные согласные - ума не приложу)
          //преобразуем в Кельвины, а точнее, в базовую величину семейства
            Conversion(UL.UnitTypes[0]);
            //и остатки
            if v.IsAffine(k) then begin
              v.Conversion(UR.UnitTypes[0]);
              v.IsAffine(k);
              UL.Multiply(Ur);
              instance:=instance*v.instance*Convert(1,AffineUnits[k].BaseConvType,FormulaToConvType(Ur));
              ConvType:=FormulaToConvType(UL);
            end
            else begin
              UL.Multiply(Ur);
              instance:=instance*v.instance*Convert(1,v.ConvType,FormulaToConvType(Ur));
              ConvType:=FormulaToConvType(UL);
            end;
          end
        else
          if v.IsAffine(j) then
            if ConvType=duUnity then begin
              ConvType:=GetAffineWithMultiplier(AffineUnits[j].BaseConvType,AffineUnits[j].multiplier*instance);
              instance:=instance*v.instance;
            end
            else begin  //еще одно "разрушение" афинных величин
              v.Conversion(UR.UnitTypes[0]);
             //но сейчас ConvType может показывать на K{0}, а не K
             //а указать нужно именно K
              v.IsAffine(k);
              //и остатки
              UL.Multiply(Ur);
              instance:=instance*v.instance*Convert(1,AffineUnits[k].BaseConvType,FormulaToConvType(Ur));
              ConvType:=FormulaToConvType(UL);
            end
          else begin
            //основная процедура
            UL.Multiply(Ur);
            instance:=instance*v.instance*Convert(1,v.ConvType,FormulaToConvType(Ur));
            ConvType:=FormulaToConvType(UL);
          end;
      finally
        Ur.Free;
      end;
    finally
      UL.Free;
    end;
  end
  else inherited;
end;

procedure TVariantWithUnit.DoInverse;
var j: Integer;
    U: TUnitsWithExponent;
begin
  U:=FindPhysUnit(ConvType);
  try
    if IsAffine(j) then begin
      //прикрываем эту лавочку - преобр. в кельвины (точнее, в баз. величину семейства)
      Conversion(U.UnitTypes[0]);
      instance:=1/instance;
      U.DoPower(-1);
      ConvType:=FormulaToConvType(U);
    end
    else begin
      //преобр. в тот тип, который нам положен по формуле
      Conversion(FormulaToConvType(U));
      instance:=1/instance;
      U.DoPower(-1);
      ConvType:=FormulaToConvType(U);
    end;
  finally
    U.Free;
  end;
end;

procedure TVariantWithUnit.DoDivide(Right: TAbstractWrapperData);
begin
  (Right as TVariantWithUnit).DoInverse;
  DoMultiply(Right);
end;

procedure TVariantWithUnit.DoPower(pow: Real);
var U: TUnitsWithExponent;
    bt: TConvType;
begin
  U:=FindPhysUnit(ConvType);
  try
    bt:=FormulaToConvType(U);
//    instance:=instance*Convert(1,ConvType,bt);  //перешли в базовый тип
    Conversion(bt);
    if VarIsComplex(instance) then
      instance:=VarComplexPower(instance,pow)
    else
      instance:=Power(instance,pow);
    U.DoPower(pow);
    ConvType:=FormulaToConvType(U);
  finally
    U.Free;
  end;
end;

procedure TVariantWithUnit.Assign(str: string);
var unitStr: string;
    i: Integer;
    dimension: TUnitsWithExponent;
    val: Extended;
    modifier: string;
begin
  for i:=1 to Length(str) do
    if (str[i]='.') or (str[i]=',') then str[i]:=DecimalSeparator;
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
    //посерединке пробел
    //либо часть справа от пробела-ед. изм., либо например разделенные действ и мним. части
    if Uppercase(str[Length(str)])='I' then begin
      Instance:=VarComplexCreate(str);
      ConvType:=duUnity;
    end
    else begin
      if TryStrToFloat(LeftStr(str,i-1),val) then
        instance:=val
      else
        instance:=VarComplexCreate(LeftStr(str,i-1));
      unitStr:=RightStr(str,Length(str)-i);
      if not DescriptionToConvType(unitStr,ConvType) then begin
        //сложное выражение, нужно создать экземпляр TUnitsWithExponent,
        //скормить ему размерность, он "выплюнет" множитель, домножаем на него
        dimension:=TUnitsWithExponent.Create;
        try
          instance:=instance*dimension.TakeFromString(unitStr,modifier);
          //а также ConvType - тот, что является базовым для данного семейства
          //возможно, семейство придется создать на ходу
          ConvType:=FormulaToConvType(dimension);
        finally
          dimension.Free;
        end;
      end;
    end;
  end;
end;

function TVariantWithUnit.AsString: string;
var deg,min: Variant;
    s: string;
    d: extended;
    i: Integer;
begin
  if ConvType=auDMS then begin
    instance:=instance+1/7200000;
    deg:=Floor(instance);
    s:=deg;
    Result:=s+'°';
    min:=Floor((instance-deg)*60);
    s:=min;
    Result:=Result+s+'''';
    deg:=(instance-deg-min/60)*3600;
    if VarIsNumeric(deg) then begin
      d:=deg;
      s:=Format('%2.2f',[d]);
    end
    else s:=deg;
    Result:=Result+s+'"';
  end
  else begin
    Result:=instance;
    s:=ConvTypeToDescription(ConvType);
    i:=Pos('\',s);
    if i>0 then
      s:=LeftStr(s,i-1);
    if s<>'' then Result:=Result+' '+s;
  end;
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
  else
    TWrapperVarData(Dest).Data:=TVariantWithUnit.CreateFromVariant(Variant(source),duUnity);
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
        with TVariantWithUnitVarData(Source).Data do begin
          if ConvTypeToFamily(ConvType)<>cbDimensionless then
            Raise Exception.CreateFmt('''%s'': can''t convert value with unit to non-string type',[AsString]);
          VarDataCastTo(Dest,TVarData(instance),AVarType);
        end;
    end;
  end
  else inherited; //нам дали пустой variant скорее всего
end;

(*
    TFundamentalPhysConstants
                                *)
constructor TFundamentalPhysConstants.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fsolver:=TSimpleGaussLEQ.Create;
  fsolver.SetTolerance(1e-19);
  recalculate;
end;

function TFundamentalPhysConstants.getName(i: Integer): string;
begin
  Result:=fNames[i];
end;

function TFundamentalPhysConstants.getDescription(i: Integer): string;
begin
  Result:=fDescriptions[i];
end;

function TFundamentalPhysConstants.getEnabled(i: Integer): Boolean;
begin
  Result:=fEnabled[i];
end;

function TFundamentalPhysConstants.GetActualIndex(i: Integer): Integer;
begin
  Result:=fActualIndex[i];
end;

resourcestring
  NonConsistentConstraints = 'Добавление условия %s=1 приводит к противоречию';

procedure TFundamentalPhysConstants.setEnabled(i: Integer; aValue: Boolean);
begin
  if aValue<>fEnabled[i] then begin
    fEnabled[i]:=aValue;
    if not Recalculate then begin
      fEnabled[i]:=not aValue;
      Raise EPhysUnitError.CreateFMT(NonConsistentConstraints,[fNames[i]]);
    end;
  end;
end;

procedure TFundamentalPhysConstants.Add(aName,aDescr: string; aValue: Variant; aEnabled: Boolean=false);
var L: Integer;
begin
  inc(fCount);
  L:=Length(fNames);
  if L<fCount then L:=2*fCount;
  SetLength(fNames,L);
  SetLength(fDescriptions,L);
  SetLength(fValues,L);
  SetLength(fEnabled,L);
  fNames[fCount-1]:=aName;
  fDescriptions[fCount-1]:=aDescr;
  fValues[fCount-1]:=aValue;
  if aEnabled then begin
    fEnabled[fCount-1]:=true;
    if not Recalculate then begin
      fEnabled[fCount-1]:=false;
      Raise EPhysUnitError.CreateFmt(NonConsistentConstraints,[aName]);
    end;
  end;
end;

function TFundamentalPhysConstants.GetVar(i: Integer): Variant;
begin
  Result:=fsolver.GetVariable(i);
end;

function TFundamentalPhysConstants.Recalculate: Boolean;
var BaseUnitsCount,EqsCount,i,j,ind: Integer;
    formula: TUnitsWithExponent;
    V: TVariantWithUnit;
begin
  BaseUnitsCount:=BaseFamilyEntries.Count;
  EqsCount:=0;
  fActuallyUsedCount:=0;
  SetLength(fActualIndex,fcount);
  fsolver.SetDimensions(BaseUnitsCount,0);
  for j:=0 to fCount-1 do
    if fenabled[j] then begin
      inc(EqsCount);
      fsolver.SetDimensions(BaseUnitsCount,EqsCount);
      V:=TVariantWithUnitVarData(fvalues[j]).Data;
      formula:=FindPhysUnit(V.ConvType);
      for i:=0 to formula.fCount-1 do begin
        ind:=IndexOfBaseFamily(ConvTypeToFamily(formula.UnitTypes[i]));
        fsolver.Matrix[ind,EqsCount-1]:=formula.Exponents[i];
      end;
      fsolver.Matrix[BaseUnitsCount,EqsCount-1]:=Ln(V.instance);
      formula.Free;
      inc(fActuallyUsedCount);
      fActualIndex[fActuallyUsedCount-1]:=j;
    end;
  fsolver.InvertMatrix;
  Result:=(fsolver.GetStatus<>slNoSolution);
end;

procedure TFundamentalPhysConstants.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('constants',ReadData,nil,false);
end;

procedure TFundamentalPhysConstants.ReadData(reader: TReader);
var p: TSimpleParser;
    aname,adescr: string;
    aunit: Variant;
    aenabled: boolean;
    expr: TVariantExpression;
begin
  reader.ReadListBegin;
  p:=TSimpleParser.Create;
  expr:=TVariantExpression.Create(nil);
  while not reader.EndOfList do begin
    p.AssignString(reader.ReadString);
    aname:=p.getString;
    expr.SetString(p.getString);
    aunit:=expr.GetVariantValue;
    adescr:=p.getString;
    if not p.eof then
      aenabled:=(p.getInt=1)
    else
      aenabled:=false;
    Add(aname,adescr,aunit,aenabled);
  end;
  p.Free;
  expr.Free;
  reader.ReadListEnd;
end;


(*
    Initialization
                        *)
procedure PopulateUnitMultiplier;
var u: PUnitMultipliersArray;
begin
  u:=@UnitMultiplier;
  u[Integer('d')]:=1e-1;
  u[Integer('д')]:=1e-1;
  u[Integer('c')]:=1e-2;
  u[Integer('с')]:=1e-2;
  u[Integer('m')]:=1e-3;
  u[Integer('м')]:=1e-3;
  u[Integer('u')]:=1e-6;
  u[Integer('n')]:=1e-9;
  u[Integer('н')]:=1e-9;
  u[Integer('p')]:=1e-12;
  u[Integer('п')]:=1e-12;
  u[Integer('f')]:=1e-15;
  u[Integer('ф')]:=1e-15;
  u[Integer('k')]:=1000;
  u[Integer('к')]:=1000;
  u[Integer('M')]:=1e6;
  u[Integer('М')]:=1e6;
  u[Integer('G')]:=1e9;
  u[Integer('Г')]:=1e9;
  u[Integer('T')]:=1e12;
  u[Integer('Т')]:=1e12;
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

function VarWithUnitCreateFromVariant(source: Variant; ConvType: TConvType): Variant;
resourcestring
  AlreadyHasDimension = '%s уже имеет размерность';
begin
  if IsVarWithUnit(source) then
    if TVariantWithUnitVarData(source).Data.ConvType=duUnity then begin
      Result:=source;
      TVariantWithUnitVarData(Result).Data.ConvType:=ConvType;
    end
    else
      Raise EPhysUnitError.CreateFmt(AlreadyHasDimension,[source])
  else
    VarWithUnitCreateInto(Result,TVariantWithUnit.CreateFromVariant(source,ConvType));
end;

function IsVarWithUnit(V: Variant): boolean;
begin
  Result:=(TWrapperVarData(V).VType=VariantWithUnit);
end;

function Isdimensionless(V: Variant): boolean;
begin
  Result:=(TVarData(V).VType=VariantWithUnit) and
    (TVariantWithUnitVarData(V).Data.ConvType=duUnity);
end;

function VarWithUnitConvert(source: Variant; DestConvType: TConvType): Variant;
var inst,dest: TVariantWithUnit;
begin
  if not IsVarWithUnit(source) then
    source:=VarWithUnitCreateFromVariant(source,duUnity);
  inst:=TVariantWithUnitVarData(source).Data;
  dest:=TVariantWithUnit.Create;
  try
    dest.Assign(inst);
    dest.Conversion(DestConvType);
    VarWithUnitCreateInto(Result,dest);
  except  //эта хреновина замаскировала ошибку!
    dest.Free;
    raise;
  end;
end;

function VarWithUnitConvert(source: Variant; UnitName: string): Variant;
begin
  Result:=VarWithUnitConvert(source,StrToConvType(UnitName));
end;

function VarWithUnitPower(source: Variant; pow: Real): Variant;
begin
  Result:=source;
  TVariantWithUnitVarData(Result).Data.DoPower(pow);
end;

(*
    General procedures
                          *)

function NameToFamily(const Ident: string; var Int: Longint): Boolean;
var id: string;
    convfam: TConvFamily;
begin
  id:=Ident;
  Result:=DescriptionToConvFamily(id,convfam);
  Int:=convfam;
end;

function FamilyToName(Int: LongInt; var Ident: string): Boolean;
begin
  Ident:=ConvFamilyToDescription(Int);
  Result:=true;
end;

function NameToConv(const Ident: string; var Int: LongInt): Boolean;
var convtype: TConvType;
begin
  Result:=DescriptionToConvType(Ident,convtype);
  Int:=convtype;
end;

function ConvToName(Int: LongInt; var Ident: string): Boolean;
begin
  Ident:=ConvTypeToDescription(Int);
  Result:=true;
end;

procedure NewConvFamilies;
var comp: TComponent;
    consts: TFundamentalPhysConstants absolute comp;
    baseconv: TBaseConvFamily absolute comp;
    der: TDerivedConvFamily absolute comp;
//  strStream: TStringStream;
  fileStream: TFileStream;
  BinStream: TMemoryStream;
  sr: TSearchRec;
  i,j,L: Integer;
  types: TConvTypeArray;
begin
  if FindFirst(Default_Dir+'*.txt',0,sr)=0 then begin
    repeat
      fileStream:=TFileStream.Create(Default_dir+sr.Name,fmOpenRead);
      fileStream.Seek(0, soFromBeginning);
      binStream:=TMemoryStream.Create;
      while FileStream.Position<FileStream.Size do
        ObjectTextToBinary(FileStream,BinStream);
      BinStream.Seek(0, soFromBeginning);
      while BinStream.Position<BinStream.Size do begin
        comp:=BinStream.ReadComponent(nil);
        if (comp is TFundamentalPhysConstants) and
         (uppercase(consts.Lang)=Uppercase(PhysUnitLanguage)) then begin
          UnityPhysConstants.Free;
          UnityPhysConstants:=consts;
        end
        else if (comp is TBaseConvFamily) then begin
          i:=IndexOfBaseFamily(baseconv.fConvFamily);
          if (i>=0) then
            if (Uppercase(PhysUnitLanguage)=Uppercase(baseconv.flang)) or (Uppercase(baseconv.flang)='ANY') then
              BaseFamilyEntries[i]:=baseconv
            else baseconv.Free
          else
            BaseFamilyEntries.Add(baseconv);
        end
        else if (comp is TDerivedConvFamily) then begin
          i:=IndexOfDerivedFamily(der.fConvFamily);
          if (i>=0) then
            if (Uppercase(PhysUnitLanguage)=Uppercase(der.flang)) or (UpperCase(der.flang)='ANY') then
              DerivedFamilyEntries[i]:=der
            else der.Free
          else
            DerivedFamilyEntries.Add(der);
        end
        else
          comp.Free;
      end;
      BinStream.Free;
      FileStream.Free;
    until FindNext(sr)<>0
  end;

  for i:=0 to BaseFamilyEntries.Count-1 do
    if TBaseConvFamily(BaseFamilyEntries[i]).fAffine then begin
      GetConvTypes(TBaseConvFamily(BaseFamilyEntries[i]).fConvFamily,Types);
      L:=Length(AffineUnits);
      SetLength(AffineUnits,L+Length(Types));
      for j:=0 to Length(Types)-1 do begin
        AffineUnits[L+j].ConvType:=Types[j];
        AffineUnits[L+j].BaseConvType:=Types[j];
        AffineUnits[L+j].multiplier:=1;
      end;
    end;

end;
(*
      TBaseConvFamily
                          *)
procedure TAbstractSavedConvFamily.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Units',ReadUnits,nil,true);
end;

procedure TAbstractSavedConvFamily.ReadUnits(Reader: TReader);
var p: TSimpleParser;
    varexpr: TVariantExpression;
    UnitName: string;
    Multiplier,Offset: Real;
resourcestring
  UnitNotFound = 'Единица измерения %s не найдена';
begin
  if not DescriptionToConvFamily(name,fConvFamily) then
      fConvFamily:=RegisterConversionFamily(name);

  varexpr:=TVariantExpression.Create(nil);
  Reader.ReadListBegin;
  p:=TSimpleParser.Create;
  while not Reader.EndOfList do begin
    p.AssignString(Reader.ReadString);
    UnitName:=p.getString;
    varexpr.SetString(p.getString);
    Multiplier:=varexpr.getVariantValue;
    if not p.eof then begin
      varexpr.SetString(p.getString);
      offset:=varExpr.getVariantValue;
      RegisterConversionType(TconvTypeAffine.Create(fConvFamily,UnitName,offset,multiplier),fBaseConvType)
      //baseconvtype здесь подставлен, чтобы лишнюю перем. не вводить, знач. мы игнорируем
    end
    else
      RegisterConversionType(fConvFamily,UnitName,multiplier);
  end;
  p.Free;
  varexpr.Free;
  Reader.ReadListEnd;
  //на этом этапе уже должен считаться baseType
  if not DescriptionToConvType(fStrBaseUnit,fBaseConvType) then
    Raise Exception.CreateFMT(UnitNotFound,[fStrBaseUnit]);
  //регистрацию оставим для Loaded
end;

constructor TDerivedConvFamily.Create(owner: TComponent);
begin
  inherited Create(Owner);
  fformula:=TUnitsWithExponent.Create;
end;

destructor TDerivedConvFamily.Destroy;
begin
  fformula.Free;
  inherited Destroy;
end;

procedure TDerivedConvFamily.Loaded;
var mul: Real;
    modifier: string;
resourcestring
  NonCoherentUnit = '%s должна быть когерентной (с единичным безразмерным множителем) производной величиной';
  CaseAmbiguity = 'в основной единице измерения данной размерности %s не должно быть приставок м/М';
begin
  mul:=fformula.TakeFromString(fStrFormula,modifier);
  if abs(mul-1)>1e-10 then
    Raise Exception.CreateFMT(NonCoherentUnit,[fStrFormula]);
  if modifier<>'' then Raise Exception.CreateFMT(CaseAmbiguity,[fStrFormula]);
end;


(*
      TConvTypeAffine
                          *)
constructor TConvTypeAffine.Create(const AConvFamily: TConvFamily; const ADescription: string; offset,factor: Real);
begin
  inherited Create(AConvFamily,ADescription);
  foffset:=offset;
  ffactor:=factor;
end;

function TConvTypeAffine.ToCommon(const AValue: Double): Double;
begin
  Result:=foffset+AValue*ffactor;
end;

function TConvTypeAffine.FromCommon(const AValue: Double): Double;
begin
  Result:=(AValue-foffset)/ffactor;
end;

procedure InitializePhysUnitLib;
var nodim: TDerivedConvFamily;
begin
  //милли, микро, кило и пр.
  PopulateUnitMultiplier;
  //новый тип Variant'а


  BaseFamilyEntries:=TObjectList.create;
  DerivedFamilyEntries:=TObjectList.Create;
  //ключевые типы, их нужно задать в коде
  UnityPhysConstants:=TFundamentalPhysConstants.Create(nil);
  //пусть пустой, но он нам нужен, чтобы не проверять на nil

  nodim:=TDerivedConvFamily.Create(nil);

  cbDimensionless:=RegisterConversionFamily('Dimensionless');
  duUnity:=RegisterConversionType(cbDimensionless,'',1);
  nodim.fConvFamily:=cbDimensionless;
  nodim.fBaseConvType:=duUnity;
  nodim.Lang:='Any';
  DerivedFamilyEntries.Add(nodim);

  cbAngle:=RegisterConversionFamily('Angle');
  auDMS:=RegisterConversionType(cbAngle,'dms',pi/180);
  auRadian:=RegisterConversionType(cbAngle,'rad',1);


  default_dir:=GetCurrentDir+'\data\PhysUnits\';
  NewConvFamilies;
end;

procedure FinalizePhysUnitLib;
var i: Integer;
begin
  for i:=0 to DerivedFamilyEntries.Count-1 do
    UnregisterConversionFamily(TDerivedConvFamily(DerivedFamilyEntries[i]).fConvFamily);
  for i:=0 to BaseFamilyEntries.Count-1 do
    UnregisterConversionFamily(TBaseConvFamily(BaseFamilyEntries[i]).fConvFamily);
  SetLength(AffineUnits,0);  
  FreeAndNil(UnityPhysConstants);
  FreeAndNil(DerivedFamilyEntries);
  FreeAndNil(BaseFamilyEntries);
end;


initialization
  RegisterClasses([TBaseConvFamily,TDerivedConvFamily,TFundamentalPhysConstants]);
  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);
  GConvUnitToStrFmt:='%g %s';
  VarWithUnitType:=TVariantWithUnitType.Create;
  PhysUnitLanguage:='English';

finalization
  FinalizePhysUnitLib;
  FreeAndNil(VarWithUnitType);


end.
