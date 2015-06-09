unit new_phys_unit_lib;

interface
uses set_english_locale_if_not_sure,streaming_class_lib,classes,
  variantWrapper,Contnrs,sysUtils,simple_parser_lib,linear_eq,
  autocomplete_string_list;

type
  TPhysUnit =class;
  TUnitsWithExponents = class;  
  TPhysFamily=class(TStreamingClass) //можно будет и к TComponent вернуться
    private
      fIsBase: Boolean;
      fCaption,fShortName,fDescription: TLocalizedName;
      fBaseType: TPhysUnit;
      fFormula: TUnitsWithExponents;
      fstrformula: string;
      fLocalList: TStringList;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure SetupFormula;
      function index: Integer;
    published
      property IsBase: Boolean read fIsBase write fIsBase;
      property Caption: TLocalizedName read fCaption write fCaption;
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Description: TLocalizedName read fDescription write fDescription;
      property BaseType: TPhysUnit read fBaseType write fBaseType;
      property formula: string read fstrformula write fstrformula;
  end;

  TPhysUnit=class(TStreamingClass)
    private
      fShortName,fCaption: TLocalizedName;
      fScaledUp,fScaledDown: TPhysUnit;
      fPrefixOK,fIsScaled: boolean;
      function GetSomeId: string;
    public
      constructor Create(aOwner: TComponent); override;
      function CreateScaled(mult: Real): TPhysUnit; virtual; abstract;
      function CreateAndConnectScaled(mult: Real): TPhysUnit;
      function ConvertToBase(value: Variant): Variant; virtual; abstract;
      function ConvertFromBase(value: Variant): Variant; virtual; abstract;
      function Convert(value: Variant; var ConvType: TPhysUnit): Variant; virtual;
      function Family: TPhysFamily;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit); virtual;
      function MultiplyByNumber(v1,num: Variant; var ConvType: TPhysUnit): Variant; virtual; abstract;
      property SomeId: string read GetSomeId;
    published
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Caption: TLocalizedName read fCaption write fCaption;
      property PrefixOk: Boolean read fPrefixOk write fPrefixOk default false;
      property IsScaled: Boolean read fIsScaled write fIsScaled default false;
      property ScaledUp: TPhysUnit read fScaledUp write fScaledUp stored false;
      property ScaledDown: TPhysUnit read fScaledDown write fScaledDown stored false;
    end;

  TNormalConvType=class(TPhysUnit)
    private
      fMultiplier: Real;
    public
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit); override;
      function MultiplyByNumber(v1,num: Variant; var ConvType: TPhysUnit): Variant; override;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
    end;

  TAffineConvType=class(TPhysUnit)
    private
      fMultiplier, fOffset,fFactor: Real;
      fBaseAffine: TAffineConvType;
      fListOfDerived: TObjectList;
      procedure SetBaseAffine(value: TAffineConvType);
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function Convert(value: Variant;var ConvType: TPhysUnit): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit); override;
      function MultiplyByNumber(v1,num: Variant; var ConvType: TPhysUnit): Variant; override;
      function GetAffineUnit(Factor: Real): TAffineConvType;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
      property Offset: Real read fOffset write fOffset;
      property Factor: Real read fFactor write fFactor;
      property BaseAffine: TAffineConvType read fBaseAffine write SetBaseAffine;
    end;

  TLogarithmicConvType=class(TPhysUnit)
    private
      fLog10Mult: Real;
      fZeroValue: Real;
    public
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit);override;
      function MultiplyByNumber(v1,num: Variant; var ConvType: TPhysUnit): Variant; override;
    published
      property Log10Multiplier: Real read fLog10Mult write fLog10Mult;
      property ZeroValue: Real read fZeroValue write fZeroValue;
    end;

  TUnitPrefix=class(TComponent)
    private
      fPrefix,fFullName: TLocalizedName;
      fmultiplier: Real; //надо будет на expression заменить, но пока страшно
      fIsPreferred: Boolean;
    public
      constructor Create(aOwner: TComponent); override;
    published
      property Prefix: TLocalizedName read fPrefix write fPrefix;
      property FullName: TLocalizedName read fFullName write fFullName;
      property Multiplier: Real read fMultiplier write fMultiplier;
      property IsPreferred: Boolean read fIsPreferred write fIsPreferred default true;
    end;

  TUnitPrefixes = class(TStreamingClass)
  //соберем в одном месте все префиксы и отсортируем по множителю
  private
    fHigher,fLower,fOther: TList;
  public
    constructor Create(aOwner: TComponent); override;
    procedure PrepareLists;
    destructor Destroy; override;
  end;

  TPhysUnitMessagesProc = procedure (line: string); //цвет сами придумаем

  TPhysConst = class(TComponent)
  private
    fla: string;
    fcaption,fdescr: TLocalizedName;
    fIsEnabled: boolean;
    procedure SetEnabled(value: Boolean);
  public
    value: Variant;
    constructor Create(aOwner: TComponent); override;
  published
    property formula: string read fla write fla;
    property caption: TLocalizedName read fcaption write fcaption;
    property descr: TLocalizedName read fdescr write fdescr;
    property enabled: Boolean read fIsEnabled write SetEnabled;
  end;

  TPhysConsts = class(TStreamingClass)
  private
    fList: TObjectList;
    fsolver: IAbstractSLEQ;
    fActuallyUsedCount: Integer;
    fActualIndex: array of Integer;
    fLang: string;
    function GetItems(index: Integer): TPhysConst;
    function Recalculate: Boolean;
    function getCount: Integer;
  protected
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    property Items[index: Integer]: TPhysConst read GetItems; default;
    function GetVar(i: Integer): Variant;
    function GetActualIndex(i: Integer): Integer;
    property Count: Integer read getCount;
    property Solver: IAbstractSLEQ read fsolver;
    property ActuallyUsedCount: Integer read fActuallyUsedCount;
  published
    property Lang: string read fLang write fLang;
  end;

  TPhysUnitData = class(TStreamingClass)
  private
    fFamilyList: TObjectList;
    fBaseFamilyList: TObjectList;
    fMegaList: TStringList;
    fAutocompleteList: TAutocompleteStringList;
    fWarningList: TStringList;
    fUnity,fDMS,fRadian: TPhysUnit;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    warningproc: TPhysUnitMessagesProc;
    infoproc: TPhysUnitMessagesProc;
    procedure AddToMegalist(ct: TPhysUnit);
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function StrToConvType(str: string): TPhysUnit;
    property Unity: TPhysUnit read fUnity;
    property DMS: TPhysUnit read fDMS;
    property Radian: TPhysUnit read fRadian;
    property AutoCompleteList: TAutocompleteStringList read fAutocompleteList;
  published
    UnitPrefixes: TUnitPrefixes;
    PhysConsts: TPhysConsts;
  end;

  TUnitsWithExponentMergeProc = function (value: TUnitsWithExponents; i,j: Integer) : Real of object;
  TShowLocalizedName = function(ConvType: TPhysUnit): TLocalizedName;
  TExponents = array of Real;
  TUnitTypes = array of TPhysUnit;

  TUnitsWithExponents = class(TComponent)
    private
      UnitTypes: TUnitTypes;
      Exponents: TExponents;
      fCount: Integer;
      procedure Merge(value: TUnitsWithExponents; proc: TUnitsWithExponentMergeProc);
      function MergeMul(value: TUnitsWithExponents; i,j: Integer): Real;
      function MergeDiv(value: TUnitsWithExponents; i,j: Integer): Real;
      procedure WriteData(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
      function AddArbitraryUnit(ConvType: TPhysUnit; Exponent: Real): Real;
      function ShowLocalized(proc: TShowLocalizedName): TLocalizedName;
    public
      procedure Clear;
      procedure Assign(source: TPersistent); override;
      procedure SetBaseType(value: TPhysUnit);
      function TakeFromString(formula: string): Real;
      procedure DoPower(Exponent: Real);
      function SameFamily(value: TUnitsWithExponents): Boolean;
      procedure Multiply(value: TUnitsWithExponents);
      procedure Divide(right: TUnitsWithExponents);
      function AsString: string;
      function ShowLocFormula: TLocalizedName;
      function ShowLocShortName: TLocalizedName;
      function GetConvType: TPhysUnit;
      function IsUnity: Boolean;
  end;

  TVarWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TPhysUnit;
    public
      instance: Variant; //та переменная, которую мы оборачиваем
      ExplicitConversion: boolean;  //флаг, что величину "насильно" привели к данному виду
      constructor Create(text: string); reintroduce; overload;
      constructor CreateFromVariant(source: Variant; aConvType: TPhysUnit);
      procedure Assign(source: TPersistent); override;
      procedure Negate; override; //взять обратный знак
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoInverse;
      procedure DoDivide(Right: TAbstractWrapperData); override;
      procedure DoPower(pow: Real);
      function AsString: string; override;
      procedure Conversion(DestConv: TPhysUnit);
    published
      property isExplicitlyConverted: boolean read ExplicitConversion;
  end;

  TVarWithUnitType = class (TAbstractWrapperVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    function CompareOp(const Left, Right: TVarData; const Operator: Integer): Boolean; override;
  end;

  TPhysUnitParser = class (TSimpleParser)
  public
    function getPhysUnit: TPhysUnit;
    function getVariantNum: Variant;
  end;

  TVarWithUnitVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TVarWithUnit;
    Reserved4: LongInt;
  end;

  EPhysUnitError = class (Exception);
  procedure InitPhysUnitData;
  function IsPhysUnit(V: Variant): Boolean;
  function IsDimensionless(V: Variant): Boolean;
  procedure PhysUnitCreateInto(var ADest: Variant; const Adata: TVarWithUnit);
  function TryPhysUnitCreate(text: string; out Res: Variant): boolean;  
  function PhysUnitCreateFromVariant(source: Variant; ConvType: TPhysUnit): Variant;
  function PhysUnitConvert(source: Variant; DestConvType: TPhysUnit; explicit: boolean=false): Variant;
  function PhysUnitPower(source: Variant; pow: Real): Variant;
  function PhysUnitFindGoodPrefix(V: Variant): Variant;

  var PhysUnitData: TPhysUnitData;
      VarWithUnitVariantType: TVarWithUnitType;

implementation

uses Variants,math,strUtils,VarCmplx,expression_lib;

resourcestring
  AbstractErrorAdd = 'could not add %s and %s';
  LogarithmicConvTypeRequiresRealNum = 'Лог. единицы измерения допустимы только для положительных действительных значений';
  StrCanReferTo = '%s can refer to:';
  ConvTypeWeUsedIs = 'we used %s';
  ToConvertFromAToB = 'Для преобразования из [%s] в [%s] выражение домножено на %s';
  IncorrectUnitConversion = 'Некорректное приведение типов: [%s] в [%s]';
  UnitsWithExponentsSyntaxErr = 'Syntax error in unit %s';
  NonConsistentConstraints = 'Добавление условия %s=1 приводит к противоречию';
  FundPhysConstMustBeRealPositive='Fund phys const %s must be real and positive';
  FundConstMustBePhys = 'Phys unit const expected, %s found';
  AlreadyHasDimension = '%s уже имеет размерность';          
(*
      General procedures
                            *)
function NoSpaces(str: string): String;
var i,j,prev: Integer;
begin
  Result:='';
  prev:=1;
  for i:=1 to Length(str) do
    if (str[i]=' ') and (prev<i) then begin
      Result:=Result+MidStr(str,prev,i-prev);
      j:=i+1;
      while (j<=Length(str)) and (str[j]=' ') do inc(j);
      prev:=j;
      if j<=Length(str) then str[j]:=AnsiUppercase(MidStr(str,j,1))[1]
      else Exit;
    end;
  Result:=Result+RightStr(str,Length(str)-prev+1);
end;

function ToAcceptableName(str: string): string;
var i: Integer;
begin
  Result:=NoSpaces(str);
  if Length(Result)=0 then Exit;
  if not TSimpleParser.isFirstIdentSymbol(Result[1]) then
    Result[1]:='_';
  for i:=2 to Length(Result) do
    if not TSimpleParser.isIdentSymbol(Result[i]) then
      Result[i]:='_';
end;

function ConvTypeToLocShortName(ConvType: TPhysUnit): TLocalizedName;
begin
  Result:=ConvType.ShortName;
end;

function ConvTypeToLocFamilyLetter(ConvType: TPhysUnit): TLocalizedName;
begin
  Result:=ConvType.family.ShortName;
end;

(*
      TPhysFamily
                                *)
constructor TPhysFamily.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCaption:=TLocalizedName.Create(self);
  fShortName:=TLocalizedName.Create(self);
  fDescription:=TLocalizedName.Create(self);
  fFormula:=TUnitsWithExponents.Create(self);
  fLocalList:=TStringList.Create;
  fLocalList.Sorted:=true;
  fLocalList.Duplicates:=dupError;
  fLocalList.CaseSensitive:=true;
end;

destructor TPhysFamily.Destroy;
begin
  fLocalList.Free;
  inherited Destroy;
end;

function TPhysFamily.index: Integer;
begin
  Result:=(Owner as TPhysUnitData).fFamilyList.IndexOf(self);
end;

procedure TPhysFamily.SetupFormula;
begin
  if IsBase then fFormula.SetBaseType(BaseType)
  else fFormula.TakeFromString(formula);
end;


(*
    TPhysUnit
                                  *)
constructor TPhysUnit.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCaption:=TLocalizedName.Create(self);
  fShortName:=TLocalizedName.Create(self);
end;

function TPhysUnit.Family: TPhysFamily;
begin
  Result:=Owner as TPhysFamily;
end;

function TPhysUnit.CreateAndConnectScaled(mult: Real): TPhysUnit;
var iterator: TPhysUnit;
begin
  Result:=CreateScaled(mult);
  iterator:=self;
  if mult>1 then begin
    while Assigned(iterator.ScaledUp) do iterator:=iterator.ScaledUp;
    iterator.ScaledUp:=Result;
    Result.ScaledDown:=iterator;
  end
  else begin
    while Assigned(iterator.ScaledDown) do iterator:=iterator.ScaledDown;
    iterator.ScaledDown:=Result;
    Result.ScaledUp:=iterator;
  end;
end;

procedure TPhysUnit.Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit);

begin
  Raise Exception.CreateFmt(AbstractErrorAdd,[Caption.Caption,t.Caption.Caption]);
end;

function TPhysUnit.Convert(value: Variant; var ConvType: TPhysUnit): Variant;
begin
  Result:=ConvertFromBase(ConvType.ConvertToBase(value));
  ConvType:=self; //в большинстве случаев сработает как надо
end;

function TPhysUnit.GetSomeId: string;
begin
  if not Caption.TryEnglish(Result) and not ShortName.TryEnglish(Result) then
    Result:=name;
end;
(*
    TNormalConvType
                      *)
procedure TNormalConvType.Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit);
begin
  if Owner=t.Owner then begin //same family
    v1:=v1+ConvertFromBase(t.ConvertToBase(v2));
    ConvType:=self;
  end
  else inherited;
end;

function TNormalConvType.ConvertFromBase(value: Variant): Variant;
begin
  Result:=value/multiplier;
end;

function TNormalConvType.ConvertToBase(value: Variant): Variant;
begin
  Result:=value*multiplier;
end;

function TNormalConvType.MultiplyByNumber(v1,num: Variant;
  var ConvType: TPhysUnit): Variant;
begin
  Result:=v1*num;
  ConvType:=self;
end;

function TNormalConvType.CreateScaled(mult: Real): TPhysUnit;
var cpy: TNormalConvType absolute Result;
begin
  Result:=TNormalConvType.Clone(self);
  Result.PrefixOk:=false;
  Result.IsScaled:=true;
  cpy.Multiplier:=Multiplier*mult;
end;


{ TAffineConvType }

constructor TAffineConvType.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fListOfDerived:=TObjectList.Create(false);
  Factor:=1;
  BaseAffine:=self;
end;

destructor TAffineConvType.Destroy;
begin
  fListOfDerived.Free;
  inherited Destroy;
end;

procedure TAffineConvType.SetBaseAffine(value: TAffineConvType);
begin
  fBaseAffine:=value;
  if Assigned(value) then value.fListOfDerived.Add(self);
end;

function TAffineConvType.ConvertToBase(value: Variant): Variant;
begin
  Result:=value*Multiplier+Offset;
end;

function TAffineConvType.ConvertFromBase(value: Variant): Variant;
begin
  Result:=(value-offset)/Multiplier;
end;

function TAffineConvType.CreateScaled(mult: Real): TPhysUnit;
var cpy: TAffineConvType absolute Result;
begin
  Result:=TAffineConvType.Clone(self);
  Result.PrefixOk:=false;
  Result.IsScaled:=true;
  cpy.Multiplier:=Multiplier*mult;
end;

function TAffineConvType.Convert(value: Variant; var ConvType: TPhysUnit): Variant;
var AffUnit: TAffineConvType;
begin
  //если нам дали K{dif}, а мы сами Celsius, то надо преобр. в Celsius{dif}!
  AffUnit:=GetAffineUnit((ConvType as TAffineConvType).Factor);
  Result:=AffUnit.ConvertFromBase(ConvType.ConvertToBase(value));
  ConvType:=AffUnit;
end;

procedure TAffineConvType.Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit);
var t2a: TAffineConvType absolute t;
begin
  if Owner = t.Owner then begin
    v1:=v1+Convert(v2,t); //может ругнуться на приведение ConvType as TAffineConvType
    ConvType:=GetAffineUnit(Factor+t2a.Factor); //если досюда дошли, уже безопасно absolute
  end
  else inherited;
end;

function TAffineConvType.MultiplyByNumber(v1,num: Variant;
  var ConvType: TPhysUnit): Variant;
begin
  v1:=v1*num;
  ConvType:=GetAffineUnit(Factor*num);
end;

function TAffineConvType.GetAffineUnit(Factor: Real): TAffineConvType;
var i: Integer;
    der: TAffineConvType;
begin
  //либо находим имеющийся, либо создаем своего
  for i:=0 to BaseAffine.fListOfDerived.Count-1 do begin
    der:=TAffineConvType(BaseAffine.fListOfDerived[i]);
    if der.Factor=Factor then begin
      Result:=der;
      Exit;
    end;
  end;
  //не нашли
  der:=TAffineConvType.Clone(baseAffine);
  der.Factor:=Factor;
  der.Offset:=der.Offset*Factor;
  der.BaseAffine:=BaseAffine; //на этом этапе он должен автоматом появиться в списке
  //теперь украшательства - имя ему изменяем
  for i:=0 to der.ShortName.strings.Count-1 do
    der.ShortName.strings[i]:=Format('%s{%g}',[der.ShortName.strings[i],Factor]);
  for i:=0 to der.Caption.strings.Count-1 do
    der.Caption.strings[i]:=Format('%s{%g}',[der.Caption.strings[i],Factor]);
  der.ensureCorrectName(ToAcceptableName(der.Caption.InEnglish),owner);
  owner.InsertComponent(der);
  Result:=der;
  PhysUnitData.AddToMegalist(Result);
  //и в megalist добавить неплохо бы! Хотя это половинчатое решение - надо по-хорошему
  //парсить выражение a{b}
end;


(*
    TLogarithmicConvType
                          *)


procedure TLogarithmicConvType.Add(var V1: Variant; out ConvType: TPhysUnit; V2: Variant; t: TPhysUnit);
begin
  if Owner=t.Owner then begin
    v1:=ConvertFromBase(ConvertToBase(v1)+t.ConvertToBase(v2));
    ConvType:=self;
  end
  else inherited;
end;

function TLogarithmicConvType.ConvertFromBase(value: Variant): Variant;
begin
  if VarIsNumeric(value) then
    Result:=Log10Multiplier*log10(value/ZeroValue)
  else
    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.ConvertToBase(value: Variant): Variant;
begin
  if VarIsNumeric(value) then
    Result:=ZeroValue*power(10.0,(value/Log10Multiplier))
  else
    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.MultiplyByNumber(v1,num: Variant;
  var ConvType: TPhysUnit): Variant;
begin
  if VarIsNumeric(v1) and VarIsNumeric(num) and (num>0) then begin
    Result:=v1+log10multiplier*log10(num);
    ConvType:=self;
  end
  else
    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.CreateScaled(mult: Real): TPhysUnit;
var cpy: TLogarithmicConvType absolute Result;
begin
  Result:=TLogarithmicConvType.Clone(self);
  Result.PrefixOk:=false;
  Result.IsScaled:=true;
  cpy.Log10Multiplier:=Log10Multiplier/mult;
end;

(*
    TUnitPrefix
                    *)
constructor TUnitPrefix.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fFullName:=TLocalizedName.Create(self);
  fPrefix:=TLocalizedName.Create(self);
  fIsPreferred:=true;
end;

(*
    TUnitPrefixes
                    *)
constructor TUnitPrefixes.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fHigher:=TList.Create;
  fLower:=TList.Create;
  fOther:=TList.Create;
end;

destructor TUnitPrefixes.Destroy;
begin
  fHigher.free;
  fLower.Free;
  fOther.Free;
  inherited Destroy;
end;

function UnitPrefixCompare(p1,p2: Pointer): Integer;
var u1: TUnitPrefix absolute p1;
    u2: TUnitPrefix absolute p2;
begin
  if u1.Multiplier<u2.Multiplier then Result:=-1
  else if u1.Multiplier>u2.Multiplier then Result:=1
  else Result:=0;
end;

procedure TUnitPrefixes.PrepareLists;
var i: Integer;
    comp: TComponent;
    pref: TUnitPrefix absolute comp;
begin
  fHigher.Clear;
  fLower.Clear;
  fOther.Clear;
  for i:=0 to ComponentCount-1 do begin
    comp:=Components[i];
    if comp is TUnitPrefix then
      if pref.IsPreferred then
        if pref.Multiplier>1 then fHigher.Add(pref) else fLower.Add(pref)
      else fOther.Add(pref);
  end;
  fHigher.Sort(UnitPrefixCompare);
  fLower.Sort(UnitPrefixCompare);
end;

(*
        TPhysUnitData
                            *)
constructor TPhysUnitData.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  PhysUnitData:=self; //буахахахахаха!
  fFamilyList:=TObjectList.Create(false);
  fBaseFamilyList:=TObjectList.Create(false);
  fMegaList:=TStringList.Create;
  fMegaList.CaseSensitive:=true;
  fMegaList.Sorted:=true;
  fMegaList.Duplicates:=dupAccept;
  fAutocompleteList:=TAutocompleteStringList.Create;
//  fAutocompleteList:=TStringList.Create;
  fAutocompleteList.Sorted:=true;
  fAutocompleteList.CaseSensitive:=false;
  fAutocompleteList.Duplicates:=dupAccept;
  fWarningList:=TStringList.Create;
end;

destructor TPhysUnitData.Destroy;
begin
  fMegaList.Free;
  fAutocompleteList.Free;
  fWarningList.Free;
  fFamilyList.Free;
  fBaseFamilyList.Free;
  inherited Destroy;
end;

procedure TPhysUnitData.AddToMegalist(ct: TPhysUnit);
var fam: TPhysFamily;
    nm: TLocalizedName;

    procedure AddToList;
    var i: Integer;
    begin
      for i:=0 to nm.strings.Count-1 do begin
        if fMegaList.IndexOf(nm.strings[i])>=0 then
          fWarningList.Add(Format('%s ident ambiguity',[nm.strings[i]]));
        fMegaList.AddObject(nm.strings[i],ct);
        fAutocompleteList.AddObject(nm.strings[i],ct);
      end;
    end;

begin
  fam:=ct.Family;
  nm:=TLocalizedName.Create(nil);
  nm.Assign(ct.ShortName);
  AddToList;
  nm.LeftConcat(fam.Caption,'.');
  AddToList;
  nm.Assign(ct.ShortName);
  nm.LeftConcat(fam.ShortName,'.');
  AddToList;
  nm.Assign(ct.Caption);
  AddToList;
  nm.LeftConcat(fam.Caption,'.');
  AddToList;
  nm.Assign(ct.Caption);
  nm.LeftConcat(fam.ShortName,'.');
  AddToList;
  nm.Free;
end;

procedure InitPhysUnitData;
var j,k,z: Integer;
    ct: TPhysUnit;
    cpy: TPhysUnit;
    pref: TUnitPrefix;
    m1: Real;
    dimensionless: TUnitsWithExponents;
    fam: TPhysFamily;
    objlist: TObjectList;

    procedure HandleCreatedCopy;
    var i: Integer;
        prev: TPhysUnit;
    begin
    //для каждого языка нашей величины мы должны найти подходящий язык приставки
      cpy.ShortName.LeftConcat(pref.Prefix);
      cpy.Caption.LeftConcat(pref.FullName);
      cpy.EnsureCorrectName(ToAcceptableName(cpy.getSomeId),fam);
      i:=fam.fLocalList.IndexOf(cpy.GetSomeId);
      if (i>=0) then begin
        prev:=fam.flocalList.Objects[i] as TPhysUnit;
        if cpy.EqualsByAnyOtherName(prev) then begin
          if fam.BaseType=prev then
            fam.BaseType:=cpy;
          fam.fLocalList.Delete(i);
          prev.Free;
        end;
      end;
      fam.InsertComponent(cpy);
      fam.fLocalList.AddObject(cpy.GetSomeId,cpy);
    end;
begin
  with PhysUnitData do begin
    if not Assigned(UnitPrefixes) then
      UnitPrefixes:=TUnitPrefixes.Create(PhysUnitData);
    UnitPrefixes.PrepareLists;
    objList:=TObjectList.Create(false);
    //создадим физ. величины с приставками
    for j:=0 to fFamilyList.Count-1 do begin
      fam:=fFamilyList[j] as TPhysFamily;
      objList.Clear;
      for k:=0 to fam.ComponentCount-1 do
        if fam.Components[k] is TPhysUnit then
          objList.Add(fam.Components[k]);
      for k:=0 to objList.Count-1 do begin
        ct:=TPhysUnit(objList[k]);
        fam.fLocalList.AddObject(ct.GetSomeId,ct);
        if ct.PrefixOk then begin
        //нашли новую жертву
          for z:=0 to UnitPrefixes.fHigher.Count-1 do begin
            pref:=TUnitPrefix(UnitPrefixes.fHigher[z]);
            m1:=pref.Multiplier;
            cpy:=ct.CreateAndConnectScaled(m1);
            HandleCreatedCopy;
          end;
          for z:=UnitPrefixes.fLower.Count-1 downto 0 do begin
            pref:=TUnitPrefix(UnitPrefixes.fLower[z]);
            m1:=pref.Multiplier;
            cpy:=ct.CreateAndConnectScaled(m1);
            HandleCreatedCopy;
          end;
          //остальные приставки нам не нравятся!
          for z:=0 to UnitPrefixes.fOther.Count-1 do begin
            pref:=TUnitPrefix(UnitPrefixes.fOther[z]);
            m1:=pref.Multiplier;
            cpy:=ct.CreateScaled(m1);
            HandleCreatedCopy;
          end;
        end;
      end;
      for k:=0 to fam.ComponentCount-1 do
        if fam.Components[k] is TPhysUnit then begin
          ct:=TPhysUnit(fam.Components[k]);
          AddToMegaList(ct);
        end;
    end;
    objlist.Free;
    //еще нужно лист привести в порядок немножко
    fMegaList.Sorted:=false;
    fAutocompleteList.Sorted:=false;
    for j:=0 to fMegaList.Count-1 do begin
     fMegaList[j]:=NoSpaces(fMegaList[j]);
     fAutocompleteList[j]:=NoSpaces(fAutocompleteList[j]);
    end;
    fMegaList.Sorted:=true;
    fAutocompleteList.Sorted:=true;


    //на этом этапе TUnitsWithExponents должна стать работоспособной
    for j:=0 to fFamilyList.Count-1 do begin
      (fFamilyList[j] as TPhysFamily).SetupFormula;
      if TPhysFamily(fFamilyList[j]).IsBase then
        fBaseFamilyList.Add(fFamilyList[j]);
    end;
      //осталось
      //найти Unity, а именно - стандартную безразм. величину
    dimensionless:=TUnitsWithExponents.Create(nil);
    funity:=dimensionless.GetConvType;
    dimensionless.Free;
    //и еще DMS отыскать, ну пусть он будет тупо в семье Angle и под именем DMS
    fam:=FindComponent('Angle') as TPhysFamily;
    if Assigned(fam) then begin
      fDMS:=fam.FindComponent('DMS') as TPhysUnit;
      fRadian:=fam.FindComponent('Radian') as TPhysUnit;
    end;
    if Assigned(PhysConsts) then
      PhysConsts.Init;

    fMegaList.SaveToFile('megalist.txt');
    fAutocompleteList.Sort;
    fAutocompleteList.SaveToFile('autocomplete.txt');
    fWarningList.SaveToFile('warnings.txt');
  end;
end;

procedure TPhysUnitData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opInsert) and (AComponent is TPhysFamily) then
    fFamilyList.Add(AComponent);
end;

function TPhysUnitData.StrToConvType(str: string): TPhysUnit;
var i,j,k: Integer;
    obj: TPhysUnit;
    fml: TUnitsWithExponents;
    multiplier: Real;
    NormalRslt: TNormalConvType absolute Result;
begin
  i:=fMegaList.IndexOf(str);
  if i>=0 then begin
    j:=i+1;
    while (j<fMegaList.Count) and (fMegaList[j]=str) do inc(j);
    if j>i+1 then begin //неоднозначность
    //выведем в warning разные варианты
      if Assigned(warningProc) then begin
        warningProc(Format(StrCanReferTo,[str]));
        for k:=i to j-1 do
          warningProc(TPhysUnit(fMegaList.Objects[k]).Caption.Caption);
      end;
    //выбираем либо ед. измерения без приставки
      for k:=i to j-1 do begin
        Result:=TPhysUnit(fMegaList.Objects[k]);
        if not Result.IsScaled then begin
          if Assigned(warningProc) then
            warningProc(Format(ConvTypeWeUsedIs,[Result.Caption.Caption]));
          Exit;
        end
      end;
    //если дошли до этого места, значит они все с приставками, выберем наобум
      if Assigned(warningproc) then warningProc(Format(ConvTypeWeUsedIs,[Result.Caption.Caption]));
    end
    else
      Result:=TPhysUnit(fMegaList.Objects[i])
  end
  else begin
  //ничего не нашли, это вестимо что-то составное
    fml:=TUnitsWithExponents.Create(self);
    multiplier:=fml.TakeFromString(str);
    obj:=fml.GetConvType; //возможно, сейчас на лету была создана подходящая величина
    if obj=nil then Result:=nil
    else begin
      i:=fMegaList.IndexOf(str);
      if i>=0 then
        Result:=fMegaList.Objects[i] as TPhysUnit
      else begin
        Result:=TNormalConvType.Create(obj.Owner);
        Result.ensureCorrectName(ToAcceptableName(str),Result.Owner);
        Result.ShortName.AddInCurrentLang(str); //халтура
        NormalRslt.Multiplier:=multiplier;
      end;
    end;
  end;
end;

(*
    TVarWithUnit
                    *)
procedure TVarWithUnit.Assign(source: TPersistent);
var s: TVarWithUnit absolute source;
begin
  if source is TVarWithUnit then begin
    instance:=s.instance; //variant'ы - они умные, скопируются
    ConvType:=s.ConvType;
  end
  else inherited Assign(source);
end;

constructor TVarWithUnit.Create(text: string);
var expr: TVariantExpression;
    v: Variant;
begin
  inherited Create;
  expr:=TVariantExpression.Create(nil);
  expr.SetString(text);
  v:=expr.GetVariantValue;
  Assign(TVarWithUnitVarData(v).Data);
  expr.free;
end;

function TVarWithUnit.AsString: string;
var deg,min: Variant;
    s: string;
    d: extended;
begin
  if ConvType=PhysUnitData.DMS then begin
    if instance<0 then begin  //потенциальная ошибка - комплексные нельзя сравн
      Result:='-';
      instance:=-instance;
    end;
    instance:=instance+1/7200000;
    deg:=Floor(instance);
    s:=deg;
    Result:=Result+s+'°';
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
    if not ConvType.ShortName.TryCaption(s) and not ConvType.Caption.TryCaption(s)
      then s:=ConvType.Name;
    if s<>'' then Result:=Result+' '+s;
  end;
end;

procedure TVarWithUnit.Conversion(DestConv: TPhysUnit);
var buConvType:TPhysUnit;
    new_formula,formula: TUnitsWithExponents;
    addition: Variant;
    i,j,b:Integer;
    pow: Real;
    s: string;
begin
  if (ConvType<>DestConv) then begin
    if ConvType.Owner=DestConv.Owner then //same family
      instance:=DestConv.Convert(instance,ConvType)
    else begin
//      Raise EPhysUnitError.Create('sorry, implicit conversion not supported right now');
    //попробуем выразить, зная, что c=1, h=1 и т.д.
      buConvType:=ConvType;
      formula:=TUnitsWithExponents.Create(nil);
      try
        formula.Assign(ConvType.family.fFormula);
        Conversion(ConvType.family.BaseType); //всяческие электронвольты приводим к СИ
        new_formula:=DestConv.family.fFormula;
        formula.Divide(new_formula);
        addition:=0.0;
        for i:=0 to formula.fCount-1 do begin
          j:=PhysUnitData.fBaseFamilyList.IndexOf(formula.UnitTypes[i].Family);
          addition:=addition+PhysUnitData.PhysConsts.GetVar(j)*formula.Exponents[i];
        end;
        if VarManySolutionsIsNumber(addition) then begin  //успех!
          instance:=instance*Exp(-addition);
          ConvType:=new_formula.GetConvType;
          Conversion(DestConv); //теперь сделается как надо!
          if Assigned(physUnitData.infoProc) then begin
//разоблачение фокуса - на что мы помножили и поделили
            for i:=0 to physUnitData.PhysConsts.ActuallyUsedCount-1 do begin
              pow:=0.0;
              for j:=0 to formula.fCount-1 do begin
                b:=PhysUnitData.fBaseFamilyList.IndexOf(formula.UnitTypes[j].Family);
                pow:=pow-PhysUnitData.PhysConsts.Solver.getInvMatrix(i,b)*formula.Exponents[j];
              end;
              if abs(pow)>0.001 then begin
                if Length(s)>0 then s:=s+'*';
                if abs(pow-1)>1e-19 then
                  s:=s+'('+PhysUnitData.PhysConsts[PhysUnitData.PhysConsts.GetActualIndex(i)].caption.Caption+')^'+Format('%2.2g',[pow])
                else
                  s:=s+PhysUnitData.PhysConsts[PhysUnitData.PhysConsts.GetActualIndex(i)].caption.Caption;
              end;
            end;
            PhysUnitData.infoproc(Format(ToConvertFromAToB,[buConvType.Shortname.Caption,DestConv.Shortname.Caption,s]));
          end;
        end
        else Raise EphysUnitError.CreateFmt(IncorrectUnitConversion,[buConvType.Shortname.Caption,DestConv.Shortname.Caption]);
      finally
        formula.Free;
      end;
    end;
  end;
end;

constructor TVarWithUnit.CreateFromVariant(source: Variant;
  aConvType: TPhysUnit);
begin
  Create;
  instance:=source;
  ConvType:=aConvType;
end;

procedure TVarWithUnit.DoAdd(value: TAbstractWrapperData);
var v: TVarWithUnit absolute value;
begin
  if value is TVarWithUnit then begin
    if ConvType.Owner<>v.ConvType.Owner then
      v.Conversion(ConvType);
    ConvType.Add(instance,ConvType,v.instance,v.ConvType);
  end
  else inherited;
end;

procedure TVarWithUnit.DoDivide(Right: TAbstractWrapperData);
var tmp: TVarWithUnit;
begin
  tmp:=TVarWithUnit.Create;
  tmp.Assign(Right);
  tmp.DoInverse;
  DoMultiply(tmp);
  tmp.Free;
end;

procedure TVarWithUnit.DoInverse;
var U: TUnitsWithExponents;
begin
  U:=TUnitsWithExponents.Create(nil);
  try
    U.Assign(ConvType.Family.fFormula);
    Conversion(ConvType.Family.BaseType);
    instance:=1/instance; //это можно, поскольку баз. тип - нормальный!
    U.DoPower(-1);
    ConvType:=U.GetConvType;
  finally
    U.Free;
  end;
end;

procedure TVarWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVarWithUnit absolute Right;
  UL,UR,f: TUnitsWithExponents;
begin
  if Right is TVarWithUnit then begin
    UL:=ConvType.Family.fFormula;
    UR:=v.ConvType.Family.fFormula;
    if UR.IsUnity then  //безразмерная, но может быть и % или ppm
      instance:=ConvType.MultiplyByNumber(instance,v.ConvType.ConvertToBase(v.instance),ConvType)
    else if UL.IsUnity then
      instance:=v.ConvType.MultiplyByNumber(v.instance,ConvType.ConvertToBase(instance),ConvType)
    else begin
      //основная процедура
      Conversion(ConvType.family.BaseType);
      f:=TUnitsWithExponents.Create(nil);
      try
        f.Assign(UL);
        f.Multiply(UR);
        instance:=instance*v.ConvType.ConvertToBase(v.instance);
        ConvType:=f.GetConvType;
      finally
        f.Free;
      end;
    end
  end
  else inherited;
end;

procedure TVarWithUnit.DoPower(pow: Real);
var U: TUnitsWithExponents;
begin
  U:=TUnitsWithExponents.Create(nil);
  try
    U.Assign(ConvType.Family.fFormula);
    Conversion(ConvType.Family.BaseType);
    if VarIsComplex(instance) then
      instance:=VarComplexPower(instance,pow)
    else
      instance:=Power(instance,pow);
    U.DoPower(pow);
    ConvType:=U.GetConvType;
  finally
    U.Free;
  end;
end;

procedure TVarWithUnit.Negate;
begin
  instance:=ConvType.MultiplyByNumber(instance,-1.000000000,ConvType);
end;

{ TVarWithUnitType }

procedure TVarWithUnitType.Cast(var Dest: TVarData;
  const Source: TVarData);
begin
  //строку преобразуем по всем правилам, а любые другие Variant'ы "оборачиваем" безразм.
  VarDataClear(Dest);
  if VarDataIsStr(Source) then
    TWrapperVarData(Dest).Data:=TVarWithUnit.Create(VarDataToStr(Source))
  else
    TWrapperVarData(Dest).Data:=TVarWithUnit.CreateFromVariant(Variant(source),PhysUnitData.Unity);
  Dest.VType:=VarWithUnitVariantType.VarType;
end;

procedure TVarWithUnitType.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);
var tmp: Variant;
begin
  if Source.VType = VarType then begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TWrapperVarData(Source).data.AsString);
      varString:
        VarDataFromStr(Dest, TWrapperVarData(Source).data.AsString);
      else
        with TVarWithUnitVarData(Source).Data do begin
          tmp:=PhysUnitConvert(Variant(source),PhysUnitData.Unity);
          VarDataCastTo(Dest,TVarData(TVarWithUnitVarData(tmp).Data.instance),AVarType);
        end;
    end;
  end
  else inherited; //нам дали пустой variant скорее всего
end;

function TVarWithUnitType.CompareOp(const Left, Right: TVarData;
  const Operator: Integer): Boolean;
var L,R: TVarWithUnit;
begin
  Result:=false;
  L:=TVarWithUnitVarData(Left).Data;
  R:=TVarWithUnitVarData(Right).Data;
  R.Conversion(L.ConvType); //при сравнении ожидаем, что их по кр. мере можно сравнивать!
  case operator of
    opCmpEQ: Result:=(L.instance=R.instance);
    opCmpNE: Result:=(L.instance<>R.instance);
    opCmpLT: Result:=(L.instance<R.instance);
    opCmpLE: Result:=(L.instance<=R.instance);
    opCmpGT: Result:=(L.instance>R.instance);
    opCmpGE: Result:=(L.instance>=R.instance);
  end;
end;

{ TUnitsWithExponents }
function TUnitsWithExponents.AddArbitraryUnit(ConvType: TPhysUnit; Exponent: Real): Real;
var u: TUnitsWithExponents;
begin
  Assert(ConvType.Family<>nil,'Unit '+ConvType.Name+'don''t have a parent');
  Assert(ConvType.Family.BaseType<>nil,'Family '+ConvType.Family.Name+'don''t have baseType');
  u:=TUnitsWithExponents.Create(owner);
  try
    u.Assign(ConvType.Family.fformula);
    u.DoPower(Exponent); //возвести в нужную степень (может даже нулевую)
    Multiply(u); //в этих формулах не должны появляться новые множители
    if ConvType=ConvType.Family.BaseType then
      Result:=1 //это лишь для ускорения работы, а может ну его нафиг?
    else
    //если затесались градусы - жди беды. Мы тогда считаем их {0} и находим multiplier
      if ConvType is TNormalConvType then
        Result:=Power((ConvType as TNormalConvType).Multiplier,Exponent)
      else
        Result:=Power((ConvType as TAffineConvType).Multiplier,Exponent);
      //именно эта штука может выдать exception, если ConvType<>TNormalConvType
  finally
    u.Free;
  end;
end;

procedure TUnitsWithExponents.Assign(source: TPersistent);
var s: TUnitsWithExponents absolute source;
begin
  if source is TUnitsWithExponents then begin
    Exponents:=Copy(s.Exponents);
    UnitTypes:=Copy(s.UnitTypes);
    fCount:=s.fCount;
  end
  else inherited Assign(source);
end;

function TUnitsWithExponents.AsString: string;
var str: TLocalizedName;
begin
  str:=ShowLocShortName;
//  if str.strings.Count<>0 then
    result:=str.InEnglish;
//  if result='' then
//    assert(result='','wtf');
  str.Free;
end;

procedure TUnitsWithExponents.Clear;
begin
  fCount:=0;
  SetLength(UnitTypes,0);
  SetLength(Exponents,0);
end;

procedure TUnitsWithExponents.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',nil,WriteData,true);
end;

procedure TUnitsWithExponents.WriteData(writer: TWriter);
begin
  writer.WriteString(AsString);
end;

procedure TUnitsWithExponents.Divide(right: TUnitsWithExponents);
begin
  Merge(right,MergeDiv);
end;

procedure TUnitsWithExponents.DoPower(Exponent: Real);
var i: Integer;
begin
  for i:=0 to fcount-1 do
    Exponents[i]:=Exponents[i]*Exponent;
end;

procedure TUnitsWithExponents.Merge(value: TUnitsWithExponents;
  proc: TUnitsWithExponentMergeProc);
var i,j,k: Integer;
    L1,L2: Integer;
    ResultUnits: TUnitTypes;
    ResultExponents: TExponents;
    first,second: TPhysFamily;
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
    first:=UnitTypes[i].Family;
    second:=value.UnitTypes[j].Family;
    if first=second then begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=proc(value,i,j);
      inc(i);
      inc(j);
      if ResultExponents[k]<>0 then inc(k);
    end
    else if first.index>second.index then begin
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

function TUnitsWithExponents.MergeDiv(value: TUnitsWithExponents; i,
  j: Integer): Real;
begin
  if i=-1 then Result:=-value.exponents[j]
  else Result:=Exponents[i]-value.exponents[j];
end;

function TUnitsWithExponents.MergeMul(value: TUnitsWithExponents; i,
  j: Integer): Real;
begin
  if i=-1 then Result:=value.exponents[j]
  else Result:=Exponents[i]+value.exponents[j];
end;

procedure TUnitsWithExponents.Multiply(value: TUnitsWithExponents);
begin
  Merge(value,MergeMul);
end;

function TUnitsWithExponents.SameFamily(
  value: TUnitsWithExponents): Boolean;
  //по сути, просто equals
var i: Integer;
begin
  if fcount=value.fCount then begin
    Result:=true;
    for i:=0 to fCount-1 do
      if (UnitTypes[i]<>value.UnitTypes[i]) or (Exponents[i]<>value.Exponents[i]) then begin
        Result:=false;
        Exit;
      end;
  end
  else Result:=false;
end;

function TUnitsWithExponents.IsUnity: Boolean;
begin
  Result:=(fCount=0);
end;

procedure TUnitsWithExponents.SetBaseType(value: TPhysUnit);
begin
  fCount:=1;
  SetLength(UnitTypes,1);
  SetLength(Exponents,1);
  UnitTypes[0]:=value;
  Exponents[0]:=1;
end;

function TUnitsWithExponents.ShowLocalized(proc: TShowLocalizedName): TLocalizedName;
var i,j,neg: Integer;
begin
  j:=0; //кол-во уже имеющихся ед.
  neg:=0;
  Result:=TLocalizedName.CreateEmptyNeutral;
  if fCount=0 then Exit;
  for i:=0 to fCount-1 do
    if Exponents[i]>0 then begin
      if j>0 then Result.RightConcat('*');
      inc(j);
      Result.RightConcat(proc(UnitTypes[i]));
      if Exponents[i]<>1 then
        Result.RightConcat('^'+FloatToStr(Exponents[i]));
    end
    else inc(neg);
  if j=0 then Result.RightConcat('1');
  //а теперь отрицательные степени
  j:=0;
  if neg>0 then begin
    if neg=1 then Result.RightConcat('/')
    else Result.RightConcat('/(');
    for i:=0 to fCount-1 do
      if Exponents[i]<0 then begin
        if j>0 then Result.RightConcat('*');
        inc(j);
        Result.RightConcat(proc(UnitTypes[i]));
        if Exponents[i]<>-1 then
          Result.RightConcat('^'+FloatToStr(-Exponents[i]));
      end;
    if neg>1 then Result.RightConcat(')');
  end;
end;

function TUnitsWithExponents.ShowLocFormula: TLocalizedName;
begin
  Result:=ShowLocalized(ConvTypeToLocFamilyLetter);
end;

function TUnitsWithExponents.ShowLocShortName: TLocalizedName;
begin
  Result:=ShowLocalized(ConvTypeToLocShortName);
end;

function TUnitsWithExponents.TakeFromString(formula: string): Real;
var p: TSimpleParser;
    term: string;
    convType: TPhysUnit;
    ch: char;
    pow: Real;
    isDivide: Boolean;
    nextDivide: Boolean;
begin
  Clear;
  p:=TSimpleParser.Create(formula);
  Result:=1;
  isDivide:=false;
  nextDivide:=false;

  try
    while not p.eof do begin
      term:=p.getPhysUnitIdent;
//      mul:=UnitPrefixes.PrefixDescrToMultiplier(term,Modifier,ConvType);
      ConvType:=PhysUnitData.StrToConvType(term);

      pow:=1;
      if not p.eof then begin
        ch:=p.getChar;
        if ch='^' then begin
          pow:=p.getFloat;
          if (not p.eof) then begin
            if (p.NextChar<>'*') and (p.NextChar<>'/') then
              Raise EPhysUnitError.CreateFmt(UnitsWithExponentsSyntaxErr,[formula]);
            nextDivide:=(p.getChar='/');
          end;
        end
        else nextDivide:=(ch='/');
      end;
      if IsDivide then pow:=-pow;
      Result:=Result*AddArbitraryUnit(ConvType,pow);
      IsDivide:=nextDivide;
//      чтобы он к примеру Джоули преобр. в кг*м^2/с^2
    end;
  finally
    p.Free;
  end;

end;


function TUnitsWithExponents.GetConvType: TPhysUnit;
var i: Integer;
    fam: TPhysFamily;
    un: TNormalConvType;
begin
  for i:=0 to PhysUnitData.fFamilyList.Count-1 do begin
    fam:=PhysUnitData.fFamilyList[i] as TPhysFamily;
    if SameFamily(fam.fFormula) then begin
      Result:=fam.BaseType;
      Exit;
    end;
  end;
  //не нашли подходящую семью - придется создать!
  fam:=TPhysFamily.Create(PhysUnitData);
  fam.fFormula.Assign(self);
  fam.ShortName:=ShowLocFormula;
  fam.Name:=ToAcceptableName(fam.ShortName.InEnglish);
  //и еще подходящую единицу измерения создадим
  //нормальную, других нельзя
  un:=TNormalConvType.Create(fam);
  un.Name:=ToAcceptableName(AsString);
  un.Multiplier:=1;
  //нужно ей и название сформировать, чтоб находить на всех языках
  //или хотя бы на одном
  //shortname или caption - пофиг по большому счету!
  un.ShortName:=ShowLocShortName;
  fam.BaseType:=un;
  Result:=un;

  PhysUnitData.AddToMegalist(Result);
end;

(*
    TPhysUnitParser
                          *)
function TPhysUnitParser.getPhysUnit: TPhysUnit;
var id: string;
    InitPos,tempPos: Integer;
    ch: char;
begin
  //хитрость в том, чтобы найти окончание.
  //скажем, 1 км*2 - здесь ед. изм "км"
  //но в 1 Н*м - "Н*м".
  skip_spaces;
  InitPos:=_pos;  //fBackupPos будет меняться внутри цикла
  TempPos:=_pos;
  while not eof do begin
    id:=GetPhysUnitIdent;
    //хотя у нас есть безразмерная величина, принимать
    //пустое место за нее не имеем права!
    if (id='') or (PhysUnitData.fMegaList.IndexOf(id)=-1) then begin
      _pos:=TempPos;
      break;
    end
    else begin
      if eof then break;
      TempPos:=_pos;
      ch:=GetChar;
      if ch='^' then begin
        GetFloat;
        TempPos:=_pos;
        if eof then break;
        ch:=GetChar;
      end;
      if eof or ((ch<>'*') and (ch<>'/')) then begin
        _pos:=TempPos;
        break;
      end;
    end;
  end;

  fBackupPos:=InitPos;
  if _pos<>InitPos then
    Result:=PhysUnitData.StrToConvType(MidStr(_str,InitPos,_pos-InitPos))
  else
    Result:=nil;
end;

function TPhysUnitParser.getVariantNum: Variant;
var Ch: char;
  state: Integer;
  deg,min: Integer;
  sec: Real;

  function TryExponent: Boolean;
  begin
    Result:=(ch='e') or (ch='E');
    if Result then begin
      inc(_pos);
      if _pos>Length(_str) then begin
        putBack;
        Raise EParserError.CreateFmt(UnexpectedEndOfString,[GetString]);
      end;
      ch:=_str[_pos];
      if (ch='-') or (ch='+') then state:=4
      else if IsNumberSymbol(ch) then state:=5
      else begin
        putBack;
        Raise EParserError.CreateFmt(UnknownSymbolAfterExponent,[GetString]);
      end;
    end;
  end;

  procedure TryImaginary;
  begin
    if (ch='i') or (ch='I') or (ch='j') or (ch='J') then
      inc(_pos);
  end;

begin
  Result:=Unassigned;
  //в самом числе не может содержаться никаких пробелов!
  skip_spaces;
  fBackupPos:=_pos;
  state:=0;
  while (_pos<=Length(_str)) do begin
    Ch:=_str[_pos];
    case state of
      0: begin
          if IsNumberSymbol(ch) then state:=1 else begin
            putBack;
            Raise EParserError.CreateFmt(DigitExpected,[GetString]);
          end;
        end;
      1: begin
          if not IsNumberSymbol(ch) then
            if (ch='.') or (ch=',') then begin
              state:=2;
              _str[_pos]:=DecimalSeparator;
            end
            else if (ch='d') or (ch='D') or (ch='°') then begin
              deg:=StrToInt(MidStr(_str,fBackupPos,_pos-fBackupPos));
              getChar;
              min:=getInt;
              if getChar<>'''' then Raise EParserError.Create(MinutesExpected);
              sec:=getFloat;
              ch:=getChar;
              if (ch<>'"') and (getChar<>'''') then Raise EParserError.Create(SecondsExpected);
              Result:=PhysUnitCreateFromVariant(deg+min/60+sec/3600,PhysUnitData.DMS);
              Exit;
            end
            else if not TryExponent then begin
              TryImaginary;
              break;
            end;
        end;
      2: if IsNumberSymbol(ch) then state:=3 else begin
          putBack;
          Raise EParserError.CreateFmt(DigitExpectedAfterDecimalSeparator,[GetString]);
        end;
      3:  if not IsNumberSymbol(ch) then
            if not TryExponent then begin
              TryImaginary;
              break;
            end;
      4: if IsNumberSymbol(ch) then state:=5 else begin
          putBack;
          Raise EParserError.CreateFmt(DigitExpectedAfterExponent,[GetString]);
        end;
      5: if not IsNumberSymbol(ch) then begin
          TryImaginary;
          break;
          end;
      end;
    inc(_pos);
  end;
  if state=0 then Raise EParserError.Create('getVariantNum: empty expression');
  if state=2 then begin
    PutBack;
    Raise EParserError.CreateFmt(DigitExpectedAfterDecimalSeparator,[GetString]);
  end;
  if state=4 then begin
    PutBack;
    Raise EParserError.CreateFmt(DigitExpectedAfterExponent,[GetString]);
  end;

  if (UpperCase(_str[_pos-1])='I') or (UpperCase(_str[_pos-1])='J') then
    Result:=VarComplexCreate(0,StrToFloat(MidStr(_str,fBackupPos,_pos-fBackupPos-1)))
  else
    Result:=StrToFloat(MidStr(_str,fBackupPos,_pos-fBackupPos));
end;

(*
    TPhysConst
                  *)
constructor TPhysConst.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fdescr:=TLocalizedName.Create(self);
  fcaption:=TLocalizedName.Create(self);
end;

procedure TPhysConst.SetEnabled(value: Boolean);
begin
  if Value<>Enabled then begin
    fIsEnabled:=Value;
    if (not (csLoading in ComponentState)) and not (Owner as TPhysConsts).Recalculate then begin
      fIsEnabled:=not value;
      Raise EPhysUnitError.CreateFMT(NonConsistentConstraints,[caption.Caption]);
    end;
  end;
end;

(*
    TFundamentalPhysConstants
                                *)
constructor TPhysConsts.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fsolver:=TSimpleGaussLEQ.Create;
  fsolver.SetTolerance(1e-19);
  fList:=TObjectList.Create(false);
  recalculate;
end;

destructor TPhysConsts.Destroy;
begin
  //debug purpose
  //Heisenbug: now it works without error
  fsolver:=nil;
  fList.Free;
  inherited Destroy;
end;

function TPhysConsts.GetItems(index: Integer): TPhysConst;
begin
  Result:=fList[index] as TPhysConst;
end;

function TPhysConsts.getCount: Integer;
begin
  Result:=fList.Count;
end;

function TPhysConsts.GetActualIndex(i: Integer): Integer;
begin
  Result:=fActualIndex[i];
end;

procedure TPhysConsts.Notification(aComponent: TComponent; Operation: TOperation);
//здесь у нас пустой, совсем пустой компонент, ничего не можем с ним сделать, только добавить
begin
  if (aComponent is TPhysConst) and (operation = opInsert) then
    fList.Add(aComponent);
end;

function TPhysConsts.GetVar(i: Integer): Variant;
begin
  Result:=fsolver.GetVariable(i);
end;

procedure TPhysConsts.Init;
var i: Integer;
    expr: TVariantExpression;
begin
  expr:=TVariantExpression.Create(nil);
  for i:=0 to Count-1 do begin
    expr.SetString(Items[i].formula);
    Items[i].value:=expr.GetVariantValue;
  end;
  expr.Free;
  Recalculate;
end;

function TPhysConsts.Recalculate: Boolean;
var BaseUnitsCount,EqsCount,i,j,ind: Integer;
    formula: TUnitsWithExponents;
    V: TVarWithUnit;
begin
  BaseUnitsCount:=PhysUnitData.fBaseFamilyList.Count;
  EqsCount:=0;
  fActuallyUsedCount:=0;
  SetLength(fActualIndex,count);
  fsolver.SetDimensions(BaseUnitsCount,0);
  for j:=0 to Count-1 do
    if items[j].enabled then begin
      inc(EqsCount);
      fsolver.SetDimensions(BaseUnitsCount,EqsCount);
      if not IsPhysUnit(items[j].value) then
        Raise EPhysUnitError.CreateFmt(FundConstMustBePhys,[items[j].value]);
      V:=TVarWithUnitVarData(items[j].value).Data;
      formula:=V.ConvType.Family.fFormula;
      for i:=0 to formula.fCount-1 do begin
        ind:=PhysUnitData.fBaseFamilyList.IndexOf(formula.unitTypes[i].Family);
        fsolver.Matrix[ind,EqsCount-1]:=formula.Exponents[i];
      end;
      if VarIsNumeric(V.instance) and (V.instance>0) then
        fsolver.Matrix[BaseUnitsCount,EqsCount-1]:=Ln(V.instance)
      else
        Raise EPhysUnitError.CreateFmt(FundPhysConstMustBeRealPositive,[V.AsString]);
      inc(fActuallyUsedCount);
      fActualIndex[fActuallyUsedCount-1]:=j;
    end;
  fsolver.InvertMatrix;
  Result:=(fsolver.GetStatus<>slNoSolution);
end;

(*
    Variant routines
                        *)
function IsPhysUnit(V: Variant): Boolean;
begin
  Result:=(TWrapperVarData(V).VType=VarWithUnitVariantType.VarType);
end;

function IsDimensionless(V: Variant): Boolean;
begin
  //% или ppm тоже подходят
  Result:=IsPhysUnit(V) and (TVarWithUnitVarData(V).Data.ConvType.Family.fFormula.IsUnity);
end;

procedure PhysUnitCreateInto(var ADest: Variant; const Adata: TVarWithUnit);
begin
  VarClear(ADest);
  TWrapperVarData(ADest).VType:=VarWithUnitVariantType.VarType;
  TWrapperVarData(ADest).Data:=Adata;
end;

function TryPhysUnitCreate(text: string; out Res: Variant): boolean;
begin
  Result:=false;
  try
    PhysUnitCreateInto(Res,TVarWithUnit.Create(text));
    Result:=true;
  except
    on E: Exception do
      Res:=E.Message;
  end;
end;
(*
function PhysUnitCreate(text: string): Variant;
begin
  PhysUnitCreateInto(Result,TVarWithUnit.Create(text));
end;
*)

function PhysUnitCreateFromVariant(source: Variant; ConvType: TPhysUnit): Variant;
begin
  if IsPhysUnit(source) then
    if TVarWithUnitVarData(source).Data.ConvType=PhysUnitData.Unity then begin
      Result:=source;
      TVarWithUnitVarData(Result).Data.ConvType:=ConvType;
    end
    else
      Raise EPhysUnitError.CreateFmt(AlreadyHasDimension,[source])
  else
    PhysUnitCreateInto(Result,TVarWithUnit.CreateFromVariant(source,ConvType));
end;

function PhysUnitConvert(source: Variant; DestConvType: TPhysUnit; explicit: boolean=false): Variant;
var inst,dest: TVarWithUnit;
begin
  if not IsPhysUnit(source) then
    source:=PhysUnitCreateFromVariant(source,PhysUnitData.Unity);
  inst:=TVarWithUnitVarData(source).Data;
  dest:=TVarWithUnit.Create;
  try
    dest.Assign(inst);
    dest.Conversion(DestConvType);
    dest.ExplicitConversion:=explicit;
    PhysUnitCreateInto(Result,dest);
  except  //эта хреновина замаскировала ошибку!
    dest.Free;
    raise;
  end;
end;

function PhysUnitPower(source: Variant; pow: Real): Variant;
begin
  Result:=source;
  TVarWithUnitVarData(Result).Data.DoPower(pow);
end;

function VarGetLength(source: Variant): Real;
begin
  if VarIsNumeric(source) then result:=abs(source)
  else if VarIsComplex(source) then Result:=VarComplexAbs(source)
  else Raise Exception.Create('can''t get length of variable');
end;

function PhysUnitFindGoodPrefix(V: Variant): Variant;
var vwithunit: TVarWithUnit;
    L: Real;
begin
  if IsPhysUnit(V) then begin
    vwithunit:=TVarWithUnit.Create;
    vwithunit.Assign(TVarWithUnitVarData(V).Data);
    repeat
      L:=VarGetLength(vwithunit.instance);
      if (L<1) and Assigned(vwithunit.ConvType.ScaledDown) then
        vwithunit.Conversion(vwithunit.ConvType.ScaledDown)
      else if (L>=1000) and Assigned(vwithunit.ConvType.ScaledUp) then
        vwithunit.Conversion(vwithunit.ConvType.ScaledUp)
      else begin
        PhysUnitCreateInto(Result,vwithunit);
        break;
      end;
    until false;
  end
  else
    Result:=V;
end;


initialization
  RegisterClasses([TAffineConvType,TLogarithmicConvType,TNormalConvType,
  TPhysUnitData,TPhysFamily,TUnitPrefix,TUnitPrefixes,TPhysConst]);

  PhysUnitData:=TPhysUnitData.Create(nil);
  InitPhysUnitData;
  VarWithUnitVariantType:=TVarWithUnitType.Create;
finalization
  FreeAndNil(PhysUnitData);
  FreeAndNil(VarWithUnitVariantType);


end.
