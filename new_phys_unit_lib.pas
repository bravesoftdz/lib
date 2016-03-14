unit new_phys_unit_lib;

interface
uses set_english_locale_if_not_sure,streaming_class_lib,IntrospectionLib,classes,
  variantWrapper,Contnrs,sysUtils,simple_parser_lib,linear_eq,
  autocomplete_string_list,syncObjs,variants;
{$DEFINE DirtyHack}
type
  TPhysUnit =class;
  TUnitsWithExponents = class;
  TPhysFamily=class(TIntrospectedStreamingClass) //можно будет и к TComponent вернуться
    private
      fIsBase: Boolean;
      fIsGoodName: Boolean;
      fCaption,fShortName,fDescription: TLocalizedName;
      fBaseType: TPhysUnit;
      fFormula: TUnitsWithExponents;
      fstrformula: string;
      fLocalList: TStringList;

      fMultiplicationList: TBucketList;
      fInversed, fSquared, fCubed: TPhysFamily;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure SetupFormula;
      function index: Integer;
      function BaseIndex: Integer;

      function GetMultipliedBy(fam: TPhysFamily): TPhysFamily;
      function GetDividedBy(fam: TPhysFamily): TPhysFamily;
      function GetInversed: TPhysFamily;
      function GetSquared: TPhysFamily;
      function GetCubed: TPhysFamily;
    published
      property IsBase: Boolean read fIsBase write fIsBase;
      property IsGoodName: Boolean read fIsGoodName write fIsGoodName default false;
      property Caption: TLocalizedName read fCaption write fCaption;
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Description: TLocalizedName read fDescription write fDescription;
      property BaseType: TPhysUnit read fBaseType write fBaseType;
      property formula: string read fstrformula write fstrformula;
  end;

  TPhysUnit=class(TIntrospectedStreamingClass)
    private
      fShortName,fCaption: TLocalizedName;
      fScaledUp,fScaledDown: TPhysUnit;
      fPrefixOK,fIsScaled: boolean;
      fIsBase: Boolean; //чтобы не думать долго при вычислениях
      function GetSomeId: string;
    public
      constructor Create(aOwner: TComponent); override;
      function CreateScaled(mult: Real): TPhysUnit; virtual; abstract;
      function CreateAndConnectScaled(mult: Real): TPhysUnit;
      function ConvertToBase(const value: Variant): Variant; virtual; abstract;
      function ConvertFromBase(const value: Variant): Variant; virtual; abstract;
      function Convert(value: Variant; var ConvType: TPhysUnit): Variant; virtual;
      function Family: TPhysFamily;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); virtual;
      procedure Subtract(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); virtual;
      procedure Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); virtual;
      procedure MultiplyByNumber(var v: Variant; const num: Variant;
         var ConvType: TPhysUnit); virtual; abstract;
      procedure DivideByNumber(var v: Variant; const num: Variant; var ConvType: TPhysUnit); virtual;
      function isNumericallyEqual(un: TPhysUnit): Boolean; virtual; abstract;
      function UniqueName: string;
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
      procedure SetMultiplier(value: Real);
    public
      function ConvertToBase(const value: Variant): Variant; override;
      function ConvertFromBase(const value: Variant): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure Subtract(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure MultiplyByNumber(var V: Variant; const num: Variant;
        var ConvType: TPhysUnit); override;
      procedure DivideByNumber(var V: Variant; const num: Variant;
        var ConvType: TPhysUnit); override;
      function isNumericallyEqual(un: TPhysUnit): Boolean; override;
    published
      property Multiplier: Real read fMultiplier write SetMultiplier;
    end;

  TAffineConvType=class(TPhysUnit)
    private
      fMultiplier, fOffset: Real;
      fFactor: Variant;
      fBaseAffine: TAffineConvType;
      fListOfDerived: TObjectList;
      procedure SetBaseAffine(value: TAffineConvType);
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      function ConvertToBase(const value: Variant): Variant; override;
      function ConvertFromBase(const value: Variant): Variant; override;
      function Convert(value: Variant;var ConvType: TPhysUnit): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure MultiplyByNumber(var V: Variant;const num: Variant;
        var ConvType: TPhysUnit); override;
      function GetAffineUnit(Factor: Variant): TAffineConvType;
      function isNumericallyEqual(un: TPhysUnit): Boolean; override;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
      property Offset: Real read fOffset write fOffset;
      property Factor: Variant read fFactor write fFactor;
      property BaseAffine: TAffineConvType read fBaseAffine write SetBaseAffine;
    end;

  TLogarithmicConvType=class(TPhysUnit)
    private
      fLog10Mult: Real;
      fZeroValue: Real;
    public
      function ConvertToBase(const value: Variant): Variant; override;
      function ConvertFromBase(const value: Variant): Variant; override;
      function CreateScaled(mult: Real): TPhysUnit; override;
      procedure Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);override;
      procedure Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit); override;
      procedure MultiplyByNumber(var V: Variant; const num: Variant;
        var ConvType: TPhysUnit); override;
      function isNumericallyEqual(un: TPhysUnit): Boolean; override;
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

  TUnitPrefixes = class(TIntrospectedStreamingClass)
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

  TPhysConsts = class(TIntrospectedStreamingClass)
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

  TPhysUnitData = class(TIntrospectedStreamingClass)
  private
    fFamilyList: TObjectList;
    fBaseFamilyList: TObjectList;
    fMegaList: TStringList;
    fAutocompleteList: TAutocompleteStringList;
    fWarningList: TStringList;
    fUnity,fDMS,fRadian: TPhysUnit;
    fSuspiciousList: TStringList;

    fTmpFormula: TUnitsWithExponents;
    fLocName: TLocalizedName;
    fCriticalSection: TCriticalSection;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    warningproc: TPhysUnitMessagesProc;
    infoproc: TPhysUnitMessagesProc;
    procedure AddToMegalist(ct: TPhysUnit);
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function StrToConvType(str: string): TPhysUnit;
    function isAmbigious(str: string): boolean;
    property Unity: TPhysUnit read fUnity;
    property DMS: TPhysUnit read fDMS;
    property Radian: TPhysUnit read fRadian;
    property AutoCompleteList: TAutocompleteStringList read fAutocompleteList;
  published
    UnitPrefixes: TUnitPrefixes;
    PhysConsts: TPhysConsts;
    property SuspiciousList: TStringList read fSuspiciousList write fSuspiciousList;
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
      procedure ShowLocalized(proc: TShowLocalizedName; dest: TLocalizedName);
      procedure SimplifiedShowLocalized(proc: TShowLocalizedName; dest: TLocalizedName);
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
      procedure ShowLocFormula(dest: TLocalizedName);
      procedure ShowLocShortName(dest: TLocalizedName);
      function GetPhysFamily: TPhysFamily;
      function GetConvType: TPhysUnit;  //должен от нее однажды избавиться
      function IsUnity: Boolean;
  end;

  TVarWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TPhysUnit;
    public
      instance: Variant; //та переменная, которую мы оборачиваем
      ExplicitConversion: boolean;  //флаг, что величину "насильно" привели к данному виду
      procedure Release; override;
      class function GetInstance: TAbstractWrapperData; override;
      class function CreateFromText(text: string): TVarWithUnit;
      class function CreateFromVariant(const source: Variant; aConvType: TPhysUnit): TVarWithUnit;
      destructor Destroy; override; //debug purposes
      procedure Assign(source: TPersistent); override;
      procedure FastAssign(source: TAbstractWrapperData); override;
      procedure DoNegate; override; //взять обратный знак
      procedure DoAdd(const value: TAbstractWrapperData); override;
      procedure DoAddNumber(const Right: Variant);
      procedure DoSubtract(const Right: TAbstractWrapperData); override;
      procedure DoSubtractNumber(const Right: Variant);
//      procedure DoInvSubtractNumber(const Left: TVarData);
      procedure DoMultiply(const Right: TAbstractWrapperData); override;
      procedure DoMultiplyNumber(const value: Variant);
      procedure DoInverse;
      procedure DoDivide(const Right: TAbstractWrapperData); override;
      procedure DoDivideNumber(const Right: Variant);
//      procedure DoInvDivideNumber(const Left: TVarData);
      procedure DoPower(pow: Real);
      procedure DoSquare;
      procedure DoAbsSquared;
      procedure DoCube;
      function GetAsString: string; override;
      function GetLengthSquared: Real;
      procedure Conversion(DestConv: TPhysUnit);
    published
      procedure Add(value: Variant);
      property LengthSquared: Real read GetLengthSquared;
      property isExplicitlyConverted: boolean read ExplicitConversion;
  end;

  TVarWithUnitType = class (TAbstractWrapperVariantType)
  protected
    function RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;    
    function CompareOp(const Left, Right: TVarData; const Operator: Integer): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
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
  function IsPhysUnit(const V: Variant): Boolean;
  function IsDimensionless(const V: Variant): Boolean;
  procedure PhysUnitCreateInto(var ADest: Variant; const Adata: TVarWithUnit);
  function TryPhysUnitCreate(text: string; out Res: Variant): boolean;
  function PhysUnitCreateFromVariant(const source: Variant; ConvType: TPhysUnit): Variant; overload;
  function PhysUnitCreateFromVariant(const source: Variant; ConvType: string): Variant; overload;
  function PhysUnitCreate(text: string): Variant; overload;
  function PhysUnitCreate(value: Real; ConvType: string): Variant; overload;
  function PhysUnitConvert(const source: Variant; DestConvType: TPhysUnit; explicit: boolean=false): Variant; overload;
  function PhysUnitConvert(const source: Variant; DestType: string): Variant; overload;
  function PhysUnitPar(const a,b: Variant): Variant;
  function PhysUnitPower(const source: Variant; pow: Real): Variant;
  function PhysUnitSquare(const source: Variant): Variant;
  function PhysUnitCube(const source: Variant): Variant;
  function PhysUnitAbsSquared(const source: Variant): Variant;
  function PhysUnitSqrt(const source: Variant): Variant;
  function PhysUnitAbs(const source: Variant): Variant;
  function PhysUnitArg(const source: Variant): Variant;
  function PhysUnitRe(const source: Variant): Variant;
  function PhysUnitIm(const source: Variant): Variant;
  function PhysUnitConj(const source: Variant): Variant;  
  function PhysUnitExp(const source: Variant): Variant;
  function PhysUnitFindGoodPrefix(const V: Variant): Variant;
  function PhysUnitGetNumberIn(const source: Variant; UnitName: TPhysUnit): Variant; overload;
  function PhysUnitGetNumberIn(const source: Variant; UnitName: string): Variant; overload;
  function PhysUnitGetNumber(const source: Variant): Variant;
  function NoSpaces(str: string): String;
  function StrToPhysFamily(str: string): TPhysFamily;
  function StrToPhysUnit(str: string): TPhysUnit;
  function IsPhysUnitSameFamily(const v1,v2: Variant): Boolean;
  function PhysUnitGetConvType(const source: Variant): TPhysUnit;
  function VarGetLengthSquared(const value: Variant): Real;

  var PhysUnitData: TPhysUnitData;
      VarWithUnitVariantType: TVarWithUnitType;

      PhysUnitPool: TVarWithUnit;
implementation

uses math,strUtils,VarCmplxMk2,expression_lib;

resourcestring
  AbstractErrorAdd = 'could not add %s and %s';
  AbstractErrorPar = 'could not compute %s || %s ("parallel connection")';
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
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
begin
  Result:=NoSpaces(str);
  if Length(Result)=0 then Exit;
  if not (Result[1] in Alpha) then
    Result[1]:='_';
  for i:=2 to Length(Result) do
    if not (Result[i] in Alphanumeric) then
      Result[i]:='_';
end;

function ConvTypeToLocShortName(ConvType: TPhysUnit): TLocalizedName;
begin
  Result:=ConvType.ShortName;
end;

function ConvTypeToLocFamilyLetter(ConvType: TPhysUnit): TLocalizedName;
begin
  if ConvType.Family.ShortName.Enabled then
    Result:=ConvType.family.ShortName
  else
    Result:=ConvType.family.Caption;
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

  fMultiplicationList:=TBucketList.Create;
end;

destructor TPhysFamily.Destroy;
begin
  fLocalList.Free;
  fMultiplicationList.Free;
  inherited Destroy;
end;

function TPhysFamily.index: Integer;
begin
  Result:=(Owner as TPhysUnitData).fFamilyList.IndexOf(self);
end;

function TPhysFamily.BaseIndex: Integer;
begin
  Result:=(Owner as TPhysUnitData).fBaseFamilyList.IndexOf(self);
end;

procedure TPhysFamily.SetupFormula;
begin
  if IsBase then fFormula.SetBaseType(BaseType)
  else fFormula.TakeFromString(formula);
end;

function TPhysFamily.GetMultipliedBy(fam: TPhysFamily): TPhysFamily;
var link: Pointer;
    fla: TUnitsWithExponents;
begin
  if fMultiplicationList.Find(Pointer(fam),link) then
    Result:=TPhysFamily(link)
  else begin
    fla:=TUnitsWithExponents.Create(self);
    fla.Assign(fFormula);
    fla.Multiply(fam.fFormula);
    Result:=fla.GetPhysFamily;
    fMultiplicationList.Add(Pointer(fam),Pointer(Result));
    fla.Free;
  end;
end;

function TPhysFamily.GetDividedBy(fam: TPhysFamily): TPhysFamily;
begin
  Result:=GetMultipliedBy(fam.GetInversed);
end;

function TPhysFamily.GetInversed: TPhysFamily;
var fla: TUnitsWithExponents;
begin
  if not Assigned(finversed) then begin
    fla:=TUnitsWithExponents.Create(self);
    fla.Assign(fFormula);
    fla.DoPower(-1);
    fInversed:=fla.GetPhysFamily;
    fla.Free;
  end;
  Result:=fInversed;
end;


function TPhysFamily.GetSquared: TPHysFamily;
var fla: TUnitsWithExponents;
begin
  if not Assigned(fSquared) then begin
    fla:=TUnitsWithExponents.Create(self);
    fla.Assign(fFormula);
    fla.DoPower(2);
    fSquared:=fla.getPhysFamily;
    fla.Free;
  end;
  Result:=fSquared;
end;


function TPhysFamily.GetCubed: TPHysFamily;
var fla: TUnitsWithExponents;
begin
  if not Assigned(fCubed) then begin
    fla:=TUnitsWithExponents.Create(self);
    fla.Assign(fFormula);
    fla.DoPower(3);
    fCubed:=fla.getPhysFamily;
    fla.Free;
  end;
  Result:=fCubed;
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

procedure TPhysUnit.Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  Raise EPhysUnitError.CreateFmt(AbstractErrorAdd,[Caption.Caption,t.Caption.Caption]);
end;

procedure TPhysUnit.Subtract(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  Raise EPhysUnitError.CreateFmt(AbstractErrorAdd,[Caption.Caption,t.Caption.Caption]);
end;

procedure TPhysUnit.Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  Raise EPhysUnitError.CreateFmt(AbstractErrorPar,[Caption.Caption,t.Caption.Caption]);
end;

procedure TPhysUnit.DivideByNumber(var v: Variant; const num: Variant; var ConvType: TPhysUnit);
begin
  MultiplyByNumber(v,1/num,ConvType);
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

function TPhysUnit.UniqueName: string;
begin
  if ShortName.Enabled and not PhysUnitData.isAmbigious(NoSpaces(ShortName.Caption)) then
    Result:=NoSpaces(ShortName.Caption)
  else if Caption.Enabled and not PhysUnitData.isAmbigious(NoSpaces(Caption.Caption)) then
    Result:=NoSpaces(Caption.Caption)
  else begin
    if family.ShortName.Enabled then Result:=NoSpaces(family.Shortname.Caption)
    else Result:=NoSpaces(family.Caption.Caption);
    if ShortName.Enabled then
      Result:=Result+'.'+NoSpaces(ShortName.Caption)
    else if Caption.Enabled then
      Result:=Result+'.'+NoSpaces(Caption.Caption)
    else Result:=name;
  end;
end;
(*
    TNormalConvType
                      *)
procedure TNormalConvType.SetMultiplier(value: Real);
begin
  fMultiplier:=value;
  fIsBase:=(value=1);
end;

procedure TNormalConvType.Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
//когда выполняется Add, уже проверено, что размерности совпадают
//  if Owner=t.Owner then begin //same family
    if t=self then
      {$ifdef Dirtyhack}
        CallVarAdd(v1,v2)
      {$ELSE}
        v1:=v1+v2
      {$ENDIF}
    else
      {$ifdef Dirtyhack}
        CallVarAdd(v1,ConvertFromBase(t.ConvertToBase(v2)));
      {$else}
        v1:=v1+ConvertFromBase(t.ConvertToBase(v2));
      {$ENDIF}
    ConvType:=self;
//  end
//  else inherited;
end;

procedure TNormalConvType.Subtract(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  //уже проверено, что размерности совпадают
  if t=self then
    {$ifdef DirtyHack}
      CallVarSub(v1,v2)
    {$else}
      v1:=v1-v2
    {$endif}
  else
    {$ifdef DirtyHack}
      CallVarSub(v1,ConvertFromBase(t.ConvertToBase(v2)));
    {$else}
      v1:=v1-ConvertFromBase(t.ConvertToBase(v2));
    {$endif}
  ConvType:=self;
end;

procedure TNormalConvType.Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  if Owner=t.Owner then begin //same family
    if v1<>0 then begin //иначе заведомо ноль - пусть он и останется!
      if t=self then
        v1:=v1*v2/(v1+v2)
      else
        v1:=1/(1/v1+1/ConvertFromBase(t.ConvertToBase(v2))); //даже Normal||Log - правильно преобр
    end;
    ConvType:=self;
  end
  else inherited;
end;

function TNormalConvType.ConvertFromBase(const value: Variant): Variant;
begin
  Result:=value/multiplier;
end;

function TNormalConvType.ConvertToBase(const value: Variant): Variant;
begin
  Result:=value*multiplier;
end;

procedure TNormalConvType.MultiplyByNumber(var V: Variant;const num: Variant;
  var ConvType: TPhysUnit);
begin
  {$ifdef dirtyhack}
    CallVarMul(V,num);
  {$else}
    v:=v*num;
  {$endif}
  ConvType:=self;
end;

procedure TNormalConvType.DivideByNumber(var V: Variant; const num: Variant;
  var ConvType: TPhysUnit);
begin
  {$ifdef dirtyhack}
    CallVarDiv(V,num);
  {$else}
    v:=v/num;
  {$endif}
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

function TNormalConvType.isNumericallyEqual(un: TPhysUnit): Boolean;
var nct: TNormalConvType absolute un;
begin
  Result:=(un is TNormalConvType) and (nct.Multiplier=Multiplier);
end;

{ TAffineConvType }

constructor TAffineConvType.Create(aOwner: TComponent);
const one: Real = 1;
begin
  inherited Create(aOwner);
  fListOfDerived:=TObjectList.Create(false);
  Factor:=one;  //чтобы это получился именно real!
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

function TAffineConvType.ConvertToBase(const value: Variant): Variant;
begin
  Result:=value*Multiplier+Offset;
end;

function TAffineConvType.ConvertFromBase(const value: Variant): Variant;
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

procedure TAffineConvType.Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
var t2a: TAffineConvType absolute t;
begin
  if Owner = t.Owner then begin
    v1:=v1+Convert(v2,t); //может ругнуться на приведение ConvType as TAffineConvType
    ConvType:=GetAffineUnit(Factor+t2a.Factor); //если досюда дошли, уже безопасно absolute
  end
  else inherited;
end;

procedure TAffineConvType.Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  if Owner = t.Owner then begin
    if (self=t) and (self=Family.BaseType) then begin
      if v1<>0 then
        v1:=v1*v2/(v1+v2) //в противном случае пусть остается 0
    end
    else begin
      v1:=ConvertToBase(v1);
      if v1<>0 then
        v1:=1/(1/v1+1/ConvertToBase(v2));
    end;
    ConvType:=Family.BaseType;
  end
  else inherited;
end;

procedure TAffineConvType.MultiplyByNumber(var v: Variant; const num: Variant;
  var ConvType: TPhysUnit);
begin
  v:=v*num;
  ConvType:=GetAffineUnit(Factor*num);
end;

function TAffineConvType.GetAffineUnit(Factor: Variant): TAffineConvType;
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
  der.Factor:=VarComplexSimplify(Factor);
  der.Offset:=der.Offset*Factor;
  der.BaseAffine:=BaseAffine; //на этом этапе он должен автоматом появиться в списке
  //теперь украшательства - имя ему изменяем
  for i:=0 to der.ShortName.strings.Count-1 do
    if Factor=0 then
      der.ShortName.strings[i]:=Format('%s{dif}',[der.ShortName.strings[i]])
    else
      der.ShortName.strings[i]:=Format('%s{%s}',[der.ShortName.strings[i],der.Factor]);
  for i:=0 to der.Caption.strings.Count-1 do
    if Factor=0 then
      der.Caption.strings[i]:=Format('%s{dif}',[der.Caption.strings[i]])
    else
      der.Caption.strings[i]:=Format('%s{%s}',[der.Caption.strings[i],der.Factor]);
  der.ensureCorrectName(ToAcceptableName(der.Caption.InEnglish),owner);
  owner.InsertComponent(der);
  Result:=der;
  PhysUnitData.AddToMegalist(Result);
  //и в megalist добавить неплохо бы! Хотя это половинчатое решение - надо по-хорошему
  //парсить выражение a{b}
end;

function TAffineConvType.isNumericallyEqual(un: TPhysUnit): Boolean;
var act: TAffineConvType absolute un;
begin
  Result:=(un is TAffineConvType) and (act.Multiplier=Multiplier) and
    (act.Offset=Offset) and (act.Factor=Factor);
end;

(*
    TLogarithmicConvType
                          *)


procedure TLogarithmicConvType.Add(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  if Owner=t.Owner then begin
    v1:=ConvertFromBase(ConvertToBase(v1)+t.ConvertToBase(v2));
    ConvType:=self;
  end
  else inherited;
end;

procedure TLogarithmicConvType.Par(var V1: Variant; out ConvType: TPhysUnit; const V2: Variant; t: TPhysUnit);
begin
  if Owner = t.Owner then begin
    if self=t then
      V1:=V1+V2-fLog10Mult*log10(Power(10,V1/fLog10Mult)+Power(10,V2/fLog10Mult))
    else
      V1:=V1+fLog10Mult*log10(t.ConvertToBase(V2)/(fZeroValue*Power(10,V1/fLog10Mult)+t.ConvertToBase(V2)));
    ConvType:=self;
  end
  else inherited;
end;

function TLogarithmicConvType.ConvertFromBase(const value: Variant): Variant;
begin
//  if VarIsNumeric(value) then
//    Result:=Log10Multiplier*log10(value/ZeroValue)
//  else
    Result:=Log10Multiplier*VarComplexLog10(value/ZeroValue);
//    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.ConvertToBase(const value: Variant): Variant;
begin
//  if VarIsNumeric(value) then
//    Result:=ZeroValue*power(10.0,(value/Log10Multiplier))
//  else
    Result:=VarComplexSimplify(ZeroValue*VarComplexExp(value*ln(10)/Log10Multiplier))
//    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
end;

procedure TLogarithmicConvType.MultiplyByNumber(var v: Variant; const num: Variant;
  var ConvType: TPhysUnit);
begin
  v:=v+log10multiplier*VarComplexLog10(num);
  ConvType:=self;
(*
  if VarIsNumeric(v1) and VarIsNumeric(num) and (num>0) then begin
    Result:=v1+log10multiplier*log10(num);
    ConvType:=self;
  end
  else
    Raise EPhysUnitError.Create(LogarithmicConvTypeRequiresRealNum);
    *)
end;

function TLogarithmicConvType.CreateScaled(mult: Real): TPhysUnit;
var cpy: TLogarithmicConvType absolute Result;
begin
  Result:=TLogarithmicConvType.Clone(self);
  Result.PrefixOk:=false;
  Result.IsScaled:=true;
  cpy.Log10Multiplier:=Log10Multiplier/mult;
end;

function TLogarithmicConvType.isNumericallyEqual(un: TPhysUnit): Boolean;
var lct: TLogarithmicConvType absolute un;
begin
  Result:=(un is TLogarithmicConvType) and (lct.Log10Multiplier=Log10Multiplier)
    and (lct.ZeroValue=ZeroValue);
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
  fLocName:=TLocalizedName.Create(self);
  fTmpFormula:=TUnitsWithExponents.Create(self);
  fCriticalSection:=TCriticalSection.Create;
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
  fSuspiciousList:=TStringList.Create;
  fSuspiciousList.CaseSensitive:=true;

end;

destructor TPhysUnitData.Destroy;
begin
  //debug
//  fMegaList.SaveToFile('megalist.txt');
  //end of debug
  fMegaList.Free;
  fAutocompleteList.Free;
  fWarningList.Free;
  fFamilyList.Free;
  fBaseFamilyList.Free;
  fSuspiciousList.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TPhysUnitData.AddToMegalist(ct: TPhysUnit);
var fam: TPhysFamily;

    procedure AddToList;
    var i: Integer;
    begin
      for i:=0 to fLocName.strings.Count-1 do begin
        if fMegaList.IndexOf(fLocName.strings[i])>=0 then
          fWarningList.Add(Format('%s ident ambiguity',[fLocName.strings[i]]));
        fMegaList.AddObject(NoSpaces(fLocName.strings[i]),ct);
        fAutocompleteList.AddObject(NoSpaces(fLocName.strings[i]),ct);
      end;
    end;

begin
  //в каждый момент времени должна создаваться только одна ед. изм., иначе
  //могут пойти повторы
  //поэтому где-то мы уже вошли в крит. секцию, прежде чем вызывать AddToMegaList
  //безопасно пользоваться fLocName
  fam:=ct.Family;
  fLocName.Assign(ct.ShortName);
  AddToList;
  fLocName.LeftConcat(fam.Caption,'.');
  AddToList;
  fLocName.Assign(ct.ShortName);
  fLocName.LeftConcat(fam.ShortName,'.');
  AddToList;
  fLocName.Assign(ct.Caption);
  AddToList;
  fLocName.LeftConcat(fam.Caption,'.');
  AddToList;
  fLocName.Assign(ct.Caption);
  fLocName.LeftConcat(fam.ShortName,'.');
  AddToList;
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
        if cpy.isNumericallyEqual(prev) then begin
          if Assigned(cpy.ScaledUp) then
            cpy.ScaledUp.ScaledDown:=prev;
          if Assigned(cpy.ScaledDown) then
            cpy.ScaledDown.ScaledUp:=prev;
          prev.ScaledUp:=cpy.ScaledUp;
          prev.ScaledDown:=cpy.ScaledDown;
          cpy.Free;
          Exit;
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
            cpy.ScaledDown:=ct;
            cpy.ScaledUp:=ct; //условимся, что ScaledUp=ScaledDown означает ссылку на осн. объект
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

    fWarningList.SaveToFile('warnings.txt');
  end;
end;

procedure TPhysUnitData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opInsert) and (AComponent is TPhysFamily) then
    fFamilyList.Add(AComponent);
  inherited;
end;

function TPhysUnitData.isAmbigious(str: string): boolean;
var i: Integer;
begin
  if fmegalist.Find(str,i) then
    Result:=((i+1<fMegaList.Count) and (fMegaList[i+1]=fMegaList[i])) or (SuspiciousList.IndexOfName(str)>=0)
  else
    Result:=true; //если строки в списке нет - надо еще подумать, что вернуть
end;

function TPhysUnitData.StrToConvType(str: string): TPhysUnit;
var i,j,k: Integer;
    obj: TPhysUnit;
    fml: TUnitsWithExponents;
    multiplier: Real;
    NormalRslt: TNormalConvType absolute Result;
    p: TSimpleParser;
begin
  //здесь, случается, создается новая величина, но только при пользовательском
  //вводе и на стадиях всяких иниц. вообще нужно сделать "железный" thread-safe
  i:=fMegaList.IndexOf(NoSpaces(str));
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
    else begin
      j:=SuspiciousList.IndexOfName(str);
      if (j>=0) and Assigned(warningProc) then
        WarningProc(SuspiciousList.ValueFromIndex[j]);
      Result:=TPhysUnit(fMegaList.Objects[i])
    end;
  end
  else begin
  //ничего не нашли, это вестимо что-то составное
  //костыль, избежать Stack Overflow
    p:=TPhysUnitParser.Create(str);
    try
      if p.getPhysUnitIdent=str then Raise EPhysUnitError.CreateFmt('StrToConvType: unit %s not found',[str]);
    finally
      p.Free;
    end;
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
        AddToMegaList(Result);
      end;
    end;
  end;
end;

(*
    TVarWithUnit
                    *)
procedure TVarWithUnit.Release;
begin
  ConvType:=TPhysUnit(PhysUnitPool);  //добавили к пулу еще один элем.
  PhysUnitPool:=self;
end;

class function TVarWithUnit.getInstance: TAbstractWrapperData;
begin
  if Assigned(PhysUnitPool) then begin
    Result:=PhysUnitPool;
    PhysUnitPool:=TVarWithUnit(PhysUnitPool.ConvType);
  end
  else
    Result:=TVarWithUnit.Create;
end;

procedure TVarWithUnit.Assign(source: TPersistent);
var s: TVarWithUnit absolute source;
begin
  if source is TVarWithUnit then begin
    instance:=s.instance; //variant'ы - они умные, скопируются
    ConvType:=s.ConvType;
  end
  else inherited Assign(source);
end;

procedure TVarWithUnit.FastAssign(source: TAbstractWrapperData);
begin
  instance:=TVarWithUnit(source).instance;
  ConvType:=TVarWithUnit(source).ConvType;
end;

class function TVarWithUnit.CreateFromText(text: string): TVarWithUnit;
var expr: TVariantExpression;
    v: Variant;
begin
  Result:=TVarWithUnit(GetInstance);
  expr:=TVariantExpression.Create(nil);
  expr.SetString(text);
  v:=expr.GetVariantValue;  //сейчас так сделано, что v может и не быть VarWithUnit
  Result.FastAssign(TVarWithUnitVarData(v).Data);
  expr.free;
end;

class function TVarWithUnit.CreateFromVariant(const source: Variant;
  aConvType: TPhysUnit): TVarWithUnit;
begin
  Result:=TVarWithUnit(GetInstance);
  Result.instance:=source;
  Result.ConvType:=aConvType;
end;

destructor TVarWithUnit.Destroy;
begin
  inherited Destroy;
end;

function TVarWithUnit.GetAsString: string;
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
    s:=ConvType.UniqueName;
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
//  if (ConvType<>DestConv) then begin
    if ConvType.Owner=DestConv.Owner then //same family
      instance:=DestConv.Convert(instance,ConvType)
    else begin
      if (instance=0) and ((ConvType is TNormalConvType) or
      ((ConvType is TAffineConvType) and (TAffineConvType(ConvType).Factor=0))) then begin
        ConvType:=DestConv;
        Exit;
      end;
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
//  end;
end;

procedure TVarWithUnit.DoAdd(const value: TAbstractWrapperData);
var v: TVarWithUnit absolute value;
begin
  if ConvType.Owner<>v.ConvType.Owner then begin
    if instance=0 then begin
      instance:=v.instance;
      ConvType:=v.ConvType;
      Exit;
    end;
    v.Conversion(ConvType);
  end;
  ConvType.Add(instance,ConvType,v.instance,v.ConvType);
end;

procedure TVarWithUnit.DoAddNumber(const Right: Variant);
var un: TVarWithUnit;
begin
  if ConvType=PhysUnitData.Unity then
    CallVarAdd(instance,Right)
  else if instance=0 then begin
    instance:=Right;
    ConvType:=PhysUnitData.Unity;
  end
  else begin
    un:=TVarWithUnit(TVarWithUnit.GetInstance);
    try
      un.instance:=Right;
      un.ConvType:=PhysUnitData.Unity;
      un.Conversion(ConvType);
      DoAdd(un);
    finally
      un.Free;
    end;
  end;
end;

procedure TVarWithUnit.DoSubtractNumber(const Right: Variant);
var un: TVarWithUnit;
begin
  if ConvType=PhysUnitData.Unity then
    CallVarSub(instance,Right)
  else if instance=0 then begin
    instance:=-Right;
    ConvType:=PhysUnitData.Unity;
  end
  else begin
    un:=TVarWithUnit(TVarWithUnit.GetInstance);
    try
      un.instance:=Right;
      un.ConvType:=PhysUnitData.Unity;
      un.Conversion(ConvType);
      DoSubtract(un);
    finally
      un.Free;
    end;
  end;
end;

procedure TVarWithUnit.DoMultiplyNumber(const value: Variant);
begin
  ConvType.MultiplyByNumber(instance,value,ConvType);
end;

procedure TVarWithUnit.DoDivideNumber(const Right: Variant);
begin
  ConvType.DivideByNumber(instance,Right,ConvType);  
end;

procedure TVarWithUnit.DoSubtract(const Right: TAbstractWrapperData);
var v: TVarWithUnit absolute Right;
begin
//  if Right is TVarWithUnit then begin
    if ConvType.Owner<>v.ConvType.Owner then begin
      if instance=0 then begin
        instance:=v.instance;
        ConvType:=v.ConvType;
        DoNegate;
        Exit;
      end;
      v.Conversion(ConvType);
    end;
    ConvType.Subtract(instance,ConvType,v.instance,v.ConvType);
//  end
//  else inherited;
end;

procedure TVarWithUnit.Add(value: Variant);
var un: TVarWithUnit;
begin
  if IsPhysUnit(value) then
    DoAdd(TVarWithUnitVarData(value).Data)
  else begin
    un:=TVarWithUnit.CreateFromVariant(value,PhysUnitData.Unity);
    DoAdd(un);
    un.Free;
  end;
end;

procedure TVarWithUnit.DoDivide(const Right: TAbstractWrapperData);
var v: TVarWithUnit absolute Right;
begin
//  if Right is TVarWithUnit then begin
    if v.ConvType.Family.fFormula.IsUnity then begin
      //делим на безразмерную вел., но возможно % или ppm и пр
      if v.ConvType.fIsBase then
        ConvType.DivideByNumber(instance,v.instance,ConvType)
      else
        ConvType.DivideByNumber(instance,v.ConvType.ConvertToBase(v.instance),ConvType);
    end
    else begin
      //основная процедура
      if not ConvType.fIsBase then
        Conversion(ConvType.family.BaseType);
      if v.ConvType.fIsBase then
        {$ifdef Dirtyhack}
        CallVarDiv(instance,v.instance)
        {$ELSE}
        instance:=instance/v.instance
        {$ENDIF}
      else
        {$ifdef Dirtyhack}
        CallVarDiv(instance,v.ConvType.ConvertToBase(v.instance));
        {$ELSE}
        instance:=instance/v.ConvType.ConvertToBase(v.instance);
        {$ENDIF}
      ConvType:=ConvType.family.GetDividedBy(v.ConvType.Family).BaseType;
      //если получилась афинная величина на выходе, нужно сделать ее {0}
      if ConvType is TAffineConvType then
        ConvType:=(ConvType as TAffineConvType).GetAffineUnit(0);
    end;
//  end
//  else inherited;
end;

procedure TVarWithUnit.DoInverse;
begin
  Conversion(ConvType.Family.BaseType);
  instance:=1/instance; //это можно, поскольку баз. тип - нормальный!
  ConvType:=ConvType.family.GetInversed.BaseType;
end;

procedure TVarWithUnit.DoMultiply(const Right: TAbstractWrapperData);
var v: TVarWithUnit absolute Right;
  procedure DealWithLeftUnity;
  var tmp: Variant;
  begin
    tmp:=v.instance;
    if ConvType.fIsBase then
      v.ConvType.MultiplyByNumber(tmp,instance,ConvType)
    else
      v.ConvType.MultiplyByNumber(tmp,ConvType.ConvertToBase(instance),ConvType);
    instance:=tmp;
  end;

begin
//  if Right is TVarWithUnit then begin
    if v.ConvType.Family.fFormula.IsUnity then  //безразмерная, но может быть и % или ppm
      if v.ConvType.fIsBase then
        ConvType.MultiplyByNumber(instance,v.instance,ConvType)
      else
        ConvType.MultiplyByNumber(instance,v.ConvType.ConvertToBase(v.instance),ConvType)
    else if ConvType.Family.fFormula.IsUnity then DealWithLeftUnity
    else begin
      //основная процедура
      if not ConvType.fIsBase then
        Conversion(ConvType.family.BaseType);
      if v.ConvType.fIsBase then
        {$ifdef Dirtyhack}
        CallVarMul(instance,v.instance)
        {$ELSE}
        instance:=instance*v.instance
        {$ENDIF}
      else
        {$ifdef Dirtyhack}
        CallVarMul(instance,v.ConvType.ConvertToBase(v.instance));
        {$ELSE}
        instance:=instance*v.ConvType.ConvertToBase(v.instance);
        {$ENDIF}
      ConvType:=ConvType.family.GetMultipliedBy(v.ConvType.Family).BaseType;
      //если получилась афинная величина на выходе, нужно сделать ее {0}
      if ConvType is TAffineConvType then
        ConvType:=(ConvType as TAffineConvType).GetAffineUnit(0);
    end
//  end
//  else inherited;
end;

procedure TVarWithUnit.DoPower(pow: Real);
var U: TUnitsWithExponents;
begin
  U:=TUnitsWithExponents.Create(nil);
  try
    U.Assign(ConvType.Family.fFormula);
    if not ConvType.fIsBase then
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

procedure TVarWithUnit.DoSquare;
begin
  if not ConvType.fIsBase then
    Conversion(ConvType.family.BaseType);
  instance:=instance*instance;
  ConvType:=ConvType.family.getSquared.fBaseType;
end;

procedure TVarWithUnit.DoAbsSquared;
begin
  if not ConvType.fIsBase then
    Conversion(ConvType.family.BaseType);
  instance:=VarComplexAbsSqr(instance);
//  ConvType:=ConvType.Family.getSquared.fBaseType;
  ConvType:=ConvType.family.getMultipliedBy(ConvType.Family).fBaseType;
end;

procedure TVarWithUnit.DoCube;
begin
  if not ConvType.fIsBase then
    Conversion(ConvType.family.BaseType);
  instance:=instance*instance*instance;
  ConvType:=ConvType.family.getMultipliedBy(ConvType.Family).
                              GetMultipliedBy(ConvType.Family).fBaseType;
end;

procedure TVarWithUnit.DoNegate;
begin
  ConvType.MultiplyByNumber(instance,-1.000000000,ConvType);
end;

function TVarWithUnit.GetLengthSquared: Real;
begin
  Result:=VarGetLengthSquared(instance);
end;

{ TVarWithUnitType }

function TVarWithUnitType.RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  RequiredVarType:=V.VType;
  Result:=true;
end;

procedure TVarWithUnitType.BinaryOp(var Left: TVarData;
  const Right: TVarData; const Operator: TVarOp);

  procedure DealWithNumPlusPhysUnit;
  var t: TVarWithUnit;
  begin
  //возможны утечки памяти при возн. ошибки
    case Operator of
      opAdd: begin
        if TVarWithUnitVarData(Right).data.ConvType=PhysUnitData.Unity then
          CallVarAdd(Variant(Left),TVarWithUnitVarData(Right).Data.instance)
        else begin
          t:=TVarWithUnit(TVarWithUnit.GetInstance);
          t.FastAssign(TVarWithUnitVarData(Right).data);
          if Variant(Left)<>0 then
            t.DoAddNumber(Variant(Left));
          //Left был numeric, значит, память не пропадает, мы его просто сейчас
          //перезапишем
          Left.VType:=VarType;
          TVarWithUnitVarData(Left).data:=t;
        end;
      end;
      opSubtract: begin
        if TVarWithUnitVarData(Right).Data.ConvType=PhysUnitData.Unity then
          CallVarSub(Variant(Left),TVarWithUnitVarData(Right).Data.instance)
        else begin
          t:=TVarWithUnit(TVarWithUnit.GetInstance);
          t.FastAssign(TVarWithUnitVarData(Right).data);
          t.DoNegate;
          if Variant(Left)<>0 then
            t.DoAddNumber(Variant(Left));
          //Left был numeric, значит, память не пропадает, мы его просто сейчас
          //перезапишем
          Left.VType:=VarType;
          TVarWithUnitVarData(Left).data:=t;
        end;
      end;
      opMultiply:
        RaiseInvalidOp;
//            co.MulReal(Variant(Left));
      opDivide:
        RaiseInvalidOp;
//            co.InvDivReal(Variant(Left));
      else
          RaiseInvalidOp;
    end;
  end;

begin
  if Right.VType = VarType then
    case Left.VType of
      varString:
        case Operator of
          opAdd:
            Variant(Left) := Variant(Left) + TVarWithUnitVarData(Right).Data.GetAsString;
        else
          RaiseInvalidOp;
        end;
      else
        if Left.VType = VarType then
          case Operator of
            opAdd:
              TVarWithUnitVarData(Left).Data.DoAdd(TVarWithUnitVarData(Right).Data);
            opSubtract:
              TVarWithUnitVarData(Left).Data.DoSubtract(TVarWithUnitVarData(Right).Data);
            opMultiply:
              TVarWithUnitVarData(Left).Data.DoMultiply(TVarWithUnitVarData(Right).Data);
            opDivide:
              TVarWithUnitVarData(Left).Data.DoDivide(TVarWithUnitVarData(Right).Data);
          else
            RaiseInvalidOp;
          end
        else
          DealWithNumPlusPhysUnit
    end
  else  //справа число, а не PhysUnit
    case Operator of
      opAdd:
        TVarWithUnitVarData(Left).data.DoAddNumber(Variant(Right));
      opSubtract:
        TVarWithUnitVarData(Left).data.DoSubtractNumber(Variant(Right));
      opMultiply:
        TVarWithUnitVarData(Left).Data.ConvType.MultiplyByNumber(TVarWithUnitVarData(Left).data.instance,Variant(Right),TVarWithUnitVarData(Left).data.convtype);
      opDivide:
//        TVarWithUnitVarData(Left).Data.DoDivideNumber(Variant(Right));
        TVarWithUnitVarData(Left).Data.ConvType.DivideByNumber(TVarWithUnitVarData(Left).data.instance,Variant(Right),TVarWithUnitVarData(Left).data.convtype);
      else
      RaiseInvalidOp;
    end;
end;

procedure TVarWithUnitType.Cast(var Dest: TVarData;
  const Source: TVarData);
begin
  //строку преобразуем по всем правилам, а любые другие Variant'ы "оборачиваем" безразм.
  VarDataClear(Dest);
  if VarDataIsStr(Source) then
    TWrapperVarData(Dest).Data:=TVarWithUnit.CreateFromText(VarDataToStr(Source))
  else
    TWrapperVarData(Dest).Data:=TVarWithUnit.CreateFromVariant(Variant(source),PhysUnitData.Unity);
  Dest.VType:=VarWithUnitVariantType.VarType;
end;

procedure TVarWithUnitType.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);

  procedure DealWithConversion;
  var tmp: Variant;
  begin
    tmp:=PhysUnitConvert(Variant(source),PhysUnitData.Unity);
    VarDataCastTo(Dest,TVarData(TVarWithUnitVarData(tmp).Data.instance),AVarType);
  end;

begin
  if Source.VType = VarType then begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TWrapperVarData(Source).data.GetAsString);
      varString:
        VarDataFromStr(Dest, TWrapperVarData(Source).data.GetAsString);
      else
        with TVarWithUnitVarData(Source).Data do begin
          if TVarWithUnitVarData(source).Data.ConvType=PhysUnitData.Unity then
            VarDataCastTo(Dest,TVarData(TVarWithUnitVarData(source).Data.instance),AVarType)
          else DealWithConversion;
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
  if not R.ConvType.fIsBase then
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

(*
function TVarWithUnitType.RightPromotion(const V: TVarData;
  const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  RequiredVarType:=V.VType; //пусть остается как есть, Cast от правого операнда работает хреново
  Result:=true;
end;
*)

function TVarWithUnitType.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result:=true;
  if Name='ADD' then
    if Arguments[0].VType = (varVariant or varByRef) then
      TVarWithUnitVarData(V).Data.DoAdd(TVarWithUnitVarData(Arguments[0].VPointer^).data)
    else if Arguments[0].VType = VarType then
      TVarWithUnitVarData(V).Data.DoAdd(TVarWithUnitVarData(Arguments[0]).data)
    else
      Result:=false
  else if Name='ASSIGN' then
    if Arguments[0].VType = (varVariant or VarByRef) then
      TVarWithUnitVarData(V).Data.Assign(TVarWithUnitVarData(Arguments[0].VPointer^).data)
    else if Arguments[0].VType = VarType then
      TVarWithUnitVarData(V).Data.Assign(TVarWithUnitVarData(Arguments[0]).data)
    else
      Result:=false
  else
    Result:=false;
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
  str:=TLocalizedName.Create(nil);
  ShowLocShortName(str);
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

procedure TUnitsWithExponents.ShowLocalized(proc: TShowLocalizedName; dest: TLocalizedName);
var i,j,neg: Integer;
begin
  j:=0; //кол-во уже имеющихся ед.
  neg:=0;
  dest.MakeEmptyNeutral;
  if fCount=0 then Exit;
  for i:=0 to fCount-1 do
    if Exponents[i]>0 then begin
      if j>0 then dest.RightConcat('*');
      inc(j);
      dest.RightConcat(proc(UnitTypes[i]));
      if Exponents[i]<>1 then
        dest.RightConcat('^'+FloatToStr(Exponents[i]));
    end
    else inc(neg);
  if j=0 then dest.RightConcat('1');
  //а теперь отрицательные степени
  j:=0;
  if neg>0 then begin
    if neg=1 then dest.RightConcat('/')
    else dest.RightConcat('/(');
    for i:=0 to fCount-1 do
      if Exponents[i]<0 then begin
        if j>0 then dest.RightConcat('*');
        inc(j);
        dest.RightConcat(proc(UnitTypes[i]));
        if Exponents[i]<>-1 then
          dest.RightConcat('^'+FloatToStr(-Exponents[i]));
      end;
    if neg>1 then dest.RightConcat(')');
  end;
end;

procedure TUnitsWithExponents.SimplifiedShowLocalized(proc: TShowLocalizedName; dest: TLocalizedName);
var i: Integer;
begin
  dest.MakeEmptyNeutral;
  for i:=0 to fCount-1 do begin
    if Exponents[i]=1 then
      dest.RightConcat(proc(UnitTypes[i]))
    else begin
      dest.RightConcat(proc(UnitTypes[i]));
      dest.RightConcat('^');
(*
      if Exponents[i]<0 then
        Result.RightConcat('('+FloatToStr(Exponents[i])+')')
      else
        Result.RightConcat(FloatToStr(Exponents[i]));
        *)
      dest.RightConcat(FloatToStr(Exponents[i]));
    end;
    if i<fCount-1 then dest.RightConcat('*');
  end;
end;

procedure TUnitsWithExponents.ShowLocFormula(dest: TLocalizedName);
begin
  SimplifiedShowLocalized(ConvTypeToLocFamilyLetter,dest);
end;

procedure TUnitsWithExponents.ShowLocShortName(dest: TLocalizedName);
begin
  ShowLocalized(ConvTypeToLocShortName,dest);
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


function TUnitsWithExponents.GetPhysFamily: TPhysFamily;
var i: Integer;
    un: TNormalConvType;
begin
  for i:=0 to PhysUnitData.fFamilyList.Count-1 do begin
    Result:=PhysUnitData.fFamilyList[i] as TPhysFamily;
    if SameFamily(Result.fFormula) then Exit;
  end;
  //не нашли подходящую семью - придется создать!
  Result:=TPhysFamily.Create(PhysUnitData);
  Result.fFormula.Assign(self);
  //утечка памяти
  ShowLocFormula(Result.ShortName);
  Result.Name:=ToAcceptableName(Result.ShortName.InEnglish);
  //и еще подходящую единицу измерения создадим
  //нормальную, других нельзя
  un:=TNormalConvType.Create(Result);
  un.Name:=ToAcceptableName(AsString);
  un.Multiplier:=1;
  //нужно ей и название сформировать, чтоб находить на всех языках
  //или хотя бы на одном
  //shortname или caption - пофиг по большому счету!
  ShowLocShortName(un.ShortName);
  Result.BaseType:=un;
  PhysUnitData.AddToMegalist(un);
end;

function TUnitsWithExponents.GetConvType: TPhysUnit;
var i: Integer;
    fam: TPhysFamily;
    un: TNormalConvType;

(*
    j: Integer;
    solver: IAbstractSLEQ;
    nvars,neq: Integer;
    s: string;
    *)
begin
  for i:=0 to PhysUnitData.fFamilyList.Count-1 do begin
//    inc(numberOfGuesses);
    fam:=PhysUnitData.fFamilyList[i] as TPhysFamily;
    if SameFamily(fam.fFormula) then begin
      Result:=fam.BaseType;
      Exit;
    end;
  end;
(*
  if fcount>2 then begin
    if Assigned(PhysUnitData.infoproc) then
      PhysUnitData.infoproc('creating new family');
    solver:=TSimpleGaussLEQ.Create;
    nvars:=PhysUnitData.fBaseFamilyList.Count;
    neq:=nvars;
    solver.SetDimensions(nvars,neq);
    for i:=0 to nvars-1 do begin
      solver.Matrix[i,i]:=1;
      solver.VariableName[i]:=(PhysUnitData.fBaseFamilyList[i] as TPhysFamily).BaseType.UniqueName;
    end;
    //единичная матрица для основных величин
    for i:=0 to fcount-1 do
      solver.Matrix[nvars,UnitTypes[i].Family.BaseIndex]:=Exponents[i];

    for i:=0 to PhysUnitData.fFamilyList.Count-1 do begin
      fam:=PhysUnitData.fFamilyList[i] as TPhysFamily;
      if fam.IsGoodName then begin
        inc(nvars);
        solver.SetDimensions(nvars,neq);
        for j:=0 to fam.fFormula.fCount-1 do
          solver.Matrix[nvars-1,fam.fFormula.UnitTypes[j].Family.BaseIndex]:=fam.fFormula.Exponents[j];
        if not Assigned(fam.BaseType) then
          raise Exception.Create(fam.fCaption.Caption);
        solver.VariableName[nvars-1]:=fam.BaseType.UniqueName;
      end;
    end;

    solver.Solve;
    if Assigned(PhysUnitData.infoproc) then begin
      for i:=0 to nvars-1 do begin
        s:=solver.GetVariable(i);
        PhysUnitData.infoproc(solver.VariableName[i]+'='+s);
      end;
    end;

  end;
*)

  //не нашли подходящую семью - придется создать!
  fam:=TPhysFamily.Create(PhysUnitData);
  fam.fFormula.Assign(self);
  //утечка памяти
  ShowLocFormula(fam.ShortName);
  fam.Name:=ToAcceptableName(fam.ShortName.InEnglish);

  //и еще подходящую единицу измерения создадим
  //нормальную, других нельзя
  un:=TNormalConvType.Create(fam);
  un.Name:=ToAcceptableName(AsString);
  un.Multiplier:=1;
  //нужно ей и название сформировать, чтоб находить на всех языках
  //или хотя бы на одном
  //shortname или caption - пофиг по большому счету!
  ShowLocShortName(un.ShortName);
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
  inherited;
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
        Raise EPhysUnitError.CreateFmt(FundPhysConstMustBeRealPositive,[V.GetAsString]);
      inc(fActuallyUsedCount);
      fActualIndex[fActuallyUsedCount-1]:=j;
    end;
  fsolver.InvertMatrix;
  Result:=(fsolver.GetStatus<>slNoSolution);
end;

(*
    Variant routines
                        *)
function IsPhysUnit(const V: Variant): Boolean;
begin
  Result:=(TWrapperVarData(V).VType=VarWithUnitVariantType.VarType);
end;

function IsDimensionless(const V: Variant): Boolean;
begin
  //% или ppm тоже подходят
  Result:=(not IsPhysUnit(V)) or (TVarWithUnitVarData(V).Data.ConvType.Family.fFormula.IsUnity);
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
    PhysUnitCreateInto(Res,TVarWithUnit.CreateFromText(text));
    Result:=true;
  except
    on E: Exception do
      Res:=E.Message;
  end;
end;

function PhysUnitCreate(text: string): Variant;
begin
  PhysUnitCreateInto(Result,TVarWithUnit.CreateFromText(text));
end;

function PhysUnitCreate(value: Real; ConvType: string): Variant;
begin
  PhysUnitCreateInto(Result,TVarWithUnit.CreateFromVariant(value,StrToPhysUnit(ConvType)));
end;

function PhysUnitCreateFromVariant(const source: Variant; ConvType: TPhysUnit): Variant;
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

function PhysUnitCreateFromVariant(const source: Variant; ConvType: string): Variant;
begin
  Result:=PhysUnitCreateFromVariant(source,StrToPhysUnit(ConvType));
end;

function PhysUnitConvert(const source: Variant; DestConvType: TPhysUnit; explicit: boolean=false): Variant;
var inst,dest: TVarWithUnit;
begin
  if not IsPhysUnit(source) then begin
    inst:=TVarWithUnit(TVarWithUnit.GetInstance);
    inst.ConvType:=PhysUnitData.Unity;
    inst.instance:=source;
  end
  else
    inst:=TVarWithUnitVarData(source).Data;
  dest:=TVarWithUnit(TVarWithUnit.GetInstance);
  try
    dest.FastAssign(inst);
    dest.Conversion(DestConvType);
    dest.ExplicitConversion:=explicit;
    PhysUnitCreateInto(Result,dest);
  except  //эта хреновина замаскировала ошибку!
    dest.release;
    raise;
  end;
end;

function PhysUnitConvert(const source: Variant; DestType: string): Variant;
begin
  Result:=PhysUnitConvert(source,StrToPhysUnit(DestType),true);
end;

function PhysUnitPower(const source: Variant; pow: Real): Variant;
begin
  Result:=source;
  TVarWithUnitVarData(Result).Data.DoPower(pow);
end;

function PhysUnitSquare(const source: Variant): Variant;
begin
  Result:=source;
  TVarWithUnitVarData(Result).Data.DoSquare;
end;

function PhysUnitCube(const source: Variant): Variant;
var un: TVarWithUnit;
begin
//  Result:=source;
//  TVarWithUnitVarData(Result).Data.DoCube;
  with TVarWithUnitVarData(source).Data do begin
    if ConvType.fIsBase then begin
      un:=TVarWithUnit(TVarWithUnit.getInstance);
      {$ifdef dirtyHack}
        un.instance:=instance;
        CallVarMul(un.instance,instance);
        CallVarMul(un.instance,instance);
      {$else}
        un.instance:=instance*instance*instance;
      {$endif}
//      un.ConvType:=ConvType.family.getMultipliedBy(ConvType.Family).
//                              GetMultipliedBy(ConvType.Family).fBaseType;
      un.ConvType:=ConvType.family.GetCubed.fBaseType;
      PhysUnitCreateInto(Result,un);
    end
    else begin
      Result:=source;
      TVarWithUnitVarData(result).Data.DoCube;
    end
  end;
end;

function PhysUnitSqrt(const source: Variant): Variant;
begin
  Result:=PhysUnitPower(source,0.5);
end;

function PhysUnitPar(const a,b: Variant): Variant;
var v,tmp: Variant;
    ctype: TPhysUnit;
begin

  v:=TVarWithUnitVarData(a).Data.instance;
  if IsPhysUnitSameFamily(a,b) then
    TVarWithUnitVarData(a).Data.ConvType.Par(v,ctype,TVarWithUnitVarData(b).Data.instance,TVarWithUnitVarData(b).Data.ConvType)
  else begin
    tmp:=PhysUnitConvert(b,TVarWithUnitVarData(a).Data.ConvType);
    TVarWithUnitVarData(a).Data.ConvType.Par(v,ctype,TVarWithUnitVarData(tmp).Data.instance,TVarWithUnitVarData(tmp).Data.ConvType);
  end;
  Result:=PhysUnitCreateFromVariant(v,ctype);
end;

function PhysUnitAbs(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if IsPhysUnit(source) then begin
    with TVarWithUnitVarData(source).Data do begin
      un:=TVarWithUnit(TVarWithUnit.GetInstance);
      un.instance:=VarComplexAbs(instance);
      un.ConvType:=ConvType;
      PhysUnitCreateInto(Result,un);
    end;
//    Result:=source;
//    TVarWithUnitVarData(Result).Data.instance:=VarComplexAbs(TVarWithUnitVarData(source).Data.instance)
  end
  else
    Result:=PhysUnitCreateFromVariant(VarComplexAbs(source),PhysUnitData.Unity);
end;

function PhysUnitAbsSquared(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if IsPhysUnit(source) then begin
    with TVarWithUnitVarData(source).data do begin
      if ConvType.fIsBase then begin
        un:=TVarWithUnit(TVarWithUnit.GetInstance);
        un.instance:=VarComplexAbsSqr(instance);
        un.ConvType:=ConvType.family.GetSquared.fBaseType;
        PhysUnitCreateInto(Result,un);
      end
      else begin
        Result:=source;
        TVarWithUnitVarData(Result).Data.DoAbsSquared;
      end;
    end;
  end
  else
    Result:=PhysUnitCreateFromVariant(VarComplexAbsSqr(source),PhysUnitData.Unity);
end;

function PhysUnitArg(const source: Variant): Variant;
begin
  if isPhysUnit(source) then
    Result:=PhysUnitCreateFromVariant(VarComplexAngle(TVarWithUnitVarData(source).Data.instance),PhysUnitData.Radian)
  else
    Result:=PhysUnitCreateFromVariant(VarComplexAngle(source),PhysUnitData.Radian);
end;

function PhysUnitRe(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if isPhysUnit(source) then begin
//    Result:=source;
//    TVarWithUnitVarData(Result).Data.instance:=TVarWithUnitVarData(Result).Data.instance.Real;
    with TVarWithUnitVarData(source).Data do begin
      un:=TVarWithUnit(TVarWithUnit.GetInstance);
      un.instance:=VarComplexRe(instance);
      un.ConvType:=ConvType;
      PhysUnitCreateInto(Result,un);
    end;
  end
  else
    Result:=PhysUnitCreateFromVariant(source.Re,PhysUnitData.Unity);
end;

function PhysUnitIm(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if isPhysUnit(source) then begin
//    Result:=source;
//    TVarWithUnitVarData(Result).Data.instance:=TVarWithUnitVarData(Result).Data.instance.Imaginary;
    with TVarWithUnitVarData(source).Data do begin
      un:=TVarWithUnit(TVarWIthUnit.GetInstance);
      un.instance:=VarComplexIm(instance);
      un.ConvType:=ConvType;
      PhysUnitCreateInto(Result,un);
    end;
  end
  else
    Result:=PhysUnitCreateFromVariant(source.Im,PhysUnitData.Unity);
end;

function PhysUnitConj(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if isPhysUnit(source) then begin
    with TVarWithUnitVarData(source).Data do begin
      un:=TVarWithUnit(TVarWithUnit.GetInstance);
      un.instance:=VarComplexConjugate(instance);
      un.ConvType:=ConvType;
      PhysUnitCreateInto(Result,un);
    end;
  end
  else
    Result:=PhysUnitCreateFromVariant(VarComplexConjugate(source),PhysUnitData.Unity);
end;

function PhysUnitExp(const source: Variant): Variant;
var un: TVarWithUnit;
begin
  if isPhysUnit(source) then begin
    with TVarWithUnitVarData(source).data do begin
      un:=TVarWithUnit(TVarWithUnit.GetInstance);
      un.ConvType:=PhysUnitData.Unity;
      if ConvType=un.ConvType then
        un.instance:=VarComplexExp(instance)
      else
        un.instance:=VarComplexExp(TVarWithUnitVarData(PhysUnitConvert(source,
                                                        un.ConvType)).Data.instance);
      PhysUnitCreateInto(Result,un);
    end;
//    Result:=source;
//    TVarWithUnitVarData(Result).Data.instance:=VarComplexExp(TVarWithUnitVarData(Result).Data.instance);
  end
  else
    Result:=PhysUnitCreateFromVariant(VarComplexExp(source),PhysUnitData.Unity);
end;

function PhysUnitGetNumberIn(const source: Variant; UnitName: TPhysUnit): Variant;
var tmp: Variant;
begin
  tmp:=PhysUnitConvert(source,UnitName);
  Result:=TVarWithUnitVarData(tmp).Data.instance;
end;

function PhysUnitGetNumberIn(const source: Variant; UnitName: string): Variant;
begin
  Result:=PhysUnitGetNumberIn(source,PhysUnitData.strToConvType(UnitName));
end;

function PhysUnitGetNumber(const source: Variant): Variant;
begin
  if IsPhysUnit(source) then
    Result:=TVarWithUnitVarData(source).Data.instance
  else
    Result:=source;
end;

function VarGetLengthSquared(const value: Variant): Real;
begin
  if value=null then Result:=-1
  else if VarIsNumeric(value) then
   result:=Sqr(value)
  else if VarIsComplex(value) then
    result:=VarComplexAbsSqr(value)
  else Result:=value.LengthSquared;
end;

function VarGetLength(source: Variant): Real;
begin
  if VarIsNumeric(source) then result:=abs(source)
  else if VarIsComplex(source) then Result:=VarComplexAbs(source)
  else Raise Exception.Create('can''t get length of variable');
end;

function PhysUnitFindGoodPrefix(const V: Variant): Variant;
var vwithunit: TVarWithUnit;
    L: Real;
begin
  if IsPhysUnit(V) then begin
    vwithunit:=TVarWithUnit.Create;
    vwithunit.FastAssign(TVarWithUnitVarData(V).Data);
    repeat
      if Assigned(vwithunit.ConvType.ScaledUp) and (vwithunit.ConvType.ScaledUp=vwithunit.ConvType.ScaledDown) then
        vwithunit.Conversion(vwithunit.ConvType.ScaledDown);
      L:=VarGetLength(vwithunit.instance);
      if (L<(1-1e-9)) then
        if Assigned(vwithunit.ConvType.ScaledDown) then
          vwithunit.Conversion(vwithunit.ConvType.ScaledDown)
        else begin
          Result:=V;
          break;
        end
      else if (L>=1000) then
        if Assigned(vwithunit.ConvType.ScaledUp) then
          vwithunit.Conversion(vwithunit.ConvType.ScaledUp)
        else begin
          Result:=V;
          break;
        end
      else begin
        PhysUnitCreateInto(Result,vwithunit);
        break;
      end;
    until false;
  end
  else
    Result:=V;
end;

function StrToPhysUnit(str: string): TPhysUnit;
begin
  Result:=PhysUnitData.StrToConvType(str);
end;

function StrToPhysFamily(str: string): TPhysFamily;
var i: Integer;
begin
  for i:=0 to PhysUnitData.fFamilyList.Count-1 do begin
    Result:=PhysUnitData.fFamilyList[i] as TPhysFamily;
    if (Result.Caption.strings.IndexOf(str)>=0) or
      (Result.ShortName.strings.IndexOf(str)>=0) then
        Exit;
  end;
  Raise EPhysUnitError.CreateFMT('StrToPhysFamily: family %s not found',[str]);
end;

function IsPhysUnitSameFamily(Const v1,v2: Variant): Boolean;
var dat1: TVarWithUnitVarData absolute v1;
    dat2: TVarWithUnitVarData absolute v2;
    fam1,fam2: TPhysFamily;
begin
  if not IsPhysUnit(v1) then
    fam1:=PhysUnitData.Unity.Family
  else
    fam1:=dat1.Data.ConvType.Family;
  if not IsPhysUnit(v2) then
    fam2:=PhysUnitData.Unity.Family
  else
    fam2:=dat2.Data.ConvType.Family;
  Result:=(fam1=fam2);
end;

function PhysUnitGetConvType(const source: Variant): TPhysUnit;
begin
  if IsPhysUnit(source) then
    Result:=TVarWithUnitVarData(source).Data.ConvType
  else
    Result:=PhysUnitData.Unity;
end;

procedure FreePhysUnitPool;
var tmp: TVarWithUnit;
begin
  while Assigned(PhysUnitPool) do begin
    tmp:=PhysUnitPool;
    PhysUnitPool:=TVarWithUnit(tmp.ConvType);
    tmp.Free;
  end;
end;

initialization
  RegisterClasses([TAffineConvType,TLogarithmicConvType,TNormalConvType,
  TPhysUnitData,TPhysFamily,TUnitPrefix,TUnitPrefixes,TPhysConst]);

  VarCmplxMk2.ComplexNumberDefuzzAtZero:=false;

  PhysUnitData:=TPhysUnitData.Create(nil);
  InitPhysUnitData;
  VarWithUnitVariantType:=TVarWithUnitType.Create;
finalization
  FreeAndNil(PhysUnitData);
  FreeAndNil(VarWithUnitVariantType);
  FreePhysUnitPool;
end.
