unit phys_units_lib;

//������ ����� ������� ���������, ������ ��������� ������� � �����������,
//�������������� (����� ���) � � ������� ������ (�����������),
//� ����� ���������� (�������� ������ � ��)

//�������� �� ��������� ����� � ConvUtils, ����� ��-����� ������� �� �������
//������ ������.
//�� ����, �������� ����� ����� TConvFamily, �.� ������������ ������ �� ��� �
//"��������" ���� M*L/T^2 (��� ����, ����. ��*�/�^2), � ����� �� ������� ����� �����
//������, �� ���� ������� ����� ��������� (���� �� ���� �����) � �����. ����.

interface

uses classes,VariantWrapper,ConvUtils,sysUtils,linear_eq,streaming_class_lib;

type

  TPreferredUnits=class(TStreamingClass)
  //�������� � ���� ���� (���������;�������������� ��������)
  //������������ � ��������
  private
    fFamily: array of TConvFamily;
    fUnit: array of TConvType;
    procedure WriteData(Writer: TWriter);
    procedure ReadData(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Add(aFamily: TConvFamily; aUnit: TConvType);
    function ConvertToPreferredType(value: Real; aFamily: TConvFamily): string;
    function ConvertVariantToPreferredType(value: Variant; aFamily: TConvFamily): string;
    function GetPreferredUnitName(aFamily: TConvFamily): string;
    function GetPreferredType(aFamily: TConvFamily): TConvType;
end;

  TAbstractSavedConvFamily=class(TStreamingClass)
  private
    flang: string;
    fBaseUnit: string;  //������, TConvType ����� ������������ ���������,
    //������ �� ������ ���������� BaseUnit �� ��� ����� �� ����� ������ �����
    UnitNames: array of string;
    Multipliers: array of Real;
    offsets: array of Real;
    procedure ReadUnits(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property Lang: string write flang;
    property BaseUnit: string write fBaseUnit;
  end;

  TBaseConvFamily=class(TAbstractSavedConvFamily)
  private
    fLetter: string;
    fAffine: boolean;
  public
    procedure Loaded; override;
  published
    property letter: string write fLetter;
    property IsAffine: Boolean write fAffine;
  end;

  TDerivedConvFamily=class(TAbstractSavedConvFamily)
  private
    fFormula: string;
  public
    procedure Loaded; override;
  published
    property formula: string write fFormula;
  end;

TBaseFamilyEntry=record
  ConvFamily: TConvFamily;  //������ �� description ����� ��
  BaseConvType: TConvType;  //� ��� ��-�� ������� ��. ��� ������� 1 ����� � StdConvs
  isAffine: boolean;  //false: ��������� ��������, true: �������
  letter: string;
end;  //������� ������ ������ �� �����?

  //� ������ ���� �� ���������� ����������

  TUnitsWithExponent = class;
  TUnitsWithExponentMergeProc = function (value: TUnitsWithExponent; i,j: Integer) : Real of object;
  TShowName = function (const value: TConvType): string;
  TUnitTypes = array of TConvType;  //�� TConvType ������ ������� TConvFamily
  TExponents = array of Real;

  TUnitsWithExponent = class(TPersistent)  //������� �� � record'� � ���������� � GetMem/FreeMem,
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

      function TakeFromString(formula: string): Real;
      procedure DoPower(Exponent: Real);
      function SameFamily(value: TUnitsWithExponent): Boolean;
      procedure Multiply(value: TUnitsWithExponent);
      procedure Divide(right: TUnitsWithExponent);
      function AsString: string;
      function ShowFormula: string;
      function isAffine: Boolean;
  end;
//���� ��� ����� - ���������� ���, ��� ������������� ����������� ������
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TConvType;
      function IsAffine(out i: Integer): Boolean; //������� �����
    public
      instance: Variant; //�� ����������, ������� �� �����������
      constructor Create(text: string); overload;
      constructor CreateFromVariant(source: Variant; aConvType: TConvType);
      procedure Assign(source: TPersistent); overload; override;
      procedure Assign(str: string); reintroduce; overload;
      procedure Negate; override; //����� �������� ����
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoInverse;
      procedure DoDivide(Right: TAbstractWrapperData); override;
      procedure DoPower(pow: Real);
      function AsString: string; override;
      procedure Conversion(DestConv: TConvType);
  end;

  TDerivedFamilyEntry=class
  public
    ConvFamily: TConvFamily;  //���� ����� �����!
    BaseConvType: TConvType;  //���, � ���������� ConvUtils ������ �� Family ��������� �� ������� ��. ���������
    formula: TUnitsWithExponent;
    constructor Create;
    destructor Destroy; override;
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

  TFundamentalPhysConstants = class
    private
      fNames: array of string;
      fDescriptions: array of string;
      fValues: array of Variant;
      fenabled: array of Boolean;
      fcount: Integer;
      fsolver: IAbstractSLEQ;
      fActuallyUsedCount: Integer;
      fActualIndex: array of Integer;
      function getName(i: Integer): string;
      function getDescription(i: Integer): string;
      function getEnabled(i: Integer): boolean;
      procedure setEnabled(i: Integer; avalue: boolean);
      function Recalculate: Boolean;
    public
      constructor Create;
      procedure Add(aname,adescr: string; aValue: Variant; aEnabled: boolean=false);
      function GetVar(i: Integer): Variant;
      function GetActualIndex(i: Integer): Integer;
      property Count: Integer read fCount;
      property Names[i: Integer]: string read getName;
      property Descriptions[i: Integer]: string read getDescription;
      property Enabled[i: Integer]: Boolean read getEnabled write setEnabled;
      property Solver: IAbstractSLEQ read fsolver;
      property ActuallyUsedCount: Integer read fActuallyUsedCount;
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

  TLogConversionDetailsProc = procedure (line: string); //���� ���� ���������

procedure RegisterBaseConversionFamily(Family: TConvFamily; BaseType: TConvType; letter: string; isAffine: boolean=false);
//��� ���� TConvFamily (��������, �� StdConvs), ����� ������� �� � ��� ������
//procedure RegisterBaseConversionFamily(
procedure RegisterDerivedConversionFamily(Family: TConvFamily; BaseType: TConvType; formula: string); overload;
//���� ������� ��� ����������� �������, formula - ���-�� ����� 'M*L/T^2'
function RegisterDerivedConversionFamily(formula: TUnitsWithExponent): TDerivedFamilyEntry; overload;

function ConvTypeToBaseFamilyLetter(const value: TConvType): string;

//��� VariantWithUnit
function VariantWithUnit: TVarType;

//������������ � ��.
procedure VarWithUnitCreateInto(var ADest: Variant; const AData: TVariantWithUnit);
function VarWithUnitCreate(text: string): Variant;
function VarWithUnitCreateFromVariant(source: Variant; ConvType: TConvType): Variant;
function IsVarWithUnit(V: Variant): Boolean;
function IsDimensionless(V: Variant): boolean;
function TryVarWithUnitCreate(text: string; out Res: Variant): boolean;
function VarWithUnitConvert(source: Variant; DestConvType: TConvType): Variant; overload;
function VarWithUnitConvert(source: Variant; UnitName: string): Variant; overload;
function StrToConvType(str: string): TConvType;
function PrefixDescrToConvType(str: string; out CType: TConvType): boolean;
function VarWithUnitPower(source: Variant; pow: Real): Variant;

var UnityPhysConstants: TFundamentalPhysConstants;
    LogConversionDetailsProc: TLogConversionDetailsProc;
    cbDimensionless,cbAngle: TConvFamily;
    duUnity,auDMS,auRadian: TConvType;
    PreferredUnits: TPreferredUnits;
implementation

uses StdConvs,math,simple_parser_lib,VarCmplx,strUtils,variants,set_english_locale_if_not_sure;

var BaseFamilyEntries: array of TBaseFamilyEntry;
    DerivedFamilyEntries: array of TDerivedFamilyEntry;
    VarWithUnitType: TVariantWithUnitType;
    UnitMultiplier: TUnitMultipliersArray;
    AffineUnits: array of TAffineUnitEntry;
    default_dir: string;

procedure RegisterBaseConversionFamily(Family: TConvFamily; BaseType: TConvType; letter: string; isAffine: boolean=false);
var L,i: Integer;
  Types: TConvTypeArray;
begin
  L:=Length(BaseFamilyEntries);
  SetLength(BaseFamilyEntries,L+1);
  BaseFamilyEntries[L].ConvFamily:=Family;
  BaseFamilyEntries[L].BaseConvType:=BaseType;
  BaseFamilyEntries[L].letter:=letter;
  BaseFamilyEntries[L].isAffine:=isAffine;
  if isAffine then begin
    GetConvTypes(Family,Types);
    L:=Length(AffineUnits);
    SetLength(AffineUnits,L+Length(Types));
    for i:=0 to Length(Types)-1 do begin
      AffineUnits[L+i].ConvType:=Types[i];
      AffineUnits[L+i].BaseConvType:=Types[i];
      AffineUnits[L+i].multiplier:=1;
    end;
  end;
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

function PrefixDescrToConvType(str: string; out CType: TConvType): boolean;
begin
  Result:=(LeftStr(str,2)='��') and DescriptionToConvType(RightStr(str,Length(str)-2),CType);
  if not Result then
    Result:=(UnitMultiplier[Integer(str[1])]<>0) and DescriptionToConvType(Rightstr(str,Length(str)-1),CType);
    if not Result then
      Result:=DescriptionToConvType(str,CType);
end;

function FindPhysUnit(ConvType: TConvType): TUnitsWithExponent;
var i: Integer;
    ConvFamily: TConvFamily;
begin
  Result:=TUnitsWithExponent.Create;
  ConvFamily:=ConvTypeToFamily(ConvType);
  for i:=0 to Length(BaseFamilyEntries)-1 do
    if BaseFamilyEntries[i].ConvFamily=ConvFamily then begin
//      Result.AddBaseUnit(ConvType,1); //� ������ ConvType, � �� BaseConvType?
      Result.AddBaseUnit(BaseFamilyEntries[i].BaseConvType,1);
      Exit;
    end;
  //����� ������� �� �����, ������ � �����������
  for i:=0 to Length(DerivedFamilyEntries)-1 do
    if DerivedFamilyEntries[i].ConvFamily=ConvFamily then begin
      Result.Assign(DerivedFamilyEntries[i].formula);
      Exit;
    end;
  Raise EPhysUnitError.CreateFMT('Couldn''t find unit %s among base or derived',[ConvTypeToDescription(ConvType)]);
end;

function FormulaToConvType(formula: TUnitsWithExponent): TConvType;
var i: Integer;
begin
  if (formula.fCount=1) and (formula.Exponents[0]=1) then begin  //������� ��������
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
  //���� �� ������� ����. ��������� - �� �������� ����� ��������� � ����� ����!
  Result:=RegisterDerivedConversionFamily(formula).BaseConvType;
end;

function StrToConvType(str: string): TConvType;
var f: TUnitsWithExponent;
    multiplier: Real;
    BaseConvType: TConvType;
begin
  if not DescriptionToConvType(str,Result) then begin
    f:=TUnitsWithExponent.Create;
    try
      multiplier:=f.TakeFromString(str);
      BaseConvType:=FormulaToConvType(f); //��� ����� ����� ������� �� ���� ����� �������
      if not DescriptionToConvType(str,Result) then begin
        multiplier:=multiplier*ConvertFrom(BaseConvType,1);
        Result:=RegisterConversionType(ConvTypeToFamily(BaseConvType),str,multiplier);
      end;
    finally
      f.Free;
    end;
  end;
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
    //������� �� derived, ������� � ��������� �� ��� unit
    i:=IndexOfDerivedFamily(f);
    if i=-1 then Raise EPhysUnitError.CreateFmt('Family %s is neither base nor derived',[ConvFamilyToDescription(f)]);
    u:=TUnitsWithExponent.Create;
    try
      u.Assign(DerivedFamilyEntries[i].formula);
      u.DoPower(Exponent); //�������� � ������ ������� (����� ���� �������)
      Multiply(u);  //� ���� �������� �� ������ ���������� ����� ���������
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
    isDivide: Boolean;
    nextDivide: Boolean;
    mul: Real;
begin
  Clear;
  p:=TSimpleParser.Create(formula);
  Result:=1;
  isDivide:=false;
  nextDivide:=false;
  try
    while not p.eof do begin
      term:=p.getPhysUnitIdent;
      mul:=1;
      if not DescriptionToConvType(term,convType) then
        if DescriptionToConvType(RightStr(term,Length(term)-2),ConvType) and (LeftStr(term,2)='��') then
          Mul:=1e-6
        else
          if DescriptionToConvType(RightStr(term,Length(term)-1),ConvType) then begin
            Mul:=UnitMultiplier[Integer(term[1])];
            if Mul=0 then Raise EPhysUnitError.CreateFmt('%s is not correct multiplier',[term[1]])
          end
          else
            Raise EPhysUnitError.CreateFmt('%s is not correct unit',[term]);
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

//      ����� �� � ������� ������ ������. � ��*�^2/�^2
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

procedure TUnitsWithExponent.Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
var i,j,k: Integer;
    L1,L2: Integer;
    ResultUnits: TUnitTypes;
    ResultExponents: TExponents;
    first,second: TConvFamily;
begin
  //��� ������ ������������� �� ���������� unitTypes
  //������, TConvFamily �����. unitTypes
  //�� ����, ������������ �������
  i:=0;
  j:=0;
  k:=0;
  L1:=fcount;
  L2:=value.fCount;
  SetLength(ResultUnits,L1+L2); //��������� ��������
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
      //UnitTypes[j] �� ���� � ���. ������ - ���� �������� �� ����. �����
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
  while i<L1 do begin //������ ����������
    ResultUnits[k]:=UnitTypes[i];
    ResultExponents[k]:=Exponents[i];
    inc(i);
    inc(k);
  end;
  while j<L2 do begin //� ��� ���� �����
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
    Result:=Result or BaseFamilyEntries[j].isAffine;
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
    instance:=s.instance; //variant'� - ��� �����, �����������
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
  //��� ���, �������� �������
  SetLength(AffineUnits,L+1);
  str:=ConvTypeToDescription(BaseConvType);
  if mul<>0 then
    str:=Format('%s{%g}',[str,mul])
  else
    str:=str+'{dif}';
  Result:=RegisterConversionType(ConvTypeToFamily(BaseConvType),str,1.0/0.0);
  //������� �� �� ������ ������������ ConversionFactor'�� ������������ �������!
  AffineUnits[L].ConvType:=Result;
  AffineUnits[L].BaseConvType:=BaseConvType;
  AffineUnits[L].multiplier:=mul;
end;

procedure TVariantWithUnit.Conversion(DestConv: TConvType);
var j,i,b: Integer;              //inst - ��� ��
    offset,k,mul: Real;     //dest - ��� ���� ��, �� �����
    formula: TUnitsWithExponent;
    new_formula: TUnitsWithExponent;
    addition: Variant;
    pow: Real;
    buConvType: TConvType;
    s: string;
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
      Conversion(new_formula.UnitTypes[0]);  //����� ������, � ��������
      new_formula.Free;
    end;
  end;
  if (ConvType<>DestConv) then begin
    if CompatibleConversionTypes(ConvType,DestConv) then begin
      instance:=instance*Convert(1,ConvType,DestConv);
      ConvType:=DestConv;
    end
    else begin
    //��������� ��������, ����, ��� c=1, h=1 � �.�.
      buConvType:=ConvType;
      new_formula:=nil;
      formula:=FindPhysUnit(ConvType);  //��� ���� ������
      try
        Conversion(FormulaToConvType(formula)); //��������� �������������� �������� � ��
        try
          new_formula:=FindPhysUnit(DestConv);
          formula.Divide(new_formula);
          addition:=0.0;
          for i:=0 to formula.fCount-1 do begin
            j:=IndexOfBaseFamily(ConvTypeToFamily(formula.UnitTypes[i]));
            addition:=addition+UnityPhysConstants.GetVar(j)*formula.Exponents[i];
          end;
          if VarManySolutionsIsNumber(addition) then begin  //�����!
            instance:=instance*Exp(-addition);
            ConvType:=FormulaToConvType(new_formula);
            Conversion(DestConv); //������ ��������� ��� ����!
            if Assigned(LogConversionDetailsProc) then begin
//������������ ������ - �� ��� �� ��������� � ��������
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
              LogConversionDetailsProc(Format('��� �������������� �� [%s] � [%s] ��������� ��������� �� %s',[ConvTypeToDescription(buConvType),ConvTypeToDescription(DestConv),s]));
            end;
          end
          else Raise EphysUnitError.CreateFmt('������������ ���������� �����: [%s] � [%s]',[ConvTypeToDescription(buConvType),ConvTypeToDescription(DestConv)]);
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
  tmp:=instance; //���-���� ������� ������- �� ����� �������� ����, ���
  instance:=-tmp; //instance:=-instance ����� ������
  if IsAffine(i) then
    ConvType:=GetAffineWithMultiplier(AffineUnits[i].BaseConvType,-AffineUnits[i].multiplier);
end;

function TVariantWithUnit.IsAffine (out i: Integer): Boolean;
var findex: Integer;
begin
  findex:=indexOfBaseFamily(ConvTypeToFamily(convType));
  Result:=(findex>=0) and BaseFamilyEntries[findex].isAffine;
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
    //'���� � ���� �������, ��� ����. � �������� ����. ����������� ����� �����,
    //�� ��� ������ ���� � isAffine-��� �������� ����, � �� �����!
    //K � gradC - �������, � Kdif � Cdif - ���!
      if IsAffine(j) and v.IsAffine(k) then begin
        //������ ������ ����� � ��� ����� �����
          offset:=Convert(0,AffineUnits[k].BaseConvType,AffineUnits[j].BaseConvType);
          mul:=Convert(1,AffineUnits[k].BaseConvType,AffineUnits[j].BaseConvType)-offset;
          instance:=instance+mul*v.instance+offset*AffineUnits[k].multiplier;
          mul:=AffineUnits[j].multiplier+AffineUnits[k].multiplier; //��������. ���������
          ConvType:=GetAffineWithMultiplier(AffineUnits[j].BaseConvType,mul);
      end
      else
        instance:=instance+v.instance*Convert(1,v.ConvType,ConvType)
    else begin
      v.Conversion(ConvType);
      instance:=instance+v.instance;
    end
  else inherited; //�� ���� ����, ���������� �� ��� ������� ���� ���?
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
          else begin //��������� ��� ������� ������� (��� ����� ��������� ��������� - ��� �� �������)
          //����������� � ��������, � ������, � ������� �������� ���������
            Conversion(UL.UnitTypes[0]);
            //� �������
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
            else begin  //��� ���� "����������" ������� �������
              v.Conversion(UR.UnitTypes[0]);
             //�� ������ ConvType ����� ���������� �� K{0}, � �� K
             //� ������� ����� ������ K
              v.IsAffine(k);
              //� �������
              UL.Multiply(Ur);
              instance:=instance*v.instance*Convert(1,AffineUnits[k].BaseConvType,FormulaToConvType(Ur));
              ConvType:=FormulaToConvType(UL);
            end
          else begin
            //�������� ���������
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
      //���������� ��� ������� - ������. � �������� (������, � ���. �������� ���������)
      Conversion(U.UnitTypes[0]);
      instance:=1/instance;
      U.DoPower(-1);
      ConvType:=FormulaToConvType(U);
    end
    else begin
      //������. � ��� ���, ������� ��� ������� �� �������
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
//    instance:=instance*Convert(1,ConvType,bt);  //������� � ������� ���
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
    PrefConvType: TConvType;
    val: Extended;
begin
  for i:=1 to Length(str) do
    if (str[i]='.') or (str[i]=',') then str[i]:=DecimalSeparator;
  i:=Length(str);
  while (i>=1) and (str[i]<>' ') do dec(i);
  if i=0 then begin
    //���� ������ ��. ���������, ��� �����,
    //���� ��� ��� ���� Variant, �� �� ������� �� �����, �����.
    //� ���� �������� ���� ���� ����������. ��������� � ����������� ���. ������������� ��� ��
    instance:=VarComplexCreate(str);
    instance:=VarComplexSimplify(instance);
    ConvType:=duUnity;  //�������.
  end
  else begin
    //����������� ������
    //���� ����� ������ �� �������-��. ���., ���� �������� ����������� ������ � ����. �����
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
        //������� ���������, ����� ������� ��������� TUnitsWithExponent,
        //�������� ��� �����������, �� "��������" ���������, ��������� �� ����
        dimension:=TUnitsWithExponent.Create;
        try
          instance:=instance*dimension.TakeFromString(unitStr);
          //� ����� ConvType - ���, ��� �������� ������� ��� ������� ���������
          //��������, ��������� �������� ������� �� ����
          ConvType:=FormulaToConvType(dimension);
        finally
          dimension.Free;
        end;
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
end;

function TVariantWithUnit.AsString: string;
var deg,min: Variant;
    s: string;
    d: extended;
begin
  if ConvType=auDMS then begin
    instance:=instance+1/7200000;
    deg:=Floor(instance);
    s:=deg;
    Result:=s+'�';
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
    if s<>'' then Result:=Result+' '+s;
  end;
end;
(*
    TVariantWithUnitType
                            *)
procedure TVariantWithUnitType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  //������ ����������� �� ���� ��������, � ����� ������ Variant'� "�����������" �������.
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
  else inherited; //��� ���� ������ variant ������ �����
end;

(*
    TFundamentalPhysConstants
                                *)
constructor TFundamentalPhysConstants.Create;
begin
  inherited Create;
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

procedure TFundamentalPhysConstants.setEnabled(i: Integer; aValue: Boolean);
begin
  if aValue<>fEnabled[i] then begin
    fEnabled[i]:=aValue;
    if not Recalculate then begin
      fEnabled[i]:=not aValue;
      Raise EPhysUnitError.CreateFMT('���������� ������� %s=1 �������� � ������������',[fNames[i]]);
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
      Raise EPhysUnitError.CreateFmt('���������� ������� %s=1 �������� � ������������',[aName]);
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
  BaseUnitsCount:=Length(BaseFamilyEntries);
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


(*
    Initialization
                        *)
procedure PopulateUnitMultiplier;
var u: PUnitMultipliersArray;
begin
  u:=@UnitMultiplier;
  u[Integer('d')]:=1e-1;
  u[Integer('�')]:=1e-1;
  u[Integer('c')]:=1e-2;
  u[Integer('�')]:=1e-2;
  u[Integer('m')]:=1e-3;
  u[Integer('�')]:=1e-3;
  u[Integer('u')]:=1e-6;
  u[Integer('n')]:=1e-9;
  u[Integer('�')]:=1e-9;
  u[Integer('p')]:=1e-12;
  u[Integer('�')]:=1e-12;
  u[Integer('f')]:=1e-15;
  u[Integer('�')]:=1e-15;
  u[Integer('k')]:=1000;
  u[Integer('�')]:=1000;
  u[Integer('M')]:=1e6;
  u[Integer('�')]:=1e6;
  u[Integer('G')]:=1e9;
  u[Integer('�')]:=1e9;
  u[Integer('T')]:=1e12;
  u[Integer('�')]:=1e12;
end;

resourcestring
  LightSpeedDescr = '�������� ����� � ������� (3e8 �/�)';
  PlanckDescr = '���������� ������ (�������������, 1.054e-34 ���*�)';
  GravityConstantDescr = '�������������� ���������� (6.67e-11 �^3*�^-2*��^-1)';
  BoltzmanConstantDescr = '���������� ��������� (1.38e-23 ��/�)';
  FreeSpaceDielectricConstDescr = '��������������� ���������� (8.85e-12 �/�)';
  ElementaryChargeDescr = '������������ ������������� ����� (1.6 ��)';
  RadianDescr = '�������� ������ ������������ ���������';
  pcsDescr = '�������� ''��'' ������������ ���������';
procedure RegisterUnityConstants;
var eps0: Real;
    streps0: String;
begin
  UnityPhysConstants:=TFundamentalPhysConstants.Create;
(*
  with UnityPhysConstants do begin
    Add('rad',RadianDescr,VarWithUnitCreate('1 rad'),true);
    Add('��',pcsDescr,VarWithUnitCreate('1 pcs'),true);
    Add('c',LightSpeedDescr,VarWithUnitCreate('2,99792458e8 m/s'));
    Add('h',PlanckDescr,VarWithUnitCreate('1,054571628e-34 J*s'));
    Add('G',GravityConstantDescr,VarWithUnitCreate('6,67428e-11 m^3*s^-2*kg^-1'));
    Add('k',BoltzmanConstantDescr,VarWithUnitCreate('1,3806488e-23 J/K'));
    eps0:=1/299792458/299792458*1e7;
    streps0:=FloatToStr(eps0);
    Add('4pi*Epsilon0',FreeSpaceDielectricConstDescr,VarWithUnitCreate(streps0+' F/m'));
    Add('e',ElementaryChargeDescr,VarWithUnitCreate('1,602176565e-19 C'));
  end;
  *)
end;

procedure FreeUnityConstants;
begin
  FreeAndNil(UnityPhysConstants);
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
    ������������
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
begin
  if IsVarWithUnit(source) then
    if TVariantWithUnitVarData(source).Data.ConvType=duUnity then begin
      Result:=source;
      TVariantWithUnitVarData(Result).Data.ConvType:=ConvType;
    end
    else
      Raise EPhysUnitError.CreateFmt('%s already has dimension',[source])
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
  except  //��� ��������� ������������� ������!
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
//  strStream: TStringStream;
  fileStream: TFileStream;
  BinStream: TMemoryStream;
  sr: TSearchRec;
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
        comp.Free;
      end;
      BinStream.Free;
      FileStream.Free;
    until FindNext(sr)<>0
  end;
end;

(*
    TPreferredUnits
                        *)
procedure TPreferredUnits.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',ReadData,WriteData,Length(fFamily)>0);
end;

procedure TPreferredUnits.WriteData(Writer: TWriter);
var i: Integer;
    sFamily,sUnit: string;
begin
  Writer.WriteListBegin;
  for i:=0 to Length(fFamily)-1 do
    if FamilyToName(fFamily[i],sFamily) and ConvToName(fUnit[i],sUnit) then begin
      Writer.WriteString(sFamily);
      Writer.WriteString(sUnit);
    end;
  Writer.WriteListEnd;
end;

procedure TPreferredUnits.Add(aFamily: TConvFamily; aUnit: TConvType);
begin
  SetLength(fFamily,Length(fFamily)+1);
  fFamily[Length(fFamily)-1]:=aFamily;
  SetLength(fUnit,Length(fUnit)+1);
  fUnit[Length(fUnit)-1]:=aUnit;
end;

procedure TPreferredUnits.ReadData(Reader: TReader);
var sFamily,sUnit: string;
    aFamily: TConvFamily;
    aFamInt: Integer;
    aUnit: TConvType;
begin

  Reader.ReadListBegin;
  While not Reader.EndOfList do begin
    sFamily:=Reader.ReadString;
    sUnit:=Reader.ReadString;
    if NameToFamily(sFamily,aFamInt) then begin
      aFamily:=aFamInt;
      if DescriptionToConvType(aFamily,sUnit,aUnit) then
        Add(aFamily,aUnit);
    end;
  end;
  Reader.ReadListEnd;

end;

function TPreferredUnits.ConvertToPreferredType(value: Real; aFamily: TConvFamily): string;
var i: Integer;
    uValue: Real;
    fname: string;
begin
  for i:=0 to Length(fFamily)-1 do
    if aFamily=fFamily[i] then begin
      uValue:=ConvertTo(value,fUnit[i]);
      Result:=ConvUnitToStr(uValue,fUnit[i]);
      Exit;
    end;
  FamilyToName(aFamily,fname);
  Raise Exception.CreateFMT('PreferredUnits.ConvertToPrefferedType: family %s not registered',[fname]);
end;

function TPreferredUnits.ConvertVariantToPreferredType(value: Variant; aFamily: TConvFamily): string;
var i: Integer;
    uReal,uImg: Real;
    fname: string;
begin
  value:=VarComplexSimplify(value);
  if VarIsNumeric(value) then Result:=ConvertToPreferredType(value,aFamily)
  else begin
//� ���� ���� ����������� ����� ���� �����������, ����� �����. ����. �����
//����� ����, �������� �������� ���. ������� ���� dbV
    for i:=0 to Length(fFamily)-1 do
      if aFamily=fFamily[i] then
        if VarIsComplex(value) then begin
          uReal:=ConvertTo(value.Real,fUnit[i]);
          uImg:=ConvertTo(value.Imaginary,fUnit[i]);
          Result:=Format('%s %s',[VarComplexCreate(uReal,uImg), ConvTypeToDescription(fUnit[i])]);
          Exit;
        end
        else
          Raise Exception.CreateFMT('ConvertVariantToPreferredType: variable "%s" of unsupported variant type',[value]);
  FamilyToName(aFamily,fname);
  Raise Exception.CreateFMT('PreferredUnits.ConvertToPrefferedType: family %s not registered',[fname]);
  end;
end;

function TPreferredUnits.getPreferredUnitName(aFamily: TConvFamily): string;
var i: Integer;
begin
  for i:=0 to Length(fFamily)-1 do
    if aFamily=fFamily[i] then begin
      Result:=ConvTypeToDescription(fUnit[i]);
      Exit;
    end;
end;

function TPreferredUnits.GetPreferredType(aFamily: TConvFamily): TConvType;
var i: Integer;
begin
  for i:=0 to Length(fFamily)-1 do
    if aFamily=fFamily[i] then begin
      Result:=fUnit[i];
      Exit;
    end;
  Result:=CIllegalConvType;
end;

(*
      TBaseConvFamily
                          *)
procedure TAbstractSavedConvFamily.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Units',ReadUnits,nil,true);
end;

procedure TAbstractSavedConvFamily.ReadUnits(Reader: TReader);
var i,L: Integer;
    s: string;
    p: TSimpleParser;
begin
  Reader.ReadListBegin;
  i:=0;
  p:=TSimpleParser.Create;
  while not Reader.EndOfList do begin
    inc(i);
    if Length(UnitNames)<i then begin
      L:=i*2;
      SetLength(UnitNames,L);
      SetLength(Multipliers,L);
      SetLength(offsets,L);
    end;
    s:=Reader.ReadString;
    p.AssignString(s);
    UnitNames[i-1]:=p.getString;
    Multipliers[i-1]:=p.getFloat;
    if not p.eof then offsets[i-1]:=p.getFloat;
  end;
  SetLength(UnitNames,i);
  SetLength(Multipliers,i);
  SetLength(offsets,i);
  p.Free;
  Reader.ReadListEnd;
end;

procedure TBaseConvFamily.Loaded;
var baseType: TConvType;
    family: TConvFamily;
    i: Integer;
begin
  if not DescriptionToConvFamily(name,family) then
    family:=RegisterConversionFamily(name);
  for i:=0 to Length(UnitNames)-1 do
    if fAffine then
      RegisterConversionType(TconvTypeAffine.Create(family,UnitNames[i],offsets[i],multipliers[i]),baseType)
    else
      RegisterConversionType(family,UnitNames[i],multipliers[i]);

  if Uppercase(GetDefaultLanguageInEnglish)=Uppercase(flang) then
    if DescriptionToConvType(fbaseUnit,baseType) then
      RegisterBaseConversionFamily(family,baseType,fletter,fAffine)
    else Raise Exception.CreateFmt('Couldn''t find BaseConvFamily''s baseType %s',[fBaseUnit]);
end;

procedure TDerivedConvFamily.Loaded;
var family: TConvFamily;
    baseType: TConvType;
    i: Integer;
begin
  if not DescriptionToConvFamily(name,family) then
    family:=RegisterConversionFamily(name);
  for i:=0 to Length(UnitNames)-1 do
    RegisterConversionType(family,UnitNames[i],multipliers[i]);

  if (Uppercase(GetDefaultLanguageInEnglish)=Uppercase(flang)) or (UpperCase(flang)='ANY') then
    if DescriptionToConvType(fbaseUnit,baseType) then
      RegisterDerivedConversionFamily(family,baseType,fformula)
    else Raise Exception.CreateFmt('Couldn''t find DerivedConvFamily''s baseType %s',[fBaseUnit]);
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

initialization
  //�����, �����, ���� � ��.
  PopulateUnitMultiplier;
  //����� ��� Variant'�
  VarWithUnitType:=TVariantWithUnitType.Create;

  //�������� ����, �� ����� ������ � ����
  cbDimensionless:=RegisterConversionFamily('Dimensionless');
  duUnity:=RegisterConversionType(cbDimensionless,'',1);
  cbAngle:=RegisterConversionFamily('Angle');
  auDMS:=RegisterConversionType(cbAngle,'dms',pi/180);
  auRadian:=RegisterConversionType(cbAngle,'rad',1);

  RegisterDerivedConversionFamily(cbDimensionless,duUnity,'');

  RegisterClasses([TBaseConvFamily,TDerivedConvFamily]);
  default_dir:=GetCurrentDir+'\data\PhysUnits\';
  NewConvFamilies;

  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);
  RegisterClass(TPreferredUnits);
  if FileExists('PreferredUnits.txt') then
    PreferredUnits:=TPreferredUnits.LoadFromFile('PreferredUnits.txt');
  GConvUnitToStrFmt:='%g %s';


  RegisterUnityConstants;


finalization
  FreeUnityConstants;
  FreeAndNil(VarWithUnitType);
  FreeUnits;

  FreeAndNil(PreferredUnits);


end.
