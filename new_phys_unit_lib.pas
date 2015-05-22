unit new_phys_unit_lib;

interface
uses set_english_locale_if_not_sure,streaming_class_lib,classes,
  command_class_lib,iterator_lib,variantWrapper,Contnrs,sysUtils;

type
  TAbstractStreamableConvType =class;
  TUnitsWithExponents = class;  
  TStreamableConvFamily=class(TStreamingClass) //можно будет и к TComponent вернуться
    private
      fIsBase: Boolean;
      fCaption,fShortName,fDescription: TLocalizedName;
      fBaseType: TAbstractStreamableConvType;
      fFormula: TUnitsWithExponents;
      fstrformula: string;
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
      property BaseType: TAbstractStreamableConvType read fBaseType write fBaseType;
      property formula: string read fstrformula write fstrformula;
  end;

  TAbstractStreamableConvType=class(TStreamingClass)
    private
      fShortName,fCaption: TLocalizedName;
      fScaledUp,fScaledDown: TAbstractStreamableConvType;
      fPrefixOK: boolean;
      fIsScaled: boolean;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      function CreateScaled(mult: Real): TAbstractStreamableConvType; virtual; abstract;
      function CreateAndConnectScaled(mult: Real): TAbstractStreamableConvType;
      function ConvertToBase(value: Variant): Variant; virtual; abstract;
      function ConvertFromBase(value: Variant): Variant; virtual; abstract;
      function Family: TStreamableConvFamily;
      procedure Add(V1,V2: Variant; t2: TAbstractStreamableConvType;
        out value: Variant; out t3: TAbstractStreamableConvType); virtual;
    published
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Caption: TLocalizedName read fCaption write fCaption;
      property PrefixOk: Boolean read fPrefixOk write fPrefixOk default false;
      property IsScaled: Boolean read fIsScaled write fIsScaled default false;
      property ScaledUp: TAbstractStreamableConvType read fScaledUp write fScaledUp;
      property ScaledDown: TAbstractStreamableConvType read fScaledDown write fScaledDown;
    end;

  TNormalConvType=class(TAbstractStreamableConvType)
    private
      fMultiplier: Real;
    public
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function CreateScaled(mult: Real): TAbstractStreamableConvType; override;
      procedure Add(V1,V2: Variant; t2: TAbstractStreamableConvType;
        out value: Variant; out t3: TAbstractStreamableConvType); override;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
    end;

  TAffineConvType=class(TAbstractStreamableConvType)
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
      function CreateScaled(mult: Real): TAbstractStreamableConvType; override;
      procedure Add(V1,V2: Variant; t2: TAbstractStreamableConvType;
        out value: Variant; out t3: TAbstractStreamableConvType); override;
      function GetAffineUnit(Factor: Real): TAffineConvType;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
      property Offset: Real read fOffset write fOffset;
      property Factor: Real read fFactor write fFactor;
      property BaseAffine: TAffineConvType read fBaseAffine write SetBaseAffine;
    end;

  TLogarithmicConvType=class(TAbstractStreamableConvType)
    private
      fLog10Mult: Real;
      fZeroValue: Real;
    public
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function CreateScaled(mult: Real): TAbstractStreamableConvType; override;
      procedure Add(V1,V2: Variant; t2: TAbstractStreamableConvType;
        out value: Variant; out t3: TAbstractStreamableConvType); override;
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
      destructor Destroy; override;
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

  TPhysUnitData = class(TAbstractDocument)
  private
    fConvUnitsIterator: TAbstractDocumentClassIterator;
    fFamilyList: TObjectList;
    fMegaList: TStringList;
    fWarningList: TStringList;
    fUnity,fDMS: TAbstractStreamableConvType;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    warningproc: TPhysUnitMessagesProc;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function StrToConvType(str: string): TAbstractStreamableConvType;
    property Unity: TAbstractStreamableConvType read fUnity;
    property DMS: TAbstractStreamableConvType read fDMS;
  published
    UnitPrefixes: TUnitPrefixes;
//    FundamentalPhysConstants: TFundamentalPhysConstants;
  end;


  TUnitsWithExponentMergeProc = function (value: TUnitsWithExponents; i,j: Integer) : Real of object;
  TShowName = function(ConvType: TAbstractStreamableConvType): string;
  TExponents = array of Real;
  TUnitTypes = array of TAbstractStreamableConvType;

  TUnitsWithExponents = class(TComponent)
    private
      UnitTypes: TUnitTypes;
      Exponents: TExponents;
      fCount: Integer;
      procedure Merge(value: TUnitsWithExponents; proc: TUnitsWithExponentMergeProc);
      function MergeMul(value: TUnitsWithExponents; i,j: Integer): Real;
      function MergeDiv(value: TUnitsWithExponents; i,j: Integer): Real;
      function ShowSomething(proc: TShowName): string;
      procedure WriteData(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
      function AddArbitraryUnit(ConvType: TAbstractStreamableConvType; Exponent: Real): Real;
      function PhysData: TPhysUnitData;
    public
      constructor Create(aOwner: TComponent); override;
      procedure Clear;
      procedure Assign(source: TPersistent); override;
      procedure SetBaseType(value: TAbstractStreamableConvType);
      function TakeFromString(formula: string): Real;
      procedure DoPower(Exponent: Real);
      function SameFamily(value: TUnitsWithExponents): Boolean;
      procedure Multiply(value: TUnitsWithExponents);
      procedure Divide(right: TUnitsWithExponents);
      function AsString: string;
      function ShowFormula: string;
      function GetConvType: TAbstractStreamableConvType;
  end;


  TVarWithUnit=class(TAbstractWrapperData)
    private
      ConvType: TAbstractStreamableConvType;
    public
      instance: Variant; //та переменная, которую мы оборачиваем
      ExplicitConversion: boolean;  //флаг, что величину "насильно" привели к данному виду
      constructor Create(text: string); overload;
      constructor CreateFromVariant(source: Variant; aConvType: TAbstractStreamableConvType);
      procedure Assign(source: TPersistent); overload; override;
      procedure Assign(str: string); reintroduce; overload;
      procedure Negate; override; //взять обратный знак
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoInverse;
      procedure DoDivide(Right: TAbstractWrapperData); override;
      procedure DoPower(pow: Real);
      function AsString: string; override;
      procedure Conversion(DestConv: TAbstractStreamableConvType);
    published
      property isExplicitlyConverted: boolean read ExplicitConversion;
  end;

  TVarWithUnitType = class (TAbstractWrapperVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    function CompareOp(const Left, Right: TVarData; const Operator: Integer): Boolean; override;
  end;

  TVarWithUnitVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TVarWithUnit;
    Reserved4: LongInt;
  end;

  EPhysUnitError = class (Exception);

  var PhysUnitData: TPhysUnitData;

implementation

uses Variants,math,strUtils,Simple_Parser_lib,VarCmplx;
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
    end
    else if (str[i]='{') or (str[i]='}') or (str[i]='.') then
      str[i]:='_';
  Result:=Result+RightStr(str,Length(str)-prev+1);
end;

function ConvTypeToStr(ConvType: TAbstractStreamableConvType): string;
begin
  if not ConvType.ShortName.TryCaption(Result) and
    not ConvType.Caption.TryCaption(Result) then
      Result:=ConvType.Name;
end;

function ConvTypeToFamilyLetter(ConvType: TAbstractStreamableConvType): string;
begin
  Result:=ConvType.family.ShortName.Caption;
end;

(*
      TStreamableConvFamily
                                *)
constructor TStreamableConvFamily.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCaption:=TLocalizedName.Create;
  fShortName:=TLocalizedName.Create;
  fDescription:=TLocalizedName.Create;
  fFormula:=TUnitsWithExponents.Create(self);
end;

destructor TStreamableConvFamily.Destroy;
begin
  fCaption.Free;
  fShortName.Free;
  fDescription.Free;
  inherited Destroy;
end;

function TStreamableConvFamily.index: Integer;
begin
  Result:=(Owner as TPhysUnitData).fFamilyList.IndexOf(self);
end;

procedure TStreamableConvFamily.SetupFormula;
begin
  if IsBase then fFormula.SetBaseType(BaseType)
  else fFormula.TakeFromString(formula);
end;

(*
    TAbstractStreamableConvType
                                  *)
constructor TAbstractStreamableConvType.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCaption:=TLocalizedName.Create;
  fShortName:=TLocalizedName.Create;
end;

destructor TAbstractStreamableConvType.Destroy;
begin
  fCaption.Free;
  fShortName.Free;
  inherited Destroy;
end;

function TAbstractStreamableConvType.Family: TStreamableConvFamily;
begin
  Result:=Owner as TStreamableConvFamily;
end;

function TAbstractStreamableConvType.CreateAndConnectScaled(mult: Real): TAbstractStreamableConvType;
var iterator: TAbstractStreamableConvType;
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

procedure TAbstractStreamableConvType.Add(V1, V2: Variant;
  t2: TAbstractStreamableConvType; out value: Variant;
  out t3: TAbstractStreamableConvType);
begin
  Raise Exception.CreateFmt('Unable to add %s with %s',[Caption.Caption,t2.Caption.Caption]);
end;

(*
    TNormalConvType
                      *)
procedure TNormalConvType.Add(V1, V2: Variant;
  t2: TAbstractStreamableConvType; out value: Variant;
  out t3: TAbstractStreamableConvType);
begin
  if Owner=t2.Owner then begin
    value:=v1+ConvertFromBase(t2.ConvertToBase(v2));
    t3:=self;
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

function TNormalConvType.CreateScaled(mult: Real): TAbstractStreamableConvType;
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

function TAffineConvType.CreateScaled(mult: Real): TAbstractStreamableConvType;
var cpy: TAffineConvType absolute Result;
begin
  Result:=TAffineConvType.Clone(self);
  Result.PrefixOk:=false;
  Result.IsScaled:=true;
  cpy.Multiplier:=Multiplier*mult;
end;

procedure TAffineConvType.Add(V1, V2: Variant;
  t2: TAbstractStreamableConvType; out value: Variant;
  out t3: TAbstractStreamableConvType);
var t2a: TAffineConvType absolute t2;
begin
  if Owner = t2.Owner then begin
    value:=v1+GetAffineUnit((t2 as TAffineConvType).Factor).ConvertFromBase(
      t2.ConvertToBase(v2));
    t3:=GetAffineUnit(Factor+t2a.Factor);
  end
  else inherited;
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
  der.ensureCorrectName(NoSpaces(der.Caption.InEnglish),owner);
  owner.InsertComponent(der);
  Result:=der;  
end;


(*
    TLogarithmicConvType
                          *)
resourcestring
  LogarithmicConvTypeRequiresRealNum = 'Лог. единицы измерения допустимы только для действительных значений';

procedure TLogarithmicConvType.Add(V1, V2: Variant;
  t2: TAbstractStreamableConvType; out value: Variant;
  out t3: TAbstractStreamableConvType);
begin
  if Owner=t2.Owner then begin
    value:=ConvertFromBase(ConvertToBase(v1)+t2.ConvertToBase(v2));
    t3:=self;
  end
  else inherited;
end;

function TLogarithmicConvType.ConvertFromBase(value: Variant): Variant;
begin
  if VarIsNumeric(value) then
    Result:=Log10Multiplier*log10(value/ZeroValue)
  else
    Raise Exception.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.ConvertToBase(value: Variant): Variant;
begin
  if VarIsNumeric(value) then
    Result:=ZeroValue*power(10.0,(value/Log10Multiplier))
  else
    Raise Exception.Create(LogarithmicConvTypeRequiresRealNum);
end;

function TLogarithmicConvType.CreateScaled(mult: Real): TAbstractStreamableConvType;
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
  fFullName:=TLocalizedName.Create;
  fPrefix:=TLocalizedName.Create;
  fIsPreferred:=true;
end;

destructor TUnitPrefix.Destroy;
begin
  fFullName.Free;
  fPrefix.Free;
  inherited Destroy;
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
  fFamilyList:=TObjectList.Create(false);
  fConvUnitsIterator:=TAbstractDocumentClassIterator.Create(self,TAbstractStreamableConvType);
  fMegaList:=TStringList.Create;
  fMegaList.CaseSensitive:=true;
  fMegaList.Sorted:=true;
  fMegaList.Duplicates:=dupAccept;
  fWarningList:=TStringList.Create;
end;

destructor TPhysUnitData.Destroy;
begin
  fMegaList.Free;
  fWarningList.Free;
  fFamilyList.Free;
  inherited Destroy;
end;


procedure TPhysUnitData.Loaded;
var j: Integer;
    ct: TAbstractStreamableConvType;
    cpy: TAbstractStreamableConvType;
    pref: TUnitPrefix;
    m1: Real;
    fn: string;
    dimensionless: TUnitsWithExponents;

    procedure HandleCreatedCopy;
    var k,i,neut: Integer;
        temp: TLocalizedName;
    begin
      temp:=TLocalizedName.Create;
    //для каждого языка нашей величины мы должны найти подходящий язык приставки
      for k:=0 to cpy.fShortName.strings.Count-1 do
        if cpy.fShortName.isNeutral(k) then begin
          neut:=pref.Prefix.strings.IndexOfObject(nil);
          if neut<0 then
            for i:=0 to pref.Prefix.strings.Count-1 do
              temp.strings.AddObject(pref.Prefix.strings[i]+cpy.fShortName.strings[k],
                pref.Prefix.strings.Objects[i])
          else
            temp.strings.AddObject(pref.Prefix.strings[neut]+cpy.fShortName.strings[k],nil);
        end
        else
          temp.strings.AddObject(pref.Prefix.MatchingString(cpy.fShortName.strings.Objects[k])
            +cpy.fShortName.strings[k],cpy.fShortname.strings.Objects[k]);
      cpy.fShortName.Assign(temp);
      temp.clear;
      for k:=0 to cpy.fCaption.strings.Count-1 do
        if cpy.Caption.isNeutral(k) then begin
          neut:=pref.FullName.strings.IndexOfObject(nil);
          if neut<0 then
            for i:=0 to pref.FullName.strings.Count-1 do
              temp.strings.AddObject(pref.FullName.strings[i]+cpy.Caption.strings[k],
              pref.FullName.strings.Objects[i])
          else
            temp.strings.AddObject(pref.FullName.strings[neut]+cpy.Caption.strings[k],nil);
        end
        else
          temp.strings.AddObject(pref.FullName.MatchingString(cpy.fCaption.strings.Objects[k])
            +cpy.fCaption.strings[k],cpy.Caption.strings.Objects[k]);
      cpy.Caption.Assign(temp);
      temp.Free;
      if cpy.Caption.enabled then
        cpy.EnsureCorrectName(NoSpaces(cpy.fCaption.InEnglish),self)
      else
        cpy.ensureCorrectName(NoSpaces(cpy.ShortName.InEnglish),self);
      ct.Owner.InsertComponent(cpy);
    end;

    procedure AddToList(str: string; obj: TObject);
    begin
      if fMegaList.IndexOf(str)>=0 then
        fWarningList.Add(Format('%s ident ambiguity',[str]));
      fMegaList.AddObject(str,obj);
    end;


begin
  inherited Loaded;
  UnitPrefixes.PrepareLists;
//создадим физ. величины с приставками
  fConvUnitsIterator.First(ct);
  while Assigned(ct) do begin
    if ct.PrefixOk then begin
      fn:=ct.Name;
      //нашли новую жертву
      for j:=0 to UnitPrefixes.fHigher.Count-1 do begin
        pref:=TUnitPrefix(UnitPrefixes.fHigher[j]);
        m1:=pref.Multiplier;
        cpy:=ct.CreateAndConnectScaled(m1);
        HandleCreatedCopy;
      end;
      for j:=UnitPrefixes.fLower.Count-1 downto 0 do begin
        pref:=TUnitPrefix(UnitPrefixes.fLower[j]);
        m1:=pref.Multiplier;
        cpy:=ct.CreateAndConnectScaled(m1);
        HandleCreatedCopy;
      end;
      //остальные приставки нам не нравятся!
      for j:=0 to UnitPrefixes.fOther.Count-1 do begin
        pref:=TUnitPrefix(UnitPrefixes.fOther[j]);
        m1:=pref.Multiplier;
        cpy:=ct.CreateScaled(m1);
        HandleCreatedCopy;
      end;
    end;
    fConvUnitsIterator.Next(ct);
  end;
  //все величины введены, теперь можно составить гигантский список, чтобы быстрее искать
  //(по алфавиту) и определять повторяющиеся названия
  fConvUnitsIterator.First(ct);
  while Assigned(ct) do begin
    //family.unit
    for j:=0 to ct.fShortName.strings.Count-1 do begin
      if (ct.Owner as TStreamableConvFamily).ShortName.TryMatchingString(
        ct.ShortName.strings.Objects[j],fn) then
        AddToList(NoSpaces(fn)+'.'+ct.ShortName.strings[j],ct);
      if (ct.Owner as TStreamableConvFamily).Caption.TryMatchingString(
        ct.ShortName.strings.Objects[j],fn) then
        AddToList(NoSpaces(fn)+'.'+ct.ShortName.strings[j],ct);
      //самое "хрупкое" - короткие названия без нифига, очень возможен повтор
      AddToList(ct.ShortName.strings[j],ct);
    end;

    for j:=0 to ct.Caption.strings.Count-1 do begin
      if (ct.Owner as TStreamableConvFamily).ShortName.TryMatchingString(
        ct.caption.strings.Objects[j],fn) then
        AddToList(NoSpaces(fn)+'.'+NoSpaces(ct.Caption.strings[j]),ct);
      if (ct.Owner as TStreamableConvFamily).Caption.TryMatchingString(
        ct.caption.strings.Objects[j],fn) then
        AddToList(NoSpaces(fn)+'.'+NoSpaces(ct.Caption.strings[j]),ct);
      AddToList(NoSpaces(ct.Caption.strings[j]),ct);
    end;


    fConvUnitsIterator.Next(ct);
  end;

  //на этом этапе TUnitsWithExponents должна стать работоспособной
  for j:=0 to fFamilyList.Count-1 do
    (fFamilyList[j] as TStreamableConvFamily).SetupFormula; 
  //осталось
  //найти Unity, а именно - стандартную безразм. величину
  dimensionless:=TUnitsWithExponents.Create(self);
  funity:=dimensionless.GetConvType;
  dimensionless.Free;
  //и еще DMS отыскать, ну пусть он будет тупо в семье Angle и под именем DMS
  fDMS:=FindComponent('Angle').FindComponent('DMS') as TAbstractStreamableConvType;

  fMegaList.SaveToFile('megalist.txt');
  fWarningList.SaveToFile('warnings.txt');
end;

procedure TPhysUnitData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opInsert) and (AComponent is TStreamableConvFamily) then
    fFamilyList.Add(AComponent);  
end;

function TPhysUnitData.StrToConvType(str: string): TAbstractStreamableConvType;
var i,j,k: Integer;
    obj: TAbstractStreamableConvType;
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
        warningProc(Format('%s can refer to:',[str]));
        for k:=i to j-1 do
          warningProc(TAbstractStreamableConvType(fMegaList.Objects[k]).Caption.Caption);
      end;
    //выбираем либо ед. измерения без приставки
      for k:=i to j-1 do begin
        Result:=TAbstractStreamableConvType(fMegaList.Objects[k]);
        if not Result.IsScaled then begin
          if Assigned(warningProc) then
            warningProc(Format('we used %s',[Result.Caption.Caption]));
          Exit;
        end
      end;
    //если дошли до этого места, значит он все с приставками, выберем наобум
      warningProc(Format('we used %s',[Result.Caption.Caption]));
    end
    else
      Result:=TAbstractStreamableConvType(fMegaList.Objects[i])
  end
  else begin
  //ничего не нашли, это вестимо что-то составное
    fml:=TUnitsWithExponents.Create(self);
    multiplier:=fml.TakeFromString(str);
    obj:=fml.GetConvType; //возможно, сейчас на лету была создана подходящая величина
                          //но в тек. реализации она ничего не добавляет в megalist
                          //obj может отличаться только множителем от правильной разм
    if multiplier=1 then
      Result:=obj
    else begin
      Result:=TNormalConvType.Create(obj.Owner);
      Result.Name:=NoSpaces(str);
      NormalRslt.Multiplier:=multiplier;
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

procedure TVarWithUnit.Assign(str: string);
var unitStr: string;
    i: Integer;
    val: Extended;
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
    ConvType:=PhysUnitData.Unity;  //безразм.
  end
  else begin
    //посерединке пробел
    //либо часть справа от пробела-ед. изм., либо например разделенные действ и мним. части
    if AnsiUppercase(str[Length(str)])='I' then begin
      Instance:=VarComplexCreate(str);
      ConvType:=PhysUnitData.Unity;
    end
    else begin
      if TryStrToFloat(LeftStr(str,i-1),val) then
        instance:=val
      else
        instance:=VarComplexCreate(LeftStr(str,i-1));
      unitStr:=RightStr(str,Length(str)-i);
      ConvType:=PhysUnitData.StrToConvType(unitStr);
    end;
  end;
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

procedure TVarWithUnit.Conversion(DestConv: TAbstractStreamableConvType);
var j,i,b: Integer;              //inst - это мы
    offset,k,mul: Real;     //dest - это тоже мы, но позже
//    formula: TUnitsWithExponent;
//    new_formula: TUnitsWithExponent;
    addition: Variant;
    pow: Real;
//    buConvType: TConvType;
    s: string;
resourcestring
  ToConvertFromAToB = 'Для преобразования из [%s] в [%s] выражение домножено на %s';
  IncorrectUnitConversion = 'Некорректное приведение типов: [%s] в [%s]';
begin
(*
  buConvType:=ConvType;
  ConvType:=DestConv;
  if IsAffine(j) then
    DestConv:=AffineUnits[j].BaseConvType;
  ConvType:=buConvType;
*)  //для афинных величин
(*
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
  //на этой стадии у нас уже не афинная величина, а самая простая (мы так считаем)
  if (ConvType<>DestConv) then begin
    if instance=0 then
      ConvType:=DestConv
    else if CompatibleConversionTypes(ConvType,DestConv) then begin
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
  *)
end;

constructor TVarWithUnit.Create(text: string);
begin
  Create;
  Assign(text);
end;

constructor TVarWithUnit.CreateFromVariant(source: Variant;
  aConvType: TAbstractStreamableConvType);
begin
  Create;
  instance:=source;
  ConvType:=aConvType;
end;

procedure TVarWithUnit.DoAdd(value: TAbstractWrapperData);
begin
  inherited;

end;

procedure TVarWithUnit.DoDivide(Right: TAbstractWrapperData);
begin
  inherited;

end;

procedure TVarWithUnit.DoInverse;
begin

end;

procedure TVarWithUnit.DoMultiply(Right: TAbstractWrapperData);
begin
  inherited;

end;

procedure TVarWithUnit.DoPower(pow: Real);
begin

end;

procedure TVarWithUnit.Negate;
begin
  inherited;

end;

{ TVarWithUnitType }

procedure TVarWithUnitType.Cast(var Dest: TVarData;
  const Source: TVarData);
begin
  //строку преобразуем по всем правилам, а любые другие Variant'ы "оборачиваем" безразм.
(*
  VarDataClear(Dest);
  if VarDataIsStr(Source) then
    TWrapperVarData(Dest).Data:=TVarWithUnit.Create(VarDataToStr(Source))
  else
    TWrapperVarData(Dest).Data:=TVarWithUnit.CreateFromVariant(Variant(source),duUnity);
  Dest.VType:=VariantWithUnit;
*)  //надо еще обозначить duUnity
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
//          tmp:=VarWithUnitConvert(Variant(source),duUnity); //duUnity еще нет
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

function TUnitsWithExponents.AddArbitraryUnit(ConvType: TAbstractStreamableConvType; Exponent: Real): Real;
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
      Result:=Power((ConvType as TNormalConvType).Multiplier,Exponent);
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
begin
  result:=ShowSomething(ConvTypeToStr);
end;

procedure TUnitsWithExponents.Clear;
begin
  fCount:=0;
  SetLength(UnitTypes,0);
  SetLength(Exponents,0);
end;

constructor TUnitsWithExponents.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TUnitsWithExponents.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',nil,WriteData,true);
end;

procedure TUnitsWithExponents.WriteData(writer: TWriter);
begin
  writer.WriteString(AsString);
end;

function TUnitsWithExponents.PhysData: TPhysUnitData;
var comp: TComponent;
begin
  comp:=Owner;
  while Assigned(comp) and not (comp is TPhysUnitData) do
    comp:=comp.Owner;
  Result:=TPhysUnitData(comp);
  if Result=nil then raise Exception.Create('UnitsWithExponents didn''t find its PhysData parent');
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
    first,second: TStreamableConvFamily;
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

procedure TUnitsWithExponents.SetBaseType(value: TAbstractStreamableConvType);
begin
  fCount:=1;
  SetLength(UnitTypes,1);
  SetLength(Exponents,1);
  UnitTypes[0]:=value;
  Exponents[0]:=1;
end;

function TUnitsWithExponents.ShowFormula: string;
begin
  Result:=ShowSomething(ConvTypeToFamilyLetter);
end;

function TUnitsWithExponents.ShowSomething(proc: TShowName): string;
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

function TUnitsWithExponents.TakeFromString(formula: string): Real;
var p: TSimpleParser;
    term: string;
    convType: TAbstractStreamableConvType;
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
      ConvType:=PhysData.StrToConvType(term);

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
      Result:=Result*AddArbitraryUnit(ConvType,pow);
      IsDivide:=nextDivide;
//      чтобы он к примеру Джоули преобр. в кг*м^2/с^2
    end;
  finally
    p.Free;
  end;

end;


function TUnitsWithExponents.GetConvType: TAbstractStreamableConvType;
var boss: TPhysUnitData;
    i: Integer;
    fam: TStreamableConvFamily;
    un: TNormalConvType;
begin
  boss:=PhysData;
  for i:=0 to boss.fFamilyList.Count-1 do begin
    fam:=boss.fFamilyList[i] as TStreamableConvFamily;
    if SameFamily(fam.fFormula) then begin
      Result:=fam.BaseType;
      Exit;
    end;
  end;
  //не нашли подходящую семью - придется создать!
  fam:=TStreamableConvFamily.Create(boss);
  fam.fFormula.Assign(self);
  fam.Name:=NoSpaces(ShowFormula);
  //и еще подходящую единицу измерения создадим
  //нормальную, других нельзя
  un:=TNormalConvType.Create(fam);
  un.Name:=NoSpaces(AsString);
  un.Multiplier:=1;
  //нужно ей и название сформировать, чтоб находить на всех языках
  //или хотя бы на одном
  //shortname или caption - пофиг по большому счету!
  un.ShortName.AddInCurrentLang(AsString);
  fam.BaseType:=un;
  Result:=un;
end;

initialization
  RegisterClasses([TAffineConvType,TLogarithmicConvType,TNormalConvType,
  TPhysUnitData,TStreamableConvFamily,TUnitPrefix,TUnitPrefixes]);

end.
