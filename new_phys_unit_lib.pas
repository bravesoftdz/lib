unit new_phys_unit_lib;

interface
uses set_english_locale_if_not_sure,streaming_class_lib,classes,
  command_class_lib,iterator_lib;

type
  TStreamableConvFamily=class(TStreamingClass) //можно будет и к TComponent вернуться
    private
      fIsBase: Boolean;
      fCaption,fShortName,fDescription: TLocalizedName;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
    published
      property IsBase: Boolean read fIsBase write fIsBase;
      property Caption: TLocalizedName read fCaption write fCaption;
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Description: TLocalizedName read fDescription write fDescription;
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
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
    end;

  TAffineConvType=class(TAbstractStreamableConvType)
    private
      fMultiplier, fOffset: Real;
    published
      property Multiplier: Real read fMultiplier write fMultiplier;
      property Offset: Real read fOffset write fOffset;
    end;

  TLogarithmicConvType=class(TAbstractStreamableConvType)
    private
      fLog10Mult: Real;
      fZeroValue: Real;
    public
      function ConvertToBase(value: Variant): Variant; override;
      function ConvertFromBase(value: Variant): Variant; override;
      function CreateScaled(mult: Real): TAbstractStreamableConvType; override;
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

  TPhysUnitData = class(TAbstractDocument)
  private
    fConvUnitsIterator,fConvFamiliesIterator: TAbstractDocumentClassIterator;
    fMegaList: TStringList;
    fWarningList: TStringList;
  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    UnitPrefixes: TUnitPrefixes;
//    FundamentalPhysConstants: TFundamentalPhysConstants;
  end;


implementation

uses Variants,math,sysUtils,strUtils;
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


(*
      TStreamableConvFamily
                                *)
constructor TStreamableConvFamily.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCaption:=TLocalizedName.Create;
  fShortName:=TLocalizedName.Create;
  fDescription:=TLocalizedName.Create;
end;

destructor TStreamableConvFamily.Destroy;
begin
  fCaption.Free;
  fShortName.Free;
  fDescription.Free;
  inherited Destroy;
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

(*
    TNormalConvType
                      *)
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

(*
    TLogarithmicConvType
                          *)
resourcestring
  LogarithmicConvTypeRequiresRealNum = 'Лог. единицы измерения допустимы только для действительных значений';

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
  fConvFamiliesIterator:=TAbstractDocumentClassIterator.Create(self,TStreamableConvFamily);
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
  inherited Destroy;
end;

procedure TPhysUnitData.Loaded;
var j: Integer;
    ct: TAbstractStreamableConvType;
    cpy: TAbstractStreamableConvType;
    pref: TUnitPrefix;
    m1: Real;
    fn: string;

    procedure HandleCreatedCopy;
    var k: Integer;
    begin
    //для каждого языка нашей величины мы должны найти подходящий язык приставки
      for k:=0 to cpy.fShortName.strings.Count-1 do
        cpy.fShortName.strings[k]:=
          pref.Prefix.MatchingString(cpy.fShortName.strings.Objects[k])+
            cpy.fShortName.strings[k];

      for k:=0 to cpy.fCaption.strings.Count-1 do
        cpy.fCaption.strings[k]:=
          pref.FullName.MatchingString(cpy.fCaption.strings.Objects[k])+
            cpy.fCaption.strings[k];

      cpy.EnsureCorrectName(cpy.fCaption.InEnglish,self);
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
        AddToList(NoSpaces(fn)+'.'+ct.Caption.strings[j],ct);
      if (ct.Owner as TStreamableConvFamily).Caption.TryMatchingString(
        ct.caption.strings.Objects[j],fn) then
        AddToList(NoSpaces(fn)+'.'+ct.Caption.strings[j],ct);
      AddToList(ct.Caption.strings[j],ct);
    end;


    fConvUnitsIterator.Next(ct);
  end;
  fMegaList.SaveToFile('megalist.txt');
  fWarningList.SaveToFile('warnings.txt');
end;

initialization
  RegisterClasses([TAffineConvType,TLogarithmicConvType,TNormalConvType,
  TPhysUnitData,TStreamableConvFamily,TUnitPrefix,TUnitPrefixes]);

end.
