unit new_phys_unit_lib;

interface
uses set_english_locale_if_not_sure,streaming_class_lib,classes;

type
  TStreamableConvFamily=class(TComponent) //можно будет и к TComponent вернуться
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

  TAbstractStreamableConvType=class(TComponent)
    private
      fShortName,fCaption: TLocalizedName;
      fFamily: TStreamableConvFamily;
      fPrefixOK: boolean;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
    published
      property ShortName: TLocalizedName read fShortName write fShortName;
      property Caption: TLocalizedName read fCaption write fCaption;
      property Family: TStreamableConvFamily read fFamily write fFamily;
      property PrefixOk: Boolean read fPrefixOk write fPrefixOk;
    end;

  TNormalConvType=class(TAbstractStreamableConvType)
    private
      fMultiplier: Real;
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
      fBase: Real;
      fZeroValue: Real;
    published
      property Base: Real read fBase write fBase;
      property ZeroValue: Real read fZeroValue write fZeroValue;
    end;

  TUnitPrefix=class(TComponent)
    private
      fPrefix,fFullName: TLocalizedName;
      multiplier: Real; //надо будет на expression заменить, но пока страшно
      fIsPreferred: Boolean;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
    published
      property Prefix: TLocalizedName read fPrefix write fPrefix;
      property FullName: TLocalizedName read fFullName write fFullName;
      property IsPreferred: Boolean read fIsPreferred write fIsPreferred;
    end;

  TUnitPrefixes = class(TComponent)
  private
    fPrefixes: TStringList; //чтобы по имени приставки найти множитель
    fPreferredPrefixes: TList;
  public
//    constructor Create(Owner: TComponent); override;
//    destructor Destroy; override;
//    function FindUnitWithPrefix(str: string; out CType: TConvType): boolean;
//    function PrefixDescrToMultiplier(term: string; var modifier: string; out CType: TConvType): Real;
//    procedure Assimilate(source: TUnitPrefixes);
  end;

  TPhysUnitData = class(TStreamingClass)
  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    UnitPrefixes: TUnitPrefixes;
//    FundamentalPhysConstants: TFundamentalPhysConstants;
  end;


implementation

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

(*
    TUnitPrefix
                    *)
constructor TUnitPrefix.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fFullName:=TLocalizedName.Create;
  fPrefix:=TLocalizedName.Create;
end;

destructor TUnitPrefix.Destroy;
begin
  fFullName.Free;
  fPrefix.Free;
  inherited Destroy;
end;

initialization
  RegisterClasses([TAffineConvType,TLogarithmicConvType,TNormalConvType,
  TPhysUnitData,TStreamableConvFamily,TUnitPrefix,TUnitPrefixes]);

end.
