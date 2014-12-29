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

uses VariantWrapper,ConvUtils;

type

TBaseFamilyEntry=record
  ConvFamily: TConvFamily;  //ссылка на description здесь же
  letter: string;
end;  //неужели больше ничего не нужно?

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

uses StdConvs,streamable_conv_units,sysUtils;

var BaseFamilyEntries: array of TBaseFamilyEntry;
    DerivedFamilyEntry: array of TDerivedFamilyEntry;

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

procedure RegisterDerivedConversionFamily(Family: TConvFamily; formula: string);
begin

end;

procedure RegisterDerivedConversionFamily(formula: TUnitsWithExponent);
begin
  RegisterConversionFamily(formula.AsString)

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



initialization
  RegisterStandartUnits;

end.
