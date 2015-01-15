unit streamable_conv_units;

interface

uses classes,ConvUtils,streaming_class_lib;

type
  TPreferredUnits=class(TStreamingClass)
  //содержит в себе пары (семейство;предпочитаемая величина)
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

var cbVoltage, cbCurrent, cbPressure, cbVolumetricFlowRate, cbPower,
    cbFrequency, cbDimensionless,cbForce,cbEnergy,cbCharge,cbResistance,
    cbCapacitance,cbInductance: TConvFamily;
    vuVolts,iuAmps,ruOhm,cuFarade,iuHenry: TConvType;
    puBar,puPa,puMeters: TConvType;
    vcuM3PerSec: TConvType;
    powWatt: TConvType;
    fuHz: TConvType;
    duUnity: TConvType;
    fuN: TConvType;
    euJ: TConvType;
    cuC: TConvType;

    duShortMeters, muShortKilograms, tuShortSeconds,tuShortKelvin: TConvType;
    auShortSqMeters,vuShortCubicMeters: TConvType;

    PreferredUnits: TPreferredUnits;
implementation

uses SysUtils,stdConvs,variants,VarCmplx;

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

function KelvinToCelsius(const AValue: Double): Double;
begin
  Result := AValue - 273.15;
end;

function CelsiusToKelvin(const AValue: Double): Double;
begin
  Result := AValue + 273.15;
end;

procedure NewConvFamilies;
begin
  cbVoltage:=RegisterConversionFamily('Voltage');
  vuVolts:=RegisterConversionType(cbVoltage,'V',1);
  RegisterConversionType(cbVoltage,'В',1);

  cbCurrent:=RegisterConversionFamily('Current');
  iuAmps:=RegisterConversionType(cbCurrent,'A',1);
  RegisterConversionType(cbCurrent,'А',1);

  cbPressure:=RegisterConversionFamily('Pressure');
  puPa:=RegisterConversionType(cbPressure,'Pa',1);
  RegisterConversionType(cbPressure,'Па',1);
  RegisterConversionType(cbPressure,'m_water',9800);
  RegisterConversionType(cbPressure,'м_вод_ст',9800);
  RegisterConversionType(cbPressure,'bar',101000);
  RegisterConversionType(cbPressure,'атм',101000);

  cbVolumetricFlowRate:=RegisterConversionFamily('VolumetricFlowRate');
  vcuM3PerSec:=RegisterConversionType(cbVolumetricFlowRate,'m3/sec',1);

  cbPower:=RegisterConversionFamily('Power');
  powWatt:=RegisterConversionType(cbPower,'W',1);
  RegisterConversionType(cbPower,'Вт',1);

  cbFrequency:=RegisterConversionFamily('Frequency');
  fuHz:=RegisterConversionType(cbFrequency,'Hz',1);
  RegisterConversionType(cbFrequency,'Гц',1);

  cbDimensionless:=RegisterConversionFamily('Dimensionless');
  duUnity:=RegisterConversionType(cbDimensionless,'',1);

  cbForce:=RegisterConversionFamily('Force');
  fuN:=RegisterConversionType(cbForce,'N',1);
  RegisterConversionType(cbForce,'Н',1);
  RegisterConversionType(cbForce,'kgf',9.81);
  RegisterConversionType(cbForce,'кгс',9.81);

  cbEnergy:=RegisterConversionFamily('Energy');
  euJ:=RegisterConversionType(cbEnergy,'J',1);
  RegisterConversionType(cbEnergy,'Дж',1);

  RegisterConversionType(cbDistance,'м',1);
  duShortMeters:=RegisterConversionType(cbDistance,'m',1);

  cbCharge:=RegisterConversionFamily('Charge');
  cuC:=RegisterConversionType(cbCharge,'C',1);
  RegisterConversionType(cbCharge,'Кл',1);

  cbResistance:=RegisterConversionFamily('Resistance');
  ruOhm:=RegisterConversionType(cbResistance,'Ohm',1);
  RegisterConversionType(cbResistance,'Ом',1);

  cbCapacitance:=RegisterConversionFamily('Capacitance');
  cuFarade:=RegisterConversionType(cbCapacitance,'F',1);
  RegisterConversionType(cbCapacitance,'Ф',1);

  cbInductance:=RegisterConversionFamily('Inductance');
  iuHenry:=RegisterConversionType(cbInductance,'H',1);
  RegisterConversionType(cbInductance,'Гн',1);

  RegisterConversionType(cbTime,'с',1 / SecsPerDay);
  tuShortSeconds:=RegisterConversionType(cbTime,'s',1 / SecsPerDay);
  RegisterConversionType(cbTime,'hr',3600 / SecsPerDay);
  RegisterConversionType(cbTime,'ч',3600 / SecsPerDay);
  RegisterConversionType(cbTime,'min',60 / SecsPerDay);
  RegisterConversionType(cbTime,'мин',60 / SecsPerDay);

  RegisterConversionType(cbMass,'g',1);
  muShortKilograms:=RegisterConversionType(cbMass,'kg',1000);
  RegisterConversionType(cbMass,'г',1);
  auShortSqMeters:=RegisterConversionType(cbArea,'m^2',1);
  vuShortCubicMeters:=RegisterConversionType(cbVolume,'m^3',1);
  RegisterConversionType(cbVolume,'L',0.001);

  tuShortKelvin:=RegisterConversionType(cbTemperature,'K',KelvinToCelsius, CelsiusToKelvin);
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
//    aFamInt: Integer absolute aFamily;
//странно, чем ему эта строка не понравилась?
//длина разная, вот чем.
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
//а ведь даже температура может быть комплексной, когда рассм. темп. волны
//кроме того, возможно введение лог. величин типа dbV
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


initialization
  NewConvFamilies;
  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);
  RegisterClass(TPreferredUnits);
  if FileExists('PreferredUnits.txt') then
    PreferredUnits:=TPreferredUnits.LoadFromFile('PreferredUnits.txt');
  GConvUnitToStrFmt:='%g %s';

finalization
  FreeAndNil(PreferredUnits);

end.
