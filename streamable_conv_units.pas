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
end;


var cbVoltage, cbCurrent, cbPressure, cbVolumetricFlowRate, cbPower, cbFrequency: TConvFamily;
    vuVolts,vumV,vuuV,vukV,vuMegaV: TConvType;
    iuAmps,iumA,iuuA,iukA,iuMegaA: TConvType;
    puBar,puPa,pukPa,puMegaPa, puMeters: TConvType;
    vcuM3PerH,vcuLPerSec,vcuLperMin,vcuLPerH,vcuM3PerSec,vcuM3PerMin: TConvType;
    powWatt,powkW,powmW,powuW,powMegaW: TConvType;
    fuHz,fukHz,fuMHz,fuGHz: TConvType;
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

procedure NewConvFamilies;
begin
  cbVoltage:=RegisterConversionFamily('Voltage');
  vuVolts:=RegisterConversionType(cbVoltage,'V',1);
  vukV:=RegisterConversionType(cbVoltage,'kV',1000);
  vuMegaV:=RegisterConversionType(cbVoltage,'Megavolts',1e6);
  vumV:=RegisterConversionType(cbVoltage,'mV',1e-3);
  vuuV:=RegisterConversionType(cbVoltage,'uV',1e-6);

  cbCurrent:=RegisterConversionFamily('Current');
  iuAmps:=RegisterConversionType(cbCurrent,'A',1);
  iukA:=RegisterConversionType(cbCurrent,'kA',1000);
  iuMegaA:=RegisterConversionType(cbCurrent,'Megaamperes',1e6);
  iumA:=RegisterConversionType(cbCurrent,'mA',1e-3);
  iuuA:=RegisterConversionType(cbCurrent,'uA',1e-6);

  cbPressure:=RegisterConversionFamily('Pressure');
  puPa:=RegisterConversionType(cbPressure,'Pa',1);
  pukPa:=RegisterConversionType(cbPressure,'kPa',1000);
  puMegaPa:=RegisterConversionType(cbPressure,'MPa',1e6);
  puMeters:=RegisterConversionType(cbPressure,'m',9800);
  puBar:=RegisterConversionType(cbPressure,'bar',101000);

  cbVolumetricFlowRate:=RegisterConversionFamily('VolumetricFlowRate');
  vcuM3PerSec:=RegisterConversionType(cbVolumetricFlowRate,'m3/sec',1);
  vcuM3PerMin:=RegisterConversionType(cbVolumetricFlowRate,'m3/min',1/60);
  vcuM3PerH:=RegisterConversionType(cbVolumetricFlowRate,'m3/h',1/3600);
  vcuLPerSec:=RegisterConversionType(cbVolumetricFlowRate,'L/sec',1e-3);
  vcuLPerMin:=RegisterConversionType(cbVolumetricFlowRate,'L/min',1e-3/60);
  vcuLPerH:=RegisterConversionType(cbVolumetricFlowRate,'L/h',1e-3/3600);

  cbPower:=RegisterConversionFamily('Power');
  powWatt:=RegisterConversionType(cbPower,'W',1);
  powmW:=RegisterConversionType(cbPower,'mW',1e-3);
  powuW:=RegisterConversionType(cbPower,'uW',1e-6);
  powkW:=RegisterConversionType(cbPower,'kW',1000);
  powMegaW:=RegisterConversionType(cbPower,'Megawatt',1e6);

  cbFrequency:=RegisterConversionFamily('Frequency');
  fuHz:=RegisterConversionType(cbFrequency,'Hz',1);
  fukHz:=RegisterConversionType(cbFrequency,'kHz',1000);
  fuMHz:=RegisterConversionType(cbFrequency,'MHz',1e6);
  fuGHz:=RegisterConversionType(cbFrequency,'GHz',1e9);
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


initialization
  NewConvFamilies;
  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);
  RegisterClass(TPreferredUnits);
(*
  PreferredUnits:=TPreferredUnits.Create(nil);
  PreferredUnits.Add(cbVoltage,vuVolts);
  PreferredUnits.Add(cbCurrent,iuAmps);
  PreferredUnits.Add(cbPressure,puBar);
  PreferredUnits.Add(cbVolumetricFlowRate,vcuLPerMin);
  PreferredUnits.Add(cbTemperature,tuCelsius);
  PreferredUnits.Add(cbPower,powkW);
  PreferredUnits.saveFormat:=fCyr;
  PreferredUnits.SaveToFile('PreferredUnits.txt');
*)
  PreferredUnits:=TPreferredUnits.LoadFromFile('PreferredUnits.txt');

finalization
  FreeAndNil(PreferredUnits);

end.
