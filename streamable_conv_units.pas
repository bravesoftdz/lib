unit streamable_conv_units;

interface

uses ConvUtils;

var cbVoltage, cbCurrent, cbPressure, cbVolumetricFlowRate, cbPower: TConvFamily;
    vuVolts,vumV,vuuV,vukV,vuMegaV: TConvType;
    iuAmps,iumA,iuuA,iukA,iuMegaA: TConvType;
    puBar,puPa,pukPa,puMegaPa, puMeters: TConvType;
    vcuM3PerH,vcuLPerSec,vcuLperMin,vcuLPerH,vcuM3PerSec,vcuM3PerMin: TConvType;
    powWatt,powkW,powmW,powuW,powMegaW: TConvType;
implementation

uses classes;

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
  vcuM3PerMin:=RegisterConversionType(cbVolumetricFlowRate,'m3/min',60);
  vcuM3PerH:=RegisterConversionType(cbVolumetricFlowRate,'m3/h',3600);
  vcuLPerSec:=RegisterConversionType(cbVolumetricFlowRate,'L/sec',1e-3);
  vcuLPerMin:=RegisterConversionType(cbVolumetricFlowRate,'L/min',60e-3);
  vcuLPerH:=RegisterConversionType(cbVolumetricFlowRate,'L/h',3600e-3);

  cbPower:=RegisterConversionFamily('Power');
  powWatt:=RegisterConversionType(cbPower,'W',1);
  powmW:=RegisterConversionType(cbPower,'mW',1e-3);
  powuW:=RegisterConversionType(cbPower,'uW',1e-6);
  powkW:=RegisterConversionType(cbPower,'kW',1000);
  powMegaW:=RegisterConversionType(cbPower,'Megawatt',1e6);
end;



initialization
  NewConvFamilies;
  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);

end.
