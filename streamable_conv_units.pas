unit streamable_conv_units;

interface

uses ConvUtils;

var cbVoltage, cbCurrent, cbPressure, cbVolumeConsumption: TConvFamily;
    vuVolts,vumV,vuuV,vukV,vuMegaV: TConvType;
    iuAmps,iumA,iuuA,iukA,iuMegaA: TConvType;
    puBar,puPa,pukPa,puMegaPa: TConvType;
    vcuM3PerH,vcuLPerSec,vcuLperMin,cvuLPerH: TConvType;
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
  cbCurrent:=RegisterConversionFamily('Current');
  cbPressure:=RegisterConversionFamily('Pressure');
  cbVolumeConsumption:=RegisterConversionFamily('VolumeConsumption');
end;



initialization
  NewConvFamilies;
  RegisterIntegerConsts(TypeInfo(TConvFamily),NameToFamily,FamilyToName);
  RegisterIntegerConsts(TypeInfo(TConvType),NameToConv,ConvToName);

end.
