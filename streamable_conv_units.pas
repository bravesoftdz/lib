unit streamable_conv_units;

interface

uses classes,ConvUtils,streaming_class_lib;

type
  TPreferredUnits=class(TStreamingClass)
  //содержит в себе пары (семейство;предпочитаемая величина)
  //используется в Михалыче
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
    fBaseUnit: string;  //вообще, TConvType умеет записываться символьно,
    //однако на момент считывания BaseUnit мы еще можем не знать такого юнита
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

var cbVolumetricFlowRate,
    cbFrequency,
    cbCapacitance,cbInductance, cbSolidAngle: TConvFamily;
    cuFarade,iuHenry: TConvType;
    vcuM3PerSec: TConvType;
    fuRadPerSec: TConvType;

    muShortKilograms, tuShortSeconds,tuShortKelvin: TConvType;
    sauSteradian: TConvType;

    PreferredUnits: TPreferredUnits;
implementation

uses SysUtils,stdConvs,variants,VarCmplx,simple_parser_lib,
  set_english_locale_if_not_sure, phys_units_lib;

var default_dir: string;

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

function KelvinToCelsius(const AValue: Double): Double;
begin
  Result := AValue - 273.15;
end;

function CelsiusToKelvin(const AValue: Double): Double;
begin
  Result := AValue + 273.15;
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

//объемный расход
  cbVolumetricFlowRate:=RegisterConversionFamily('VolumetricFlowRate');
  vcuM3PerSec:=RegisterConversionType(cbVolumetricFlowRate,'m3/sec',1);
//емкость
  cbCapacitance:=RegisterConversionFamily('Capacitance');
  cuFarade:=RegisterConversionType(cbCapacitance,'F',1);
  RegisterConversionType(cbCapacitance,'Ф',1);
//индуктивность
  cbInductance:=RegisterConversionFamily('Inductance');
  iuHenry:=RegisterConversionType(cbInductance,'H',1);
  RegisterConversionType(cbInductance,'Гн',1);

//температура
  tuShortKelvin:=RegisterConversionType(cbTemperature,'K',KelvinToCelsius, CelsiusToKelvin);
  RegisterConversionType(cbTemperature,'gradC',1);
//телесный угол
  cbSolidAngle:=RegisterConversionFamily('SolidAngle');
  sauSteradian:=RegisterConversionType(cbSolidAngle,'sr',1);
  RegisterConversionType(cbSolidAngle,'ср',1);
//частота
  cbFrequency:=RegisterConversionFamily('Frequency');
  fuRadPerSec:=RegisterConversionType(cbFrequency,'rad/s',1);
  RegisterConversionType(cbFrequency,'Hz',2*pi);
  RegisterConversionType(cbFrequency,'Гц',2*pi);
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
var family: TConvFamily;
    baseType: TConvType;
    i: Integer;
begin
  if not DescriptionToConvFamily(name,family) then
    family:=RegisterConversionFamily(name);
  for i:=0 to Length(UnitNames)-1 do
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


initialization
  RegisterClasses([TBaseConvFamily,TDerivedConvFamily]);
  default_dir:=GetCurrentDir+'\data\PhysUnits\';
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
