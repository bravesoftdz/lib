unit VariantWrapper;

(*
  ��������� ����������� ��� Variant'a, ������� "�����������" � ���� ������ Variant,
  ��������� ����������� ����������������. ��������, �� ������ ����������� �������,
  � ���� �������� - Variant, ���������� ���� �����, ���� ��������������, ���� �����������,
  ��� ����� ����������.
  �� ����, ����� "�������" ����� ���� ��������� - ������� �����������, � �����
  ������������ ��������, � ����� ����������� �����.
  *)
interface

uses Classes,Variants,TypInfo,
    ConvUtils;

type

  TAbstractWrapperData = class(TPersistent) //����� ����� ���� � ���������� � �����
    public
      procedure Negate; virtual; abstract; //����� �������� ����
      procedure DoAdd(value: TAbstractWrapperData); virtual; abstract;
      procedure DoSubtract(Right: TAbstractWrapperData); virtual; abstract;
      procedure DoMultiply(Right: TAbstractWrapperData); virtual; abstract;
      procedure DoDivide(Right: TAbstractWrapperData); virtual; abstract;
      function AsString: string; virtual; abstract;
  end;

  TWrapperDataClass=class of TAbstractWrapperData;

  TAbstractWrapperVariantType = class(TPublishableVariantType)
  protected
    function LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    function GetInstance(const V: TVarData): TObject; override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure UnaryOp(var Right: TVarData; const Operator: Integer); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
  end;

  TWrapperVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TAbstractWrapperData;
    Reserved4: LongInt;
  end;

  TUnitsWithExponent = class;
  TUnitsWithExponentMergeProc = function (value: TUnitsWithExponent; i,j: Integer) : Real of object;
  TShowName = function (const value: TConvType): string;
  TUnitTypes = array of TConvType;  //�� TConvType ������ ������� TConvFamily
  TExponents = array of Real;

  TUnitsWithExponent = class(TPersistent)  //������� �� � record'� � ���������� � GetMem/FreeMem,
    private
      UnitTypes: TUnitTypes;
      Exponents: TExponents;
      fMultiplier: Real;
      procedure Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
      function MergeMul(value: TUnitsWithExponent; i,j: Integer): Real;
      function MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
      function ShowSomething(proc: TShowName): string;
    public
      procedure Assign(source: TPersistent); override;
      function SameFamily(value: TUnitsWithExponent): Boolean;
      function FindMultiplier(value: TUnitsWithExponent): Real;
      function Multiply(value: TUnitsWithExponent): Real;
      function Divide(right: TUnitsWithExponent): Real;
      function AsString: string;
      function ShowFormula: string;
  end;
//���� ��� ����� - ���������� ���, ��� ������������� ����������� ������
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      Units: TUnitsWithExponent;
    protected
      instance: Variant; //�� ����������, ������� �� �����������
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(source: TPersistent); override;
      procedure Negate; override; //����� �������� ����
      procedure DoAdd(value: TAbstractWrapperData); override;
      procedure DoSubtract(Right: TAbstractWrapperData); override;
      procedure DoMultiply(Right: TAbstractWrapperData); override;
      procedure DoDivide(Right: TAbstractWrapperData); override;
  end;



implementation

uses SysUtils,streamable_conv_units,math,phys_units_lib;
(*
    TAbstractWrapperVariantType
                                  *)
function TAbstractWrapperVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result:=TWrapperVarData(V).Data;
end;

procedure TAbstractWrapperVariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  TWrapperVarData(V).Data.Free; //��������, � ������������� � �����. ������� �� �����
end;

procedure TAbstractWrapperVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var WrapperClass: TWrapperDataClass;  //�������� � Create ����������� ������
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TWrapperVarData(Dest) do
    begin
      VType := VarType;
      WrapperClass:=TWrapperDataClass(TWrapperVarData(Source).Data.ClassType);
      Data:=WrapperClass.Create;  //����� ��� �������� ����������� VariantType ������� WrapperClass!
      Data.Assign(TWrapperVarData(Source).Data);
    end;
end;

procedure TAbstractWrapperVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
//����������� ������ Variant'� � ���, ��� �� ���� "�������� ������������ ���������"

end;

procedure TAbstractWrapperVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
//����������� ��� Variant � ������ ����

end;

procedure TAbstractWrapperVariantType.UnaryOp(var Right: TVarData; const Operator: Integer);
begin
//������� ����� �, ��������, ���������� not.
  if Right.vtype=VarType then
    if Operator=opNegate then
      TWrapperVarData(Right).Data.Negate
    else
      RaiseInvalidOp
  else
    RaiseInvalidOp;
end;

procedure TAbstractWrapperVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
begin
//�������, �������, ��������, ��������, ������� �� ������� � ������� �������� (������, ���. � ��)
  if Right.VType = VarType then
    case Left.VType of
      varString:
        case Operator of
          opAdd:
            Variant(Left) := Variant(Left) + TWrapperVarData(Right).Data.AsString;
        else
          RaiseInvalidOp;
        end;
    else
      if Left.VType = VarType then
        case Operator of
          opAdd:
            TWrapperVarData(Left).data.DoAdd(TWrapperVarData(Right).Data);
          opSubtract:
            TWrapperVarData(Left).data.DoSubtract(TWrapperVarData(Right).Data);
          opMultiply:
            TWrapperVarData(Left).data.DoMultiply(TWrapperVarData(Right).Data);
          opDivide:
            TWrapperVarData(Left).data.DoDivide(TWrapperVarData(Right).Data);
        else
          RaiseInvalidOp;
        end
      else
        RaiseInvalidOp;
    end
  else
    RaiseInvalidOp;
end;

function TAbstractWrapperVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
//�� ��� ������������� ���������� ����� �� �������� ��������, ����� �������� ����� �����������
  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;
  Result := True;
//����� ��������� ������ ������, ����� "������������"
end;

(*
      TUnitWithPower
                        *)
procedure TUnitsWithExponent.Assign(source: TPersistent);
var s: TUnitsWithExponent absolute source;
begin
  if source is TUnitsWithExponent then begin
    Exponents:=Copy(s.Exponents);
    UnitTypes:=Copy(s.UnitTypes);
  end
  else inherited Assign(source);
end;

function TUnitsWithExponent.SameFamily(value: TUnitsWithExponent): Boolean;
var i: Integer;
begin
  if Length(Exponents)=Length(value.Exponents) then begin
    Result:=true;
    for i:=0 to Length(Exponents)-1 do
      if (ConvTypeToFamily(UnitTypes[i])<>ConvTypeToFamily(value.UnitTypes[i])) or (Exponents[i]<>value.Exponents[i]) then begin
        Result:=false;
        Exit;
      end;
  end
  else Result:=false;
end;

function TUnitsWithExponent.FindMultiplier(value: TUnitsWithExponent): Real;
var i: Integer;
begin
  //�� ��� �����, ��� ��������� ���� � �� ��
  Result:=1;
  for i:=0 to Length(Exponents)-1 do
    if (UnitTypes[i]<>value.UnitTypes[i]) then
      Result:=Result*Convert(1,value.UnitTypes[i],UnitTypes[i]);
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
  L1:=Length(Exponents);
  L2:=Length(value.Exponents);
  SetLength(ResultUnits,L1+L2); //��������� ��������
  while (i<Length(Exponents)) and (j<Length(value.Exponents)) do begin
    first:=ConvTypeToFamily(UnitTypes[i]);
    second:=ConvTypeToFamily(value.UnitTypes[j]);
    if first=second then begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=proc(value,i,j);
      inc(i);
      inc(j);
      inc(k);
    end
    else if UnitTypes[i]>value.UnitTypes[j] then begin
      //UnitTypes[j] �� ���� � ���. ������ - ���� �������� �� ����. �����
      ResultUnits[k]:=value.UnitTypes[j];
      ResultExponents[k]:=value.Exponents[j];
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
  SetLength(ResultUnits,k);
  SetLength(ResultExponents,k);
  UnitTypes:=Copy(ResultUnits);
  Exponents:=Copy(ResultExponents);
end;

function TUnitsWithExponent.MergeMul(value: TUnitsWithExponent; i,j: Integer): Real;
begin
  if UnitTypes[i]<>value.UnitTypes[j] then
    fMultiplier:=fMultiplier*Power(Convert(1,value.UnitTypes[j],UnitTypes[i]),value.Exponents[j]);
  Result:=Exponents[i]+value.exponents[j];
end;

function TUnitsWithExponent.MergeDiv(value: TUnitsWithExponent; i,j: Integer): Real;
begin
  if UnitTypes[i]<>value.UnitTypes[j] then
    fMultiplier:=fMultiplier*Power(Convert(1,value.UnitTypes[j],UnitTypes[i]),-value.Exponents[j]);
  Result:=Exponents[i]-value.exponents[j];
end;

function TUnitsWithExponent.Multiply(value: TUnitsWithExponent): Real;
begin
  fMultiplier:=1;
  Merge(value,MergeMul);
  Result:=fMultiplier;
end;

function TUnitsWithExponent.Divide(right: TUnitsWithExponent): Real;
begin
  fMultiplier:=1;
  Merge(right,MergeDiv);
  Result:=fMultiplier;
end;

function TUnitsWithExponent.ShowSomething(proc: TShowName): string;
var i: Integer;
begin
  Result:='';
  for i:=0 to Length(Exponents)-1 do begin
    if Exponents[i]=1 then
      Result:=Result+proc(UnitTypes[i])
    else begin
      Result:=Result+'('+proc(UnitTypes[i])+')^';
      if Exponents[i]<0 then
        Result:=Result+'('+FloatToStr(Exponents[i])+')'
      else
        Result:=Result+FloatToStr(Exponents[i]);
    end;
    if i<Length(Exponents)-1 then Result:=Result+'*';
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

(*
      TVariantWithUnit
                          *)
constructor TVariantWithUnit.Create;
begin
  units:=TUnitsWithExponent.Create;
end;

destructor TVariantWithUnit.Destroy;
begin
  units.Free;
  inherited Destroy;
end;

procedure TVariantWithUnit.Assign(source: TPersistent);
var s: TVariantWithUnit absolute source;
begin
  if source is TVariantWithUnit then begin
    instance:=s.instance; //variant'� - ��� �����, �����������
    units.Assign(s.Units);
  end
  else inherited Assign(source);
end;

procedure TVariantWithUnit.Negate;
begin
  //�� ����������� �� ������ ������
  instance:=-instance;
end;

procedure TVariantWithUnit.DoAdd(value: TAbstractWrapperData);
var v: TVariantWithUnit absolute value;
begin
  if value is TVariantWithUnit then begin
    //����������� ������ ������ ���������
    //������, ����� ������� - ������� ������� ���������������� �� TConvType    
    if units.SameFamily(v.Units) then
      instance:=instance+v.instance*units.FindMultiplier(v.Units)
    else
      Raise Exception.CreateFMT('TVariantWithUnit.DoAdd: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited; //�� ���� ����, ���������� �� ��� ������� ���� ���?
end;

procedure TVariantWithUnit.DoSubtract(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then begin
    if units.SameFamily(v.Units) then
      instance:=instance-v.instance*units.FindMultiplier(v.Units)
    else
      Raise Exception.CreateFmt('TVariantWithUnit.DoSub: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited;
end;

procedure TVariantWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
    instance:=instance*v.instance*units.Multiply(v.Units)
  else inherited;
end;

procedure TVariantWithUnit.DoDivide(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then
    instance:=instance/v.instance*units.Divide(v.Units)
  else inherited;
end;




end.


