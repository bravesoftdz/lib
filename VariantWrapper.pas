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
    protected
      instance: Variant; //�� ����������, ������� �� �����������
    public
      procedure Negate; virtual; abstract; //����� �������� ����
      procedure DoAdd(value: TAbstractWrapperData); virtual; abstract;
      procedure DoSubtract(Right: TAbstractWrapperData); virtual; abstract;
      procedure DoMultiply(Right: TAbstractWrapperData); virtual; abstract;
      procedure DoDivide(Right: TAbstractWrapperData); virtual; abstract;
  end;

  TWrapperDataClass=class of TAbstractWrapperData;

  TAbstractWrapperVariantType = class(TPublishableVariantType)
  protected
    function RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    function LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
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

  TUnitsWithExponentMergeProc = function (exp1,exp2: Real) : Real;
  TUnitTypes = array of TConvFamily;
  TExponents = array of Real;

  TUnitsWithExponent = class(TPersistent)  //������� �� � record'� � ���������� � GetMem/FreeMem,
    private
      UnitTypes: TUnitTypes;
      Exponents: TExponents;
      procedure Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
    public
      procedure Assign(source: TPersistent); override;
      function IsEqual(value: TUnitsWithExponent): Boolean;
      procedure Multiply(value: TUnitsWithExponent);
      procedure Divide(right: TUnitsWithExponent);
      function AsString: string;
  end;
//���� ��� ����� - ���������� ���, ��� ������������� ����������� ������
  TVariantWithUnit=class(TAbstractWrapperData)
    private
      Units: TUnitsWithExponent;
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

uses SysUtils,streamable_conv_units;
(*
    TAbstractWrapperVariantType
                                  *)
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

end;

procedure TAbstractWrapperVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
begin
//�������, �������, ��������, ��������, ������� �� ������� � ������� �������� (������, ���. � ��)

end;

function TAbstractWrapperVariantType.RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
//�� ��� ������������� ���������� ������ �� �������� ��������, ����� �������� ����� �����������

end;

function TAbstractWrapperVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
//�� ��� ������������� ���������� ����� �� �������� ��������, ����� �������� ����� �����������

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

function TUnitsWithExponent.IsEqual(value: TUnitsWithExponent): Boolean;
var i: Integer;
begin
  if Length(Exponents)=Length(value.Exponents) then begin
    Result:=true;
    for i:=0 to Length(Exponents)-1 do
      if (UnitTypes[i]<>value.UnitTypes[i]) or (Exponents[i]<>value.Exponents[i]) then begin
        Result:=false;
        Exit;
      end;
  end
  else Result:=false;
end;

procedure TUnitsWithExponent.Merge(value: TUnitsWithExponent; proc: TUnitsWithExponentMergeProc);
var i,j,k: Integer;
    L1,L2: Integer;
    ResultUnits: TUnitTypes;
    ResultExponents: TExponents;
begin
  //��� ������ ������������� �� ���������� unitTypes
  //�� ����, ������������ �������
  i:=0;
  j:=0;
  k:=0;
  L1:=Length(Exponents);
  L2:=Length(value.Exponents);
  SetLength(ResultUnits,L1+L2); //��������� ��������
  while (i<Length(Exponents)) and (j<Length(value.Exponents)) do begin
    if UnitTypes[i]=value.UnitTypes[j] then begin
      ResultUnits[k]:=UnitTypes[i];
      ResultExponents[k]:=proc(Exponents[i],value.Exponents[j]);
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

function UnitsWithExponentMultiply(x1,x2: Real): Real;
begin
  Result:=x1*x2;
end;

function UnitsWithExponentDivide(x1,x2: Real): Real;
begin
  Result:=x1/x2;
end;

procedure TUnitsWithExponent.Multiply(value: TUnitsWithExponent);
begin
  Merge(value,UnitsWithExponentMultiply);
end;

procedure TUnitsWithExponent.Divide(right: TUnitsWithExponent);
begin
  Merge(right,UnitsWithExponentDivide);
end;

function TUnitsWithExponent.AsString: string;
var i: Integer;
begin
  Result:='';
  for i:=0 to Length(Exponents)-1 do begin
    if Exponents[i]=1 then
      Result:=Result+PreferredUnits.GetPreferredUnitName(UnitTypes[i])
    else begin
      Result:=Result+'('+PreferredUnits.GetPreferredUnitName(UnitTypes[i])+')^';
      if Exponents[i]<0 then
        Result:=Result+'('+FloatToStr(Exponents[i])+')'
      else
        Result:=Result+FloatToStr(Exponents[i]);
    end;
    if i<Length(Exponents)-1 then Result:=Result+'*';
  end;
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
    if units.IsEqual(v.Units) then
      instance:=instance+v.instance
    else
      Raise Exception.CreateFMT('TVariantWithUnit.DoAdd: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited; //�� ���� ����, ���������� �� ��� ������� ���� ���?
end;

procedure TVariantWithUnit.DoSubtract(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then begin
    if units.IsEqual(v.Units) then
      instance:=instance-v.instance
    else
      Raise Exception.CreateFmt('TVariantWithUnit.DoSub: units don''t match, %s and %s',[units.AsString,v.Units.AsString]);
  end
  else inherited;
end;

procedure TVariantWithUnit.DoMultiply(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then begin
    instance:=instance*v.instance;
    units.Multiply(v.Units);
  end
  else inherited;
end;

procedure TVariantWithUnit.DoDivide(Right: TAbstractWrapperData);
var v: TVariantWithUnit absolute Right;
begin
  if Right is TVariantWithUnit then begin
    instance:=instance/v.instance;
    units.Divide(v.Units);
  end
  else inherited;
end;




end.


