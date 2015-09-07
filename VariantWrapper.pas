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

uses classes,TypInfo;

type

  TAbstractWrapperData = class(TPersistent) //����� ����� ���� � ���������� � �����
    public
      procedure Negate; virtual; abstract; //����� �������� ����
      procedure DoAdd(value: TAbstractWrapperData); virtual; abstract;
      procedure DoSubtract(Right: TAbstractWrapperData); virtual;
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

implementation

(*
    TAbstractWrapperData
                              *)
procedure TAbstractWrapperData.DoSubtract(Right: TAbstractWrapperData);
begin
  Right.Negate;
  DoAdd(Right);
  Right.Negate; //�������, ��� ����. �� ����� �������, ����� �� ����� �������,
  //�� ����� ����� ����� ��������� ���� TAbstractWrapperData
end;

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
var CastedRight: TVarData;
begin
//�������, �������, ��������, ��������, ������� �� ������� � ������� �������� (������, ���. � ��)
  if Right.vType<>VarType then
    Cast(CastedRight,Right)
  else
    CastedRight:=Right;
    
    case Left.VType of
      varString:
        case Operator of
          opAdd:
            Variant(Left) := Variant(Left) + TWrapperVarData(CastedRight).Data.AsString;
        else
          RaiseInvalidOp;
        end;
    else
      if Left.VType = VarType then
        case Operator of
          opAdd:
            TWrapperVarData(Left).data.DoAdd(TWrapperVarData(CastedRight).Data);
          opSubtract:
            TWrapperVarData(Left).data.DoSubtract(TWrapperVarData(CastedRight).Data);
          opMultiply:
            TWrapperVarData(Left).data.DoMultiply(TWrapperVarData(CastedRight).Data);
          opDivide:
            TWrapperVarData(Left).data.DoDivide(TWrapperVarData(CastedRight).Data);
        else
          RaiseInvalidOp;
        end
      else
        RaiseInvalidOp;
    end;
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

end.


