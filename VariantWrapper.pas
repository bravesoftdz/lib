unit VariantWrapper;

(*
  ��������� ����������� ��� Variant'a, ������� "�����������" � ���� ������ Variant,
  ��������� ����������� ����������������. ��������, �� ������ ����������� �������,
  � ���� �������� - Variant, ���������� ���� �����, ���� ��������������, ���� �����������,
  ��� ����� ����������.
  �� ����, ����� "�������" ����� ���� ��������� - ������� �����������, � �����
  ������������ ��������, � ����� ����������� �����.

  ����� ��������� ����� ������� ����� ���� �� Variants, ������������ CodeGen'��, �����
  ����� ���� ������������ ��� ��������������, ��������, ������
  a:=a+b (2 �����������, 2 �������� � ���� �������. ������) ��������
  _varAdd(a,b) ��� ������ ����������� ������� ���. �������

  *)
interface

uses classes,TypInfo,variants;

type

  TAbstractWrapperData = class(TPersistent) //����� ����� ���� � ���������� � �����
    public
      procedure Release; virtual; //��������� � ��� - ������ ����
                                          //����������� ������� create/free
      class function GetInstance: TAbstractWrapperData; virtual; //������ Create
      procedure DoNegate; virtual; abstract; //����� �������� ����
      procedure DoAdd(const Right: TAbstractWrapperData); virtual; abstract;
      procedure DoSubtract(const Right: TAbstractWrapperData); virtual;
      procedure DoMultiply(const Right: TAbstractWrapperData); virtual; abstract;
      procedure DoDivide(const Right: TAbstractWrapperData); virtual; abstract;
      function GetAsString: string; virtual; abstract;
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

  THackedCustomVariantType = class(TCustomVariantType); //����� ���������� �
                                          //LeftPromotion/RightPromotion



  TWrapperVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TAbstractWrapperData;
    Reserved4: LongInt;
  end;

  TVarOpProcedure = procedure(var Left: TVarData; const Right: TVarData; const OpCode: TVarOp);

  //������ ��� ������������ ����� � custom variant - ��� �������� �� ����...
  procedure _MyVarOp(var Left: TVarData; const Right: TVarData; const OpCode: TVarOp);

var numberOfGuesses: Integer;
    VarOpProc: TVarOpProcedure;

implementation
uses sysUtils;

//const
(*
    TAbstractWrapperData
                              *)
procedure TAbstractWrapperData.Release;
begin
  Destroy;
end;

class function TAbstractWrapperData.getInstance: TAbstractWrapperData;
begin
  Result:=Create;
end;

procedure TAbstractWrapperData.DoSubtract(const Right: TAbstractWrapperData);
begin
  Right.DoNegate;
  DoAdd(Right);
  Right.DoNegate; //�������, ��� ����. �� ����� �������, ����� �� ����� �������,
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
//  inc(NumberOfGuesses);
  V.VType:=varEmpty;
//  FreeAndNil(TWrapperVarData(V).Data);
//  TWrapperVarData(V).Data.Free; //��������, � ������������� � �����. ������� �� �����
  TWrapperVarData(V).Data.Release;
end;

procedure TAbstractWrapperVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var WrapperClass: TWrapperDataClass;  //�������� � Create ����������� ������
begin
//  if Indirect and VarDataIsByRef(Source) then
//    VarDataCopyNoInd(Dest, Source)
//  else
// ��� custom variant'� ������� �� ����� �������� �� ������
//  inc(numberOfGuesses);
    with TWrapperVarData(Dest) do
    begin
      VType := VarType;
      WrapperClass:=TWrapperDataClass(TWrapperVarData(Source).Data.ClassType);
      Data:=WrapperClass.GetInstance;  //����� ��� �������� ����������� VariantType ������� WrapperClass!
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
      TWrapperVarData(Right).Data.DoNegate
    else
      RaiseInvalidOp
  else
    RaiseInvalidOp;
end;

procedure TAbstractWrapperVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
//var CastedRight: TVarData;
begin
//�������, �������, ��������, ��������, ������� �� ������� � ������� �������� (������, ���. � ��)
(*
  if Right.vType<>VarType then
    Cast(CastedRight,Right)
  else
    CastedRight:=Right;
    *)
//  try
    case Left.VType of
      varString:
        case Operator of
          opAdd:
            Variant(Left) := Variant(Left) + TWrapperVarData(Right).Data.GetAsString;
//            Variant(Left) := Variant(Left) + TWrapperVarData(CastedRight).Data.GetAsString;
        else
          RaiseInvalidOp;
        end;
    else
      if Left.VType = VarType then
        case Operator of
          opAdd:
//            TWrapperVarData(Left).data.DoAdd(TWrapperVarData(CastedRight).Data);
            TWrapperVarData(Left).data.DoAdd(TWrapperVarData(Right).Data);
          opSubtract:
//            TWrapperVarData(Left).data.DoSubtract(TWrapperVarData(CastedRight).Data);
            TWrapperVarData(Left).data.DoSubtract(TWrapperVarData(Right).Data);
          opMultiply:
//            TWrapperVarData(Left).data.DoMultiply(TWrapperVarData(CastedRight).Data);
            TWrapperVarData(Left).data.DoMultiply(TWrapperVarData(Right).Data);
          opDivide:
            TWrapperVarData(Left).data.DoDivide(TWrapperVarData(Right).Data);
//            TWrapperVarData(Left).data.DoDivide(TWrapperVarData(CastedRight).Data);
        else
          RaiseInvalidOp;
        end
      else
        RaiseInvalidOp;
    end;
//    finally
//      if Right.VType<>VarType then
//        VarDataClear(CastedRight);  //��������� �� ���������� � ��� �� ��� � Variant'��,
//      //������� ������ �� ��������, ���� ������� ��� �������!
//    end;
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
    Variants codegen functions
                                  *)

procedure _MyVarOp(var Left: TVarData; const Right: TVarData; const OpCode: TVarOp);
begin
//  call Variants._VarOp;
end;

(*
procedure VarOpRare(var Left: TVarData; const Right: TVarData; const OpCode: TVarOp);
var
  LNewLeftType, LNewRightType: TVarType;
  LLeftHandler, LRightHandler: TCustomVariantType;
  LLeftHacked: THackedCustomVariantType absolute LLeftHandler;
  LRightHacked: THackedCustomVariantType absolute LRightHandler;
  LTemp: TVarData;
begin
  // simple and ???

  if not FindCustomVariantType(Left.VType,LLeftHandler) then
  begin
    // simple and custom but the custom doesn't really exist (nasty but possible )
    if not FindCustomVariantType(Right.VType, LRightHandler) then
      VarInvalidOp

    // does the custom want to take over?
    else if LRightHacked.LeftPromotion(Left, OpCode, LNewLeftType) then
    begin

      // convert the left side
      if Left.VType <> LNewLeftType then
      begin
        LRightHacked.VarDataInit(LTemp);
//        VariantInit(LTemp);
        try
          LRightHacked.VarDataCastTo(LTemp,Left,LNewLeftType);
//         _VarCast(LTemp, Left, LNewLeftType);
          LRightHacked.VarDataCopy(Left,LTemp);
//          _VarCopy(Left, LTemp);
          if Left.VType <> LNewLeftType then
            VarCastError;
        finally
          LRightHacked.VarDataClear(LTemp);
//          _VarClear(LTemp);
        end;
      end;
      LRightHandler.BinaryOp(Left, Right, OpCode);
    end

    // simple then converts custom then
    else
    begin
      LRightHacked.VarDataInit(LTemp);
//      VariantInit(LTemp);
      try
        // convert the right side to the left side's type
        LRightHacked.VarDataCastTo(LTemp,Right,Left.VType);
//        _VarCast(LTemp, Right, Left.VType);
        if LTemp.VType <> Left.VType then
          VarCastError;
        _VarOp(Left, LTemp, OpCode);
      finally
        LRightHacked.VarDataClear(LTemp);
//        _VarClear(LTemp);
      end;
    end;
  end

  // custom and something else
  else
  begin
    // does the left side like what is in the right side?
    if LLeftHandler.RightPromotion(Right, OpCode, LNewRightType) then
    begin

      // make the right side right
      if Right.VType <> LNewRightType then
      begin
        VariantInit(LTemp);
        try
          _VarCast(LTemp, Right, LNewRightType);
          if LTemp.VType <> LNewRightType then
            VarCastError;
          LLeftHandler.BinaryOp(Left, LTemp, OpCode);
        finally
          _VarClear(LTemp);
        end;
      end

      // type is correct so lets go!
      else
        LLeftHandler.BinaryOp(Left, Right, OpCode);
    end

    // custom and simple and the right one can't convert the simple
    else if (Right.VType and varTypeMask) < CFirstUserType then
    begin

      // convert the left side to the right side's type
      if Left.VType <> Right.VType then
      begin
        VariantInit(LTemp);
        try
          _VarCast(LTemp, Left, Right.VType);
          _VarCopy(Left, LTemp);
          if Left.VType <> Right.VType then
            VarCastError;
        finally
          _VarClear(LTemp);
        end;
      end;
      _VarOp(Left, Right, OpCode);
    end

    // custom and custom but the right one doesn't really exist (nasty but possible )
    else if not FindCustomVariantType(Right.VType, LRightHandler) then
      VarInvalidOp

    // custom and custom and the right one can handle the left's type
    else if LRightHandler.LeftPromotion(Left, OpCode, LNewLeftType) then
    begin

      // convert the left side
      if Left.VType <> LNewLeftType then
      begin
        VariantInit(LTemp);
        try
          _VarCast(LTemp, Left, LNewLeftType);
          _VarCopy(Left, LTemp);
          if Left.VType <> LNewLeftType then
            VarCastError;
        finally
          _VarClear(LTemp);
        end;
      end;
      LRightHandler.BinaryOp(Left, Right, OpCode);
    end

    // custom and custom but neither type can deal with each other
    else
      VarInvalidOp;
  end;

end;

procedure _VarAdd(var Left: TVarData; const Right: TVarData);
var
  LLeftType, LRightType: TVarType;
  LLeftHandler, LRightHandler: TCustomVariantType;
begin
  if (Left.VType=varDouble) and (Right.VType=varDouble) then
    Left.VDouble:=Left.VDouble+Right.VDouble
  else VarOpRare(Left,Right,opAdd);
//  else Variant(Left):=Variant(Left)+Variant(Right);
end;
*)

initialization
  VarOpProc:=Pointer($00417238);


end.


