unit VariantWrapper;

(*
  Описываем абстрактный тип Variant'a, который "оборачивает" в себя другой Variant,
  добиваясь расширенной функциональности. Например, мы ставим размерности величин,
  а сама величина - Variant, содержащий либо целое, либо действительное, либо комплексное,
  или вовсе кватернион.
  По идее, таких "оберток" может быть несколько - сначала размерность, а потом
  множестенные значения, а потом комплексное число.
  *)
interface

uses classes,TypInfo;

type

  TAbstractWrapperData = class(TPersistent) //чтобы можно было и запоминать в файле
    public
      procedure Negate; virtual; abstract; //взять обратный знак
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
  TWrapperVarData(V).Data.Free; //очевидно, и переписывания в конкр. классах не нужно
end;

procedure TAbstractWrapperVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var WrapperClass: TWrapperDataClass;  //задается в Create конкретного класса
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TWrapperVarData(Dest) do
    begin
      VType := VarType;
      WrapperClass:=TWrapperDataClass(TWrapperVarData(Source).Data.ClassType);
      Data:=WrapperClass.Create;  //нужно при создании конкретного VariantType указать WrapperClass!
      Data.Assign(TWrapperVarData(Source).Data);
    end;
end;

procedure TAbstractWrapperVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
//преобразуем другие Variant'ы в наш, это по сути "создание минимального контекста"

end;

procedure TAbstractWrapperVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
//преобразуем наш Variant в другие типы

end;

procedure TAbstractWrapperVariantType.UnaryOp(var Right: TVarData; const Operator: Integer);
begin
//унарный минус и, возможно, логическое not.
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
//сложить, вычесть, умножить, поделить, остаток от деления и битовые операции (сдвиги, лог. и пр)
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
//во что преобразовать переменную слева от бинарной операции, чтобы действие могло выполниться
  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;
  Result := True;
//слева допускаем только строку, иначе "ассимилируем"
end;

end.


