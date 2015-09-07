unit FloatEdit;

interface

uses classes,Cautious_edit;

type

  TFloatEdit = class(TCautiousEdit)
  private
    function get_value: Real;
    procedure set_value(value: Real);
  public
    constructor Create(owner: TComponent); override;
    procedure Change; override;
    function isValid: boolean;
  published
    property value: Real Read get_value Write set_value;
  end;

  TIntegerEdit=class(TCautiousEdit)
  private
    function get_value: Integer;
    procedure set_value(value: Integer);
  public
    procedure Change; override;
    function isValid: boolean;
    constructor Create(owner: TComponent); override;
  published
    property value: Integer Read get_value Write set_value stored false;
  end;



procedure Register;

implementation

uses sysUtils,float_expression_lib;

(*
            TFloatEdit
                                *)

function TFloatEdit.get_value: Real;
var res: Extended;
    expr: TFloatExpression;
    E: Exception;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then Result:=expr.getValue
    else begin
      Result:=0;
      if not (csDesigning in self.ComponentState) then begin
        E:=Exception.CreateFMT('TFloatLabel: %s',[expr.errorMsg]);
        expr.Free;  //не хотим тер€ть пам€ть
        Raise E;
      end;
    end;
    expr.Free;
  end
  else
    if TryStrToFloat(text,res) then begin
      Result:=res;
    end
    else begin
      Result:=0;
      if not (csDesigning in self.ComponentState) then
        Raise Exception.Create('TFloatLabel: Not a number');
    end;
end;

procedure TFloatEdit.Change;
var res: Extended;
    expr: TFloatExpression;
resourcestring
  FloatEditNotARealNumberMsg = 'Ќе €вл€етс€ действительным числом';
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then ReturnToNormal
    else TurnRed(expr.errorMsg);
    expr.Free;
  end
  else
    if TryStrToFloat(text,res) then ReturnToNormal
    else TurnRed(FloatEditNotARealNumberMsg);

  inherited Change;
end;

procedure TFloatEdit.set_value(value: Real);
begin
  text:=FloatToStr(value);
end;

constructor TFloatEdit.Create(owner: TComponent);
begin
  inherited Create(owner);
  if (csDesigning in ComponentState) then value:=0;
end;

function TFloatEdit.isValid: Boolean;
var t: Extended;
    expr: TFloatExpression;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    Result:=expr.isCorrect;
    expr.Free;
  end
  else
    Result:=TryStrToFloat(text,t);
end;

(*
          TIntegerEdit
                              *)

function TIntegerEdit.get_value: Integer;
var res: Integer;
    expr: TFloatExpression;
    E: Exception;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then Result:=expr.getIntegerValue
    else begin
      Result:=0;
      if not (csDesigning in ComponentState) then begin
        E:=Exception.CreateFmt('TIntegerEdit: %s',[expr.errorMsg]);
        expr.Free;
        raise E;
      end;
    end;
    expr.Free;
  end
  else
    if TryStrToInt(text,res) then begin
      Result:=res;
      end
    else begin
      Result:=0;
      if not (csDesigning in self.ComponentState) then
        Raise Exception.Create('TIntegerEdit: Not a number');
    end;
end;

function TIntegerEdit.isValid: boolean;
var t: Integer;
    expr: TFloatExpression;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    Result:=expr.isCorrect;
    expr.Free;
  end
  else
    Result:=TryStrToInt(text,t);
end;

procedure TIntegerEdit.Change;
var res: Integer;
    expr: TFloatExpression;
resourcestring
  IntegerEditNotAnIntegerNumberMsg = 'Ќе €вл€етс€ целым числом';
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then ReturnToNormal
    else TurnRed(expr.errorMsg);
    expr.Free;
  end
  else
    if TryStrToInt(text,res) then ReturnToNormal
    else TurnRed(IntegerEditNotAnIntegerNumberMsg);
  inherited Change;
end;

procedure TIntegerEdit.set_value(value: Integer);
begin
  text:=IntToStr(value);
end;

constructor TIntegerEdit.Create(owner: TComponent);
begin
  inherited Create(owner);
  if (csDesigning in ComponentState) then value:=0;
end;

procedure Register;
begin
  RegisterComponents('CautiousEdit', [TFloatEdit,TIntegerEdit]);
end;

end.
