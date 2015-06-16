unit Cautious_controls;

interface

uses Cautious_Edit,StdCtrls,Controls,Classes,Messages,graphics,TypInfo;

type
  TDisablingCheckBox=class(TCheckBox)
  private
    fControlToDisable: TControl;
    procedure SetControlToDisable(value: TControl);
  public
    constructor Create(owner: TComponent); override;
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
    procedure Click; override;
  published
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
  end;

  TDisablingRadioButton=class(TRadioButton)
  private
    fControlToDisable: TControl;
    procedure CautiousChange;
    procedure SetControlToDisable(value: TControl);
    procedure BMSetCheck(var Message: TMessage); message BM_SETCHECK;
  public
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
  published
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
  end;

  TDisablingGroupBox=class(TGroupBox)
  protected
    procedure Notification(AComponent: TComponent;operation: TOperation); override;
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  end;

  TCautiousExtender=class(TLabel)
  private
    fControl1,fControl2: TControl;
    procedure SetControl1(value: TControl);
    procedure SetControl2(value: TControl);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  published
    property Control1: TControl read fcontrol1 write SetControl1;
    property Control2: TControl read fcontrol2 write SetControl2;
    property enabled;
  end;

  TIntegerEdit=class(TCautiousEdit)
  private
    fTestBtmp: TBitmap;
    fTypeData: PTypeData;
    fTypeInfo: PPTypeInfo;
    function get_value: Integer;
    procedure set_value(value: Integer);
    procedure SetTypeInfo(value: PPTypeInfo);
    function WithinBounds(value: Integer; out errmsg: string): boolean;
  public
    procedure Change; override;
    function isValid: boolean;
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    property TypeInfo: PPTypeInfo read fTypeInfo write SetTypeInfo;
  published
    property value: Integer Read get_value Write set_value stored false;
  end;

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

procedure Register;


implementation

uses float_expression_lib,sysUtils,math;

(*
            TDisablingCheckBox
                                        *)
procedure TDisablingCheckBox.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    fControlToDisable:=nil;
  inherited;
end;

procedure TDisablingCheckBox.SetControlToDisable(value: TControl);
begin
  if Assigned(fControlToDisable) then begin
    EnableControl(fControlToDisable,self);
    fControlToDisable.RemoveFreeNotification(self);
  end;
  fControlToDisable:=value;
  if Assigned(value) then begin
    value.FreeNotification(self);
    Click;
  end;
end;

procedure TDisablingCheckBox.Click;
begin
  if Assigned(fControlToDisable) then begin
    if Checked then EnableControl(fControlToDisable,self)
    else DisableControl(fControlToDisable,self);
  end;
  inherited Click;
end;

constructor TDisablingCheckBox.Create(owner: TComponent);
begin
  inherited Create(owner);
end;

(*
        TCautiousExtender
                                    *)

procedure TCautiousExtender.SetControl1(value: TControl);
begin
  if Assigned(fControl1) then begin
    EnableControl(fControl1,self);
    fControl1.RemoveFreeNotification(self);
  end;
  fControl1:=value;
  if Assigned(value) then begin
    value.FreeNotification(self);
//    Click;
  end;
end;

procedure TCautiousExtender.SetControl2(value: TControl);
begin
  if Assigned(fControl2) then begin
    EnableControl(fControl2,self);
    fControl2.RemoveFreeNotification(self);
  end;
  fControl2:=value;
  if Assigned(value) then begin
    value.FreeNotification(self);
//    Click;
  end;
end;

procedure TCautiousExtender.CMEnabledChanged(var Message: TMessage);
begin
  if enabled then begin
    if Assigned(control1) then EnableControl(control1,self);
    if Assigned(control2) then EnableControl(control2,self);
  end
  else begin
    if Assigned(control1) then DisableControl(control1,self);
    if Assigned(control2) then DisableControl(control2,self);
  end;
end;


(*
        TDisablingRadioButton
                                    *)
procedure TDisablingRadioButton.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    fControlToDisable:=nil;
  inherited;
end;

procedure TDisablingRadioButton.SetControlToDisable(value: TControl);
begin
  if Assigned(fControlToDisable) then begin
    EnableControl(fControlToDisable,self);
    fControlToDisable.RemoveFreeNotification(self);
  end;
  fControlToDisable:=value;
  if Assigned(value) then begin
    value.FreeNotification(self);
    CautiousChange;
  end;
end;

procedure TDisablingRadioButton.CautiousChange;
begin
  if Assigned(fControlToDisable) then begin
    if Checked then EnableControl(fControlToDisable,self)
    else DisableControl(fControlToDisable,self);
  end;
end;

procedure TDisablingRadioButton.BMSetCheck(var Message: TMessage);
begin
  CautiousChange;
  inherited;
end;



(*
        TDisablingGroupBox
                                  *)

procedure TDisablingGroupBox.CMEnabledChanged(var Message: TMessage);
var i: Integer;
begin
  for I:= 0 to ControlCount -1 do begin
    Controls[i].Enabled:=enabled;
  end;
  inherited;
end;

procedure TDisablingGroupBox.Notification(AComponent: TComponent; operation: TOperation);
var i: Integer;
begin

 if operation=opInsert then begin
    for I:= 0 to ControlCount -1 do begin
      if Assigned(Controls[i]) then
        Controls[i].Enabled:=enabled;
    end;
  end;
  inherited Notification(AComponent,operation);
end;


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
//    expr.SetRootComponent(ExpressionRootComponent);
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
//    expr.SetRootComponent(ExpressionRootComponent);
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
//    expr.SetRootComponent(ExpressionRootComponent);
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

function TIntegerEdit.WithinBounds(value: Integer; out errmsg: string): boolean;
resourcestring
  IntegerNotWithinRange = 'допустимые значени€ дл€ %s: %d..%d';
begin
  if Assigned(fTypeInfo) then begin
    Result:=(value>=fTypeData^.MinValue) and (value<=fTypeData^.MaxValue);
    if not Result then
      errmsg:=Format(IntegerNotWithinRange,[fTypeInfo^.Name,fTypeData^.MinValue,fTypeData^.MaxValue]);
  end
  else
    Result:=true;
end;

function TIntegerEdit.get_value: Integer;
var res: Integer;
    expr: TFloatExpression;
    errmsg: string;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
//    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then begin
      Result:=expr.getIntegerValue;
      if not WithinBounds(Result,errmsg) and not (csDesigning in ComponentState) then begin
        expr.Free;
        Raise Exception.Create(errmsg);
      end;
    end
    else begin
      Result:=0;
      if not (csDesigning in ComponentState) then begin
        errmsg:=expr.errorMsg;
        expr.Free;
        raise Exception.CreateFmt('TIntegerEdit: %s',[errMsg]);
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
    errmsg: String;
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
//    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    Result:=expr.isCorrect and WithinBounds(expr.getIntegerValue,errmsg);
    expr.Free;
  end
  else
    Result:=TryStrToInt(text,t) and WithinBounds(t,errmsg);
end;

procedure TIntegerEdit.Change;
var res: Integer;
    expr: TFloatExpression;
    errMsg: string;
resourcestring
  IntegerEditNotAnIntegerNumberMsg = 'Ќе €вл€етс€ целым числом';
begin
  if AllowExpressions then begin
    expr:=TFloatExpression.Create(nil);
//    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then
      if WithinBounds(expr.getIntegerValue,errMsg) then
       ReturnToNormal
      else
        TurnRed(errMsg)
    else TurnRed(expr.errorMsg);
    expr.Free;
  end
  else
    if TryStrToInt(text,res) then
      if WithinBounds(res,errMsg) then
        ReturnToNormal
      else
        TurnRed(errMsg)
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
  fTestBtmp:=TBitmap.Create;
  if (csDesigning in ComponentState) then value:=0;
end;

destructor TIntegerEdit.Destroy;
begin
  fTestBtmp.Free;
  inherited Destroy;
end;

procedure TIntegerEdit.SetTypeInfo(value: PPTypeInfo);
var mi,ma: Integer;
begin
  fTypeInfo:=value;
  fTypeData:=GetTypeData(value^);
  if Assigned(fTypeData) then begin
    fTestBtmp.Canvas.Font:=Font;
    mi:=fTestBtmp.Canvas.TextWidth(IntToStr(fTypeData^.MinValue));
    ma:=fTestBtmp.Canvas.TextWidth(IntToStr(fTypeData^.MaxValue));
    ClientWidth:=Max(mi,ma)+5;
  end;
end;


procedure Register;
begin
  RegisterComponents('CautiousEdit',[TDisablingCheckBox,TDisablingRadioButton,TDisablingGroupBox,
  TCautiousExtender,TIntegerEdit, TFloatEdit]);
end;

end.
