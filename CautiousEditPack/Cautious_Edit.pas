unit Cautious_Edit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls,graphics,messages,Contnrs,extctrls;

type
  TCautiousEditClass=class of TCautiousEdit;

  TCautiousEdit=class(TEdit)
  private
    fAllowExpressions: Boolean;
    fExpressionRootComponent: TComponent;
    backupHint: string;
    fSeemsNormal: boolean;
    fControlToDisable: TControl;
    fonValidateResult: TNotifyEvent;
    fonDestroy: TNotifyEvent;
    procedure SetOnValidateResult(value: TNotifyEvent);
    procedure SetControlToDisable(value:TControl);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetZeroFlag(Writer: TWriter);
    procedure LoadZeroFlag(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Change; override;
    procedure DoDestroy;
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
    procedure TurnRed(explain: string);
    procedure ReturnToNormal;
    property SeemsNormal: boolean read fSeemsNormal;
  published
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
    property OnValidateResult: TNotifyEvent read fonValidateResult write SetOnValidateResult;
    property OnDestroy: TNotifyEvent read fonDestroy write fonDestroy;
    property AllowExpressions: boolean read fAllowExpressions write fAllowExpressions default true;
    property ExpressionRootComponent: TComponent read fExpressionRootComponent write fExpressionRootComponent stored fAllowExpressions;
  end;

  TVariantEdit = class(TCautiousEdit)
  private
    function get_value: Variant;
    procedure set_value(value: Variant);
  public
    constructor Create(Owner: TComponent); override;
    procedure Change; override;
    function isValid: boolean;
  published
    property value: Variant read get_value write set_value;
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

//  TLabeledFloatEdit=class(TLabeledEdit)

procedure Register;

implementation

uses expression_lib,float_expression_lib,phys_units_lib;
(* General procedures *)

var CautiousControlList: TBucketList;

procedure EnableControl(ControlToDisable: TControl; Field: TObject);
var list: TList;
    i: Integer;
begin
  if CautiousControllist.Exists(ControlToDisable) then begin
    list:=TList(CautiousControllist[ControlToDisable]);
    i:=list.IndexOf(Field);
    if i>-1 then begin
      list.Remove(Field);
      if list.Count=0 then begin
        list.Free;
        CautiousControllist.Remove(ControlToDisable);
        ControlToDisable.Enabled:=true;
      end;
    end;
  end;
end;

procedure DisableControl(ControlToDisable: TControl; Field: TObject);
var ListOfPointers: TList;
begin
  //этот элемент может по€витьс€ впервые, и это не должно воспр. как ошибка
  if CautiousControllist.Exists(ControlToDisable) then begin
  //все есть, надо найти и добавить
    ListOfPointers:=TList(CautiousControlList[ControlToDisable]);
    if ListOfPointers.IndexOf(Field)=-1 then ListOfPointers.Add(Field);
  end
  else begin
    ListOfPointers:=TList.Create;
    ListOfPointers.Add(Field);
    CautiousControlList.Add(ControlToDisable,ListOfPointers);
    //создали новый список, дл€ данной кнопки, и добавили в него один указатель
  end;
  ControlToDisable.Enabled:=false; //очевидно же!
end;

(*
            TCautiousEdit
                                *)
constructor TCautiousEdit.Create(owner: TComponent);
begin
  inherited Create(owner);
  backupHint:=Hint;
  fSeemsNormal:=true;
  AllowExpressions:=true;
end;

destructor TCautiousEdit.Destroy;
begin
  DoDestroy;
  inherited Destroy;
end;

procedure TCautiousEdit.DoDestroy;
begin
  if Assigned(fOnDestroy) then fOnDestroy(self);
end;

procedure TCautiousEdit.SetControlToDisable(value: TControl);
begin
  if Assigned(fControlToDisable) then begin
    EnableControl(fControlToDisable,self);
    fControlToDisable.RemoveFreeNotification(self);
  end;
  fControlToDisable:=value;
  if Assigned(value) then begin
    value.FreeNotification(self);
    Change;
  end;
end;

procedure TCautiousEdit.Change;
begin
  if Assigned(OnValidateResult) and enabled then OnValidateResult(self);
end;

procedure TCautiousEdit.SetOnValidateResult(value: TNotifyEvent);
begin
  fonValidateResult:=value;
  Change;
end;

procedure TCautiousEdit.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    fControlToDisable:=nil;
  inherited;
end;

procedure TCautiousEdit.CMEnabledChanged(var Message: TMessage);
begin
  if (not enabled) and Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
  //то есть, отключенное поле ввода не должно мешать нажатию на кнопку
  if enabled then Change; //поле снова ввели в строй, проверим - не пора ль снова выругатьс€?
  inherited;
end;

procedure TCautiousEdit.TurnRed(explain: string);
begin
  if SeemsNormal then begin
    Color:=clRed;
    backupHint:=Hint;
    fSeemsNormal:=false;
  end;
  Hint:=explain;
  if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self);
end;

procedure TCautiousEdit.ReturnToNormal;
begin
  if not seemsNormal then begin
    Color:=clWhite;
    Hint:=backupHint;
    fSeemsNormal:=true;
  end;
  if Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
end;

procedure TCautiousEdit.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('IsEmpty',LoadZeroFlag,SetZeroFlag,(text='') and (csDesigning in ComponentState));
end;

procedure TCautiousEdit.SetZeroFlag(Writer: TWriter);
begin
  Writer.WriteBoolean(true);
end;

procedure TCautiousEdit.LoadZeroFlag(Reader: TReader);
begin
  if Reader.ReadBoolean then
    text:='';
end;

(*
            TVariantEdit
                                *)
function TVariantEdit.get_value: Variant;
var expr: TVariantExpression;
    E: Exception;
begin
  if AllowExpressions then begin
    expr:=TVariantExpression.Create(nil);
    try
      expr.SetRootComponent(ExpressionRootComponent);
      expr.SetString(text);
      if expr.isCorrect then
        Result:=expr.getVariantValue  //не гарантируетс€ отсутствие ошибок, тем не менее
      else begin
        Result:=0;
        if not (csDesigning in self.ComponentState) then begin
          E:=Exception.CreateFMT('TFloatLabel: %s',[expr.errorMsg]);
          FreeAndNil(expr);  //не хотим тер€ть пам€ть
          Raise E;
        end;
      end;
    finally
      expr.Free;
    end;
  end
  else
    Result:=VarWithUnitCreate(text);
end;

procedure TVariantEdit.Change;
var res: Variant;
    expr: TVariantExpression;
resourcestring
  FloatEditNotARealNumberMsg = 'Ќе €вл€етс€ действительным числом';
begin
  if AllowExpressions then begin
    expr:=TVariantExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    if expr.isCorrect then ReturnToNormal
    else TurnRed(expr.errorMsg);
    expr.Free;
  end
  else
    if TryVarWithUnitCreate(text,res) then ReturnToNormal
    else TurnRed(res);
  inherited Change;
end;

procedure TVariantEdit.set_value(value: Variant);
begin
  text:=value;
end;

constructor TVariantEdit.Create(owner: TComponent);
begin
  inherited Create(owner);
  if (csDesigning in ComponentState) then value:=0;
end;

function TVariantEdit.isValid: Boolean;
var t: Variant;
    expr: TVariantExpression;
begin
  if AllowExpressions then begin
    expr:=TVariantExpression.Create(nil);
    expr.SetRootComponent(ExpressionRootComponent);
    expr.SetString(text);
    Result:=expr.isCorrect;
    expr.Free;
  end
  else
    Result:=TryVarWithUnitCreate(text,t);
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

(* General procedures *)

procedure Register;
begin
  RegisterComponents('CautiousEdit', [TCautiousEdit,TFloatEdit,TIntegerEdit,
  TDisablingCheckBox,TDisablingRadioButton,TDisablingGroupBox,TCautiousExtender,
  TVariantEdit]);
end;

procedure free_em_all(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  TList(AData).Free;
  AContinue:=true;
end;


initialization
  CautiousControlList:=TBucketList.Create();

finalization
  CautiousControlList.ForEach(free_em_all);
  CautiousControlList.Free;

end.
