unit Cautious_Edit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls,graphics,messages,Contnrs;

type
  TFloatEdit = class(TEdit)
  private
    { Private declarations }
    fControlToDisable: TControl;
    function get_value: Real;
    procedure set_value(value: Real);
    procedure SetControlToDisable(value:TControl);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
    constructor Create(Owner: TComponent); override;
    procedure Change; override;
  published
    { Published declarations }
    property value: Real Read get_value Write set_value;
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
  end;

  TIntegerEdit=class(TEdit)
  private
    { Private declarations }
    fControlToDisable: TControl;
    function get_value: Integer;
    procedure set_value(value: Integer);
    procedure SetControlToDisable(value:TControl);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
    constructor Create(Owner: TComponent); override;
    procedure Change; override;
    function isValid: boolean;
  published
    { Published declarations }
    property value: Integer Read get_value Write set_value stored false;
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
  end;

  TDisablingCheckBox=class(TCheckBox)
  private
    fControlToDisable: TControl;
    procedure SetControlToDisable(value: TControl);
  public
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

//    procedure Click; override;
  published
    property ControlToDisable: TControl read fControlToDisable write SetControlToDisable;
  end;

  TDisablingGroupBox=class(TGroupBox)
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


procedure Register;

implementation

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
  //этот элемент может появиться впервые, и это не должно воспр. как ошибка
  if CautiousControllist.Exists(ControlToDisable) then begin
  //все есть, надо найти и добавить
    ListOfPointers:=TList(CautiousControlList[ControlToDisable]);
    if ListOfPointers.IndexOf(Field)=-1 then ListOfPointers.Add(Field);
  end
  else begin
    ListOfPointers:=TList.Create;
    ListOfPointers.Add(Field);
    CautiousControlList.Add(ControlToDisable,ListOfPointers);
    //создали новый список, для данной кнопки, и добавили в него один указатель
  end;
  ControlToDisable.Enabled:=false; //очевидно же!
end;

(*
            TFloatEdit
                                *)

procedure TFloatEdit.SetControlToDisable(value: TControl);
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

procedure TFloatEdit.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (aComponent.name='button1') then ControlToDisable:=nil;
  if (name='button1') then ControlToDisable:=nil;
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    fControlToDisable:=nil;
  //    ControlToDisable:=nil;
  inherited;
end;

function TFloatEdit.get_value: Real;
var res: Extended;
begin
  if TryStrToFloat(text,res) then begin
    Result:=res;
    Color:=clWhite;
    if Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
    end
  else begin
    Result:=0;
    Color:=clRed;
    if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self);
    if not (csDesigning in self.ComponentState) then
        Raise Exception.Create('TFloatLabel: Not a number');
  end;
end;

procedure TFloatEdit.Change;
var res: Extended;
begin
  if TryStrToFloat(text,res) then begin
    Color:=clWhite;
    if Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
    end
  else begin
    Color:=clRed;
    if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self);
  end;
  inherited Change;
end;

procedure TFloatEdit.set_value(value: Real);
begin
  text:=FloatToStr(value);
end;

constructor TFloatEdit.Create;
begin
  inherited;
  value:=0;
end;

procedure TFloatEdit.CMEnabledChanged(var Message: TMessage);
begin
  if (not enabled) and Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
  //то есть, отключенное поле ввода не должно мешать нажатию на кнопку
  if enabled then Change; //поле снова ввели в строй, проверим - не пора ль снова выругаться?
  inherited;
end;

(*
          TIntegerEdit
                              *)

procedure TIntegerEdit.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    fControlToDisable:=nil;
  inherited;
end;

procedure TIntegerEdit.SetControlToDisable(value: TControl);
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

function TIntegerEdit.get_value: Integer;
var res: Integer;
begin
  if TryStrToInt(text,res) then begin
    Result:=res;
    Color:=clWhite;
    if Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
    end
  else begin
    Result:=0;
    Color:=clRed;
    if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self)
    else
      if not (csDesigning in self.ComponentState) then
        Raise Exception.Create('TIntegerEdit: Not a number');
  end;
end;

function TIntegerEdit.isValid: boolean;
var t: Integer;
begin
  Result:=TryStrToInt(text,t);
end;

procedure TIntegerEdit.Change;
var res: Integer;
begin
  if TryStrToInt(text,res) then begin
    Color:=clWhite;
    if Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
    end
  else begin
    Color:=clRed;
    if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self);
  end;
  inherited Change;
end;

procedure TIntegerEdit.set_value(value: Integer);
begin
  text:=IntToStr(value);
end;

constructor TIntegerEdit.Create;
begin
  inherited;
  value:=0;
end;

procedure TIntegerEdit.CMEnabledChanged(var Message: TMessage);
begin
  if (not enabled) and Assigned(ControlToDisable) then EnableControl(ControlToDisable,self);
  //то есть, отключенное поле ввода не должно мешать нажатию на кнопку
  if enabled then Change; //поле снова ввели в строй, проверим - не пора ль снова выругаться?
  inherited;
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
  if (aComponent.name='button1') then ControlToDisable:=nil;
  if (name='button1') then ControlToDisable:=nil;
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
    if (Controls[i] is TControl) then (Controls[i] as TControl).Enabled:=enabled;
  end;
  inherited;
end;

(* General procedures *)

procedure Register;
begin
  RegisterComponents('CautiousEdit', [TFloatEdit,TIntegerEdit,TDisablingCheckBox,TDisablingRadioButton,TDisablingGroupBox,TCautiousExtender]);
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
