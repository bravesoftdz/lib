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
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
    constructor Create(Owner: TComponent); override;
    procedure Change; override;
  published
    { Published declarations }
    property value: Integer Read get_value Write set_value;
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

  TDisablingGroupBox=class(TGroupBox)
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
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
  if Assigned(value) then value.FreeNotification(self);
end;

procedure TFloatEdit.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    ControlToDisable:=nil;
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
    if Assigned(ControlToDisable) then DisableControl(ControlToDisable,self)
    else
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

(*
          TIntegerEdit
                              *)

procedure TIntegerEdit.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    ControlToDisable:=nil;
end;

procedure TIntegerEdit.SetControlToDisable(value: TControl);
begin
  if Assigned(fControlToDisable) then begin
    EnableControl(fControlToDisable,self);
    fControlToDisable.RemoveFreeNotification(self);
  end;
  fControlToDisable:=value;
  if Assigned(value) then value.FreeNotification(self);
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

(*
            TDisablingCheckBox
                                        *)
procedure TDisablingCheckBox.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (operation=opRemove) and (aComponent=fControlToDisable) then
    ControlToDisable:=nil;
end;

procedure TDisablingCheckBox.SetControlToDisable(value: TControl);
begin
  if Assigned(fControlToDisable) then begin
    EnableControl(fControlToDisable,self);
    fControlToDisable.RemoveFreeNotification(self);
  end;
  fControlToDisable:=value;
  if Assigned(value) then value.FreeNotification(self);
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
  RegisterComponents('CautiousEdit', [TFloatEdit,TIntegerEdit,TDisablingCheckBox,TDisablingGroupBox]);
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
