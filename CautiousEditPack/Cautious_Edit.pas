unit Cautious_Edit;

interface

uses
  Classes, Controls,stdCtrls,messages,TypInfo,expression_lib;

type
  TCautiousEditClass=class of TCautiousEdit;

  TCautiousEdit=class(TEdit)
  private
    fAllowExpressions: Boolean;
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
  end;

  TVariantEdit = class(TCautiousEdit)
  private
    fExpression: TVariantExpression;
    function get_value: Variant;
    procedure set_value(value: Variant);
  public
    constructor Create(Owner: TComponent); override;
    procedure Change; override;
    function isValid: boolean;
    property Expression: TVariantExpression read fExpression;
  published
    property value: Variant read get_value write set_value;
  end;

procedure Register;
procedure EnableControl(ControlToDisable: TControl; Field: TObject);
procedure DisableControl(ControlToDisable: TControl; Field: TObject);

implementation

uses new_phys_unit_lib,sysUtils,Contnrs,graphics;
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
  if Assigned(OnChange) then OnChange(self);
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
begin
//  if AllowExpressions then begin
  expression.SetString(text);
  if expression.isCorrect then
    Result:=expression.getVariantValue  //не гарантируетс€ отсутствие ошибок, тем не менее
  else begin
    Result:=0;
    if not (csDesigning in self.ComponentState) then
      Raise Exception.CreateFMT('TFloatLabel: %s',[expression.errorMsg]);
  end;
//  end
//  else
//    Result:=PhysUnitCreate(text);
end;

procedure TVariantEdit.Change;
var res: Variant;
resourcestring
  FloatEditNotARealNumberMsg = 'Ќе €вл€етс€ действительным числом';
begin
  if AllowExpressions then begin
    expression.SetString(text);
    if expression.isCorrect then ReturnToNormal
    else TurnRed(expression.errorMsg);
  end
  else
    if TryPhysUnitCreate(text,res) then ReturnToNormal
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
  fExpression:=TVariantExpression.create(self);
  if (csDesigning in ComponentState) then value:=StrToFloat('0');
end;

function TVariantEdit.isValid: Boolean;
var t: Variant;
begin
  if AllowExpressions then begin
    expression.SetString(text);
    Result:=expression.isCorrect;
  end
  else
    Result:=TryPhysUnitCreate(text,t);
end;



(* General procedures *)

procedure Register;
begin
  RegisterComponents('CautiousEdit', [TCautiousEdit,TVariantEdit]);
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
