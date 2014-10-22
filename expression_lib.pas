unit expression_lib;

interface

uses classes,Contnrs,SysUtils;

type

ESyntaxErr=class(Exception)
end;

TEvaluationTreeNode=class(TComponent) //тогда сразу ему компоненты могут принадлежать-удобно
  public
    function getValue: Real; virtual; abstract;
//    function getIntegerValue: Integer; virtual; abstract;
    function isIndependent: boolean; virtual; abstract;
  end;

TConstantNode=class(TEvaluationTreeNode) //математическая константа
  private
    fValue: Real;
  public
    constructor Create(aValue: Real; owner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function isIndependent: boolean; override;
  end;

TNonTerminalNode=class(TEvaluationTreeNode) //действие над другими выражениями
  public
    function isIndependent: boolean; override;  //действие по умолчанию: если входящие
    //в состав выражения независимы, значит и мы независимы.
  end;

TAdditionNode=class(TNonTerminalNode) //сумма нескольких слагаемых
  public
    function getValue: Real; override;
  end;

TMultiplicationNode=class(TNonTerminalNode) //произведение
  public
    function getValue: Real; override;
  end;

TUnaryMinusNode=class(TNonTerminalNode)
  public
    function getValue: Real; override;
  end;

TInverseNode=class(TNonTerminalNode)  //обратная величина, 1/х
  public
    function getValue: Real; override;
  end;

TPowNode=class(TNonTerminalNode)  //возведение в степень
  public
    function getValue: Real; override;
  end;

TMathFuncProc=function(X: Real): Real of object;

TMathFuncNode=class(TNonTerminalNode)
  private
    func: TMathFuncProc;
  public
    constructor Create(afunc: string; aowner: TComponent); reintroduce; overload;
    function getValue: Real; override;
  published
    function Ln(x: Real): Real;
    function Lg(x: Real): Real;
    function Sin(x: Real): Real;
    function Cos(x: Real): Real;
    function Sind(x: Real): Real;
    function Cosd(x: Real): Real;
    function Tan(x: Real): Real;
    function Tand(x: Real): Real;
    function Sqrt(x: Real): Real;
  end;

TVariableNode=class(TEvaluationTreeNode)
  private
    fComponent: TPersistent;
    fPropName: string;
  public
    constructor Create(aComponent: TComponent;aPropName: string; aOwner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function isIndependent: Boolean; override;
  end;


TFloatExpression=class(TComponent)
  private
    fWorking: boolean; //чтобы поймать циклическую ссылку
    fstring: string;
    fRootComponent: TComponent;
    fEvaluationTreeRoot: TEvaluationTreeNode;
    fcorrect: boolean;  //заполняются во время присвоения строки
    fLastErrorMsg: string;  //объяснение, в чем некорректность строки
    findependent: boolean;
    fchanged: boolean;
//    fOnChange: TNotifyEvent;
//    procedure SetOnChange(value: TNotifyEvent);
    procedure ReadString(reader: TReader);
    procedure WriteString(writer: TWriter);
    function fIsIndependent: boolean;
    function getCorrect: boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure MakeEvaluationTree;
    procedure PlusMinus(s: string; var treeNode: TEvaluationTreeNode);
    procedure MulDiv(s: string; var treeNode: TEvaluationTreeNode);
    procedure Pow(s: string; var treeNode: TEvaluationTreeNode);
    procedure BracketsAndFuncs(s: string; var treeNode: TEvaluationTreeNode);
    procedure ConstsAndVars(s: string; var treeNode: TEvaluationTreeNode);
  public
    constructor Create(Owner: TComponent); override;
    constructor CreateZero(Owner: TComponent);

    destructor Destroy; override;
    procedure SetString(value: string);
//    procedure DoChange;
    function getString: string;
    procedure SetRootComponent(value: TComponent);
    function getValue: Real;
    function getIntegerValue: Integer;
    property isCorrect: Boolean read GetCorrect;
    property errorMsg: string read fLastErrorMsg;
    property isIndependent: boolean read fIsIndependent;
//    property onChange: TNotifyEvent read fOnChange write SetOnChange;
  end;

implementation

uses TypInfo,StrUtils,math;

(*
    TConstantNode
                      *)
constructor TConstantNode.Create(aValue: Real; owner: TComponent);
begin
  inherited Create(owner);
  fValue:=aValue;
end;

function TConstantNode.getValue: Real;
begin
  Result:=fValue;
end;

function TConstantNode.isIndependent: Boolean;
begin
  Result:=true;
end;

(*
  TNonTerminalNode
                      *)
function TNonTerminalNode.isIndependent: Boolean;
var i: Integer;
begin
  Result:=true;
  for i:=0 to ComponentCount-1 do
    if not (Components[i] as TEvaluationTreeNode).isIndependent then begin
      Result:=false;
      break;
    end;
end;

(*
    TAdditionNode
                    *)
function TAdditionNode.getValue: Real;
var i: Integer;
begin
  Result:=0;
  for i:=0 to ComponentCount-1 do
    Result:=Result+(Components[i] as TEvaluationTreeNode).getValue;
end;

(*
    TMultiplicationNode
                        *)
function TMultiplicationNode.getValue: Real;
var i: Integer;
begin
  Result:=1;
  for i:=0 to ComponentCount-1 do
    Result:=Result*(Components[i] as TEvaluationTreeNode).getValue;
end;

(*
  TUnaryMinusNode
                      *)
function TUnaryMinusNode.getValue: Real;
begin
  Result:=-(Components[0] as TEvaluationTreeNode).getValue;
end;

(*
  TInverseNode
                    *)
function TInverseNode.getValue: Real;
begin
  Result:=1/(Components[0] as TEvaluationTreeNode).getValue;
end;

(*
  TPowNode
              *)
function TPowNode.getValue: Real;
begin
  Result:=Power((Components[0] as TEvaluationTreeNode).getValue,(Components[1] as TEvaluationTreeNode).getValue);
end;

(*
    TMathFuncNode
                    *)
constructor TMathFuncNode.Create(afunc: string; aowner: TComponent);
begin
  inherited Create(aowner);
  @func:=MethodAddress(afunc);
end;

function TMathFuncNode.getValue: Real;
begin
  Result:=func((Components[0] as TEvaluationTreeNode).getValue);
end;

function TMathFuncNode.Ln(x: Real): Real;
begin
  Result:=system.Ln(x);
end;

function TMathFuncNode.Lg(x: Real): Real;
begin
  Result:=Ln(x)/Ln(2);
end;

function TMathFuncNode.Sin(x: Real): Real;
begin
  Result:=system.Sin(x);
end;

function TMathFuncNode.Sind(x: Real): Real;
begin
  Result:=Sin(x*pi/180);
end;

function TMathFuncNode.Cos(x: Real): Real;
begin
  Result:=system.Cos(x);
end;

function TMathFuncNode.Cosd(x: Real): Real;
begin
  Result:=Cos(x*pi/180);
end;

function TMathFuncNode.Tan(x: Real): Real;
begin
  Result:=math.Tan(x);
end;

function TMathFuncNode.Tand(x: Real): Real;
begin
  Result:=Tan(x*pi/180);
end;

function TMathFuncNode.Sqrt(x: Real): Real;
begin
  Result:=system.Sqrt(x);
end;

(*
    TVariableNode
                    *)
constructor TVariableNode.Create(aComponent: TComponent; aPropName: string; aOwner: TComponent);
begin
  inherited Create(aOwner);
  fComponent:=aComponent;
  fPropName:=aPropName;
  getValue;
end;

function TVariableNode.isIndependent: boolean;
begin
  Result:=false;
end;

function TVariableNode.getValue: Real;
var propInfo: PPropInfo;
resourcestring
  VariableNodePropertyNotExistStr = 'Не найдено переменной "%s"';
  VariableNodeWrongTypeOfPropertyStr = 'Неверный тип переменной "%s"';
begin
  if fComponent is TFloatExpression then
    Result:=TFloatExpression(fComponent).getValue
  else begin
    propInfo:=GetPropInfo(fComponent,fPropName);
    if propInfo=nil then raise ESyntaxErr.CreateFmt(VariableNodePropertyNotExistStr,[fPropName]);
    if PropInfo.PropType^.Kind=tkFloat then
      Result:=GetFloatProp(fComponent,fPropName)
    else if PropInfo.PropType^.Kind=tkInteger then
      Result:=GetOrdProp(fComponent,fPropName)
    else if (PropInfo.PropType^.Kind=tkClass) then begin
      Result:=TFloatExpression(GetObjectProp(fComponent,fPropName,TFloatExpression)).getValue;
    end
    else
      Raise ESyntaxErr.CreateFmt(VariableNodeWrongTypeOfPropertyStr,[fPropName]);
  end;
end;

(*
    TFloatExpression
                      *)
constructor TFloatExpression.Create(owner: TComponent);
var tmp: TComponent;
begin
  inherited Create(owner);
  SetSubComponent(true);
  tmp:=owner;
  while assigned(tmp) do begin
    owner:=tmp;
    tmp:=tmp.Owner;
  end;
  //если мы передавали owner=nil, то так он и останется
  fRootComponent:=owner;
end;

constructor TFloatExpression.CreateZero(Owner: TComponent);
begin
  Create(Owner);
  SetString('0');
end;

(*
procedure TFloatExpression.DoChange;
begin
  if Assigned(fOnChange) then fonChange(self);
end;

procedure TFloatExpression.SetOnChange(value: TNotifyEvent);
begin
  fOnChange:=value;
  if not (csLoading in ComponentState) then DoChange;
end;
*)

destructor TFloatExpression.Destroy;
begin
  fEvaluationTreeRoot.Free;
  inherited Destroy;
end;

procedure TFloatExpression.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('value',ReadString,WriteString,fstring<>'');
end;

procedure TFloatExpression.ReadString(reader: TReader);
begin
  SetString(reader.ReadString);
end;

procedure TFloatExpression.WriteString(writer: TWriter);
begin
  writer.WriteString(fString); //это как string, но без кавычек, мне больше нравится
end;

procedure TFloatExpression.SetString(value: string);
begin
  if value<>fstring then begin
    fstring:=value;
    fchanged:=true;
  end;
end;

procedure TFloatExpression.SetRootComponent(value: TComponent);
begin
  fRootComponent:=value;
end;

function TFloatExpression.getString: string;
begin
  Result:=fstring;
end;
//а теперь самая мякотка - построение стека и его проход.
//лексический анализ будем делать?

procedure TFloatExpression.MakeEvaluationTree;
begin
  FreeAndNil(fEvaluationTreeRoot);  //все дерево целиком сносится
  try
    PlusMinus(fstring,fEvaluationTreeRoot);
    fcorrect:=true;
    fIndependent:=fEvaluationTreeRoot.isIndependent;
  except
    on Ex: ESyntaxErr do begin
      fLastErrorMsg:=Ex.message;
      fcorrect:=false;
    end;
    else
      raise;
  end;
  fchanged:=false;
end;

resourcestring
  TooManyClosingBracketsStr = 'Закрывающих скобок больше, чем открывающих в %s';

procedure TFloatExpression.PlusMinus(s: string; var treeNode: TEvaluationTreeNode);
//treenode - это var-переменная, в которую мы должны положить адрес созданного узла
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    signCount: Integer;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
  brCount:=0;
  last_plus:=1;
  isNeg:=false;
  signCount:=0;
  for i:=1 to Length(s) do begin
    if brCount=0 then begin
      if ((s[i]='+') or (s[i]='-')) and ((i<1) or (uppercase(s[i-1])<>'E')) then begin
        if i>1 then begin
          SetLength(children,Length(children)+1);
          MulDiv(MidStr(s,last_plus,i-last_plus),temp);
          if isNeg then begin
            children[Length(children)-1]:=TUnaryMinusNode.Create(nil);  //позже закрепим
            children[Length(children)-1].InsertComponent(temp);
          end
          else
          children[length(children)-1]:=temp;
        end;
        last_plus:=i+1; //сразу за плюсом
        isNeg:=(s[i]='-');
        inc(signCount);
      end
    end;
    if s[i]='(' then inc(brCount)
    else if s[i]=')' then dec(brCount);
    if brCount<0 then Raise ESyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  if signCount=0 then MulDiv(s,treeNode)
  else begin
    SetLength(children,Length(children)+1);
    MulDiv(RightStr(s,Length(s)-last_plus+1),temp);
    if isNeg then begin
      children[Length(children)-1]:=TUnaryMinusNode.Create(nil);  //позже закрепим
      children[Length(children)-1].InsertComponent(temp);
    end
    else
      children[length(children)-1]:=temp;
    //вот, все "дети" в сборе!
    treeNode:=TAdditionNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do
      treeNode.InsertComponent(children[i]);
  end;
end;

procedure TFloatExpression.MulDiv(s: string; var treeNode: TEvaluationTreeNode);
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
  brCount:=0;
  last_plus:=1;
  isNeg:=false;
  for i:=1 to Length(s) do begin
    if brCount=0 then begin
      if (s[i]='*') or (s[i]='/') then begin
        SetLength(children,Length(children)+1);
        Pow(MidStr(s,last_plus,i-last_plus),temp);
        if isNeg then begin
          children[Length(children)-1]:=TInverseNode.Create(nil);  //позже закрепим
          children[Length(children)-1].InsertComponent(temp);
        end
        else
          children[length(children)-1]:=temp;
        last_plus:=i+1; //сразу за плюсом
        isNeg:=(s[i]='/');
      end
    end;
    if s[i]='(' then inc(brCount)
    else if s[i]=')' then dec(brCount);
    if brCount<0 then Raise EsyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  if Length(children)=0 then Pow(s,treeNode)
  else begin
    treeNode:=TMultiplicationNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do
      treeNode.InsertComponent(children[i]);
    Pow(RightStr(s,Length(s)-last_plus+1),temp);
    if isNeg then begin
      children[0]:=TInverseNode.Create(nil);
      children[0].InsertComponent(temp);
    end
    else
      children[0]:=temp;
    treeNode.InsertComponent(children[0]);
  end;
end;

procedure TFloatExpression.Pow(s: string; var treeNode: TEvaluationTreeNode);
var i: Integer;
    brCount: Integer;
    term: TEvaluationTreeNode;
begin
  brCount:=0;
  for i:=1 to Length(s) do begin
    if (s[i]='^') and (brCount=0) then begin
      treeNode:=TPowNode.Create(nil);
      BracketsAndFuncs(LeftStr(s,i-1),term);
      treeNode.InsertComponent(term);
      BracketsAndFuncs(RightStr(s,Length(s)-i),term);
      treeNode.insertComponent(term);
      Exit;
    end;
    if s[i]='(' then inc(brCount);
    if s[i]=')' then dec(brCount);
    if brCount<0 then Raise EsyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  //если выполнение дошло досюда, значит, так и не встретили символа ^
  BracketsAndFuncs(s,treeNode);
end;

procedure TFloatExpression.BracketsAndFuncs(s: string; var treeNode: TEvaluationTreeNode);
var f: string;
  i: Integer;
  temp: TEvaluationTreeNode;
resourcestring
  EmptyStringErrStr = 'Отсутствует значение';
begin
  if Length(s)=0 then raise ESyntaxErr.Create(EmptyStringErrStr);
  if s[Length(s)]=')' then begin
    if s[1]='(' then
      PlusMinus(MidStr(s,2,Length(s)-2),treeNode)
    else begin
      for i:=2 to Length(s)-1 do
        if s[i]='(' then begin
          f:=LeftStr(s,i-1);
          treeNode:=TMathFuncNode.Create(f,nil);
          PlusMinus(MidStr(s,i+1,Length(s)-i-1),temp);
          treeNode.InsertComponent(temp);
          Exit;
        end;
      Raise ESyntaxErr.Create(TooManyClosingBracketsStr);
    end;
  end
  else ConstsAndVars(s,treeNode);
end;

procedure TFloatExpression.ConstsAndVars(s: String; var treeNode: TEvaluationTreeNode);
var val: Extended;
    fComponent: TComponent;
    i: Integer;
resourcestring
  WrongExpressionStr = 'Выражение "%s" не является числом или переменной';
begin
  if TryStrToFloat(s,val) then
    treeNode:=TConstantNode.Create(val,nil)
  else if uppercase(s)='PI' then treeNode:=TConstantNode.Create(pi,nil)
  else if uppercase(s)='E' then treeNode:=TConstantNode.Create(exp(1),nil)
  else if Assigned(fRootComponent) then begin
  //видать, переменная
    fComponent:=FindNestedComponent(fRootComponent,s);
    if Assigned(fComponent) and (fComponent is TFloatExpression) then
      treeNode:=TVariableNode.Create(fComponent,'',nil)
    else begin
      i:=Length(s);
      while (i>0) and (s[i]<>'.') do dec(i);
      fComponent:=FindNestedComponent(fRootComponent,leftstr(s,i-1));
      if fComponent=nil then
        Raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
      treeNode:=TVariableNode.Create(fComponent,RightStr(s,Length(s)-i),nil);
    end;
  end
  else raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
end;

function TFloatExpression.getValue: Real;
resourcestring
  CircularReferenceErrStr = 'циклическая ссылка в выражении %s';
  EmptyEvaluationTreeErrStr = 'пустое дерево синтаксического разбора';
begin
  if fchanged then MakeEvaluationTree;
  if fCorrect then begin
    if Assigned(fEvaluationTreeRoot) then begin
      if fworking then Raise Exception.CreateFMT(CircularReferenceErrStr,[fstring]);
      fworking:=true;
      Result:=fEvaluationTreeRoot.getValue;
      fworking:=false;
    end
    else Raise Exception.Create(EmptyEvaluationTreeErrStr);
  end
  else Raise Exception.Create(fLastErrorMsg);
end;

function TFloatExpression.getIntegerValue: Integer;
begin
  Result:=Round(getValue);
end;

function TFLoatExpression.fIsIndependent: Boolean;
begin
  if fchanged then MakeEvaluationTree;
  Result:=fIndependent;
end;

function TFloatExpression.getCorrect: Boolean;
begin
  if fchanged then MakeEvaluationTree;
  Result:=fCorrect;
end;

end.
