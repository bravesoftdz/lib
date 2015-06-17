unit expression_lib;

interface

uses classes,Contnrs,SysUtils,new_phys_unit_lib,specchars;

type

ESyntaxErr=class(Exception);
ELexicalErr=class(Exception);

TEvaluationTreeNode=class(TComponent) //тогда сразу ему компоненты могут принадлежать-удобно
  public
    function getValue: Real; virtual; //по умолчанию берет getVariantValue и преобр. в Real
    function getVariantValue: Variant; virtual; abstract;
    function isIndependent: boolean; virtual; //по умолчанию, true
  end;

TConstantNode=class(TEvaluationTreeNode) //математическая константа
  private
    fValue: Real;
  public
    constructor Create(aValue: Real; owner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;

TConstantVariantNode=class(TEvaluationTreeNode) //может быть компл. число или с ед. изм
  private
    fValue: Variant;
  public
    constructor Create(aValue: Variant; owner: TComponent); reintroduce; overload;
    function getVariantValue: Variant; override;
  end;

TNonTerminalNode=class(TEvaluationTreeNode) //действие над другими выражениями
  public
    function isIndependent: boolean; override;  //действие по умолчанию: если входящие
    //в состав выражения независимы, значит и мы независимы.
  end;

TAdditionNode=class(TNonTerminalNode) //сумма нескольких слагаемых
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;

TMultiplicationNode=class(TNonTerminalNode) //произведение
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;

TParNode=class(TNonTerminalNode) //параллельное соединение
  public
    function getVariantValue: Variant; override;
  end;

TUnaryMinusNode=class(TNonTerminalNode)
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;

TInverseNode=class(TNonTerminalNode)  //обратная величина, 1/х
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;

TPowNode=class(TNonTerminalNode)  //возведение в степень
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  end;


TUnitConversionNode=class(TNonTerminalnode) //приведение к другой единице
  private
    fUnitType: TPhysUnit;
  public
    constructor Create(aUnitType: TPhysUnit; owner: TComponent); reintroduce; overload;
    function getVariantValue: Variant; override;
  end;

TUnitAssignmentNode=class(TUnitConversionNode)
  public
    function getVariantValue: Variant; override;
  end;

TAssignNode=class(TNonTerminalNode)
  public
    function getVariantValue: Variant; override;
  end;

TMathFuncProc=function(X: Variant): Variant of object;

TMathFuncNode=class(TNonTerminalNode)
  private
    func: TMathFuncProc;
    function HandleFuncOfUnits(V: Variant; funcname: string): Variant;
    function HandleTrigFunc(V: Variant): Variant;
  public
    constructor Create(afunc: string; aowner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function getVariantValue: Variant; override;
  published
    function Ln(x: Variant): Variant;
    function Lg2(x: Variant): Variant;
    function Log2(x: Variant): Variant;
    function Lg10(x: Variant): Variant;
    function Log10(x: Variant): Variant;

    function Sin(x: Variant): Variant;
    function Cos(x: Variant): Variant;
    function Tan(x: Variant): Variant;
    function Tg(x: Variant): Variant;

    function Asin(x: Variant): Variant;
    function ACos(x: Variant): Variant;
    function ATan(x: Variant): Variant;
    function Arcsin(x: Variant): Variant;
    function Arccos(x: Variant): Variant;
    function Arctan(x: Variant): Variant;
    function Arctg(x: Variant): Variant;

    function sinh(x: Variant): Variant;
    function cosh(x: Variant): Variant;
    function tanh(x: Variant): Variant;
    function tgh(x: Variant): Variant;

    function asinh(x: Variant): Variant;
    function acosh(x: Variant): Variant;
    function atanh(x: Variant): Variant;
    function Arcsinh(x: Variant): Variant;
    function Arccosh(x: Variant): Variant;
    function Arctanh(x: Variant): Variant;
    function Arctgh(x: Variant): Variant;

    function Sqrt(x: Variant): Variant;
    function Exp(x: Variant): Variant;

    function Abs(x: Variant): Variant;
    function Arg(x: Variant): Variant;
    function Conj(x: Variant): Variant;
    function Re(x: Variant): Variant;
    function Im(x: Variant): Variant;
  end;

IVariantProperties=Interface
['{99CB6FA7-1795-4C73-A4B7-E2854435DAFA}']
  function SetProperty(name: string; value: Variant): Boolean;
  function GetProperty(name: string; out val: Variant): Boolean; //идем напролом,
  //если нет св-ва или read-only, выкидываем exception
  //позже можно сделать еще TrySet и TryGet
end;

TVariableNode=class(TEvaluationTreeNode)
  private
    fComponent: TPersistent;
    fPropName: string;
  public
    constructor Create(aComponent: TComponent;aPropName: string; aOwner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function getVariantValue: Variant; override;
    procedure Assign(value: Variant); reintroduce; overload;
    function isIndependent: Boolean; override;
  end;

IExpression = interface
  procedure SetString(value: string);
  function getString: string;
  function getVariantValue: Variant;
  function getValue: Real;
  function getIntegerValue: Integer;
end;

TAbstractExpression=class(TComponent,IExpression)
  protected
    fWorking: boolean; //чтобы поймать циклическую ссылку
    fstring: string;
    fRootComponent: TComponent;
    fEvaluationTreeRoot: TEvaluationTreeNode;
    fcorrect: boolean;  //заполняются во время присвоения строки
    fLastErrorMsg: string;  //объяснение, в чем некорректность строки
    findependent: boolean;
    fchanged: boolean;
    function fIsIndependent: boolean;
    function getCorrect: boolean;
    procedure MakeEvaluationTree; virtual; abstract;
    procedure ConstsAndVars(s: string; var treeNode: TEvaluationTreeNode); virtual;
  public
    constructor Create(Owner: TComponent); override;
    constructor CreateZero(Owner: TComponent);

    destructor Destroy; override;
    procedure SetString(value: string);
    function getString: string;
    procedure SetRootComponent(value: TComponent);

    function getVariantValue: Variant; virtual; abstract;
    function getValue: Real; virtual;
    function getIntegerValue: Integer; virtual;

    property isCorrect: Boolean read GetCorrect;
    property errorMsg: string read fLastErrorMsg;
    property isIndependent: boolean read fIsIndependent;
  published
    property data: string read getString write SetString;
end;

TLexemType=(ltLeftBracket,ltRightBracket,ltPlus,ltMinus,ltMul,ltDiv,ltPow,
  ltNumber,ltIdent,ltPhysUnit,ltPhysUnitConversion,ltPar,ltAssign,ltLeftSquareBracket,ltRightSquareBracket);
TLexem=record
  LType: TLexemType;
  Num: Variant; //комплексное число тоже может быть
  Ident: string;
  PhysUnit: TPhysUnit;
  startStr,endStr: Integer; //начало и конец лексемы в строке
end;

TAssignValueToVariableProc = function (aname: string; avalue: Variant): Boolean;

TVariantExpression=class(TAbstractExpression)  //
  private
    fUnitRestriction: TPhysUnit;
    procedure EnsureLexemsLen(i: Integer);
  protected
    function AddBracketsForAssign(text: string; var map: TIntegerArray): string;
    procedure LexicalAnalysis;
    procedure AssignOperators(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure UnitConversionOperators(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure PlusMinus(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure MulDiv(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure Par(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure Pow(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure PhysUnits(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure BracketsAndFuncs(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure ConstsAndVars(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
  public
    Lexems: array of TLexem;
    AssignValueToVariableProc: TAssignValueToVariableProc;
    constructor CreateZero(owner: TComponent; unitRestriction: string);reintroduce; overload;
    procedure MakeEvaluationTree; override;
    procedure SetUnitRestriction(unitRestriction: string);
    function GetVariantValue: Variant; override;
    property UnitRestriction: TPhysUnit read fUnitRestriction;
end;

TStandAloneVariantExpression=class(TVariantExpression)
  public
    constructor Create(Owner: TComponent); override;
end;

resourcestring
  TooManyClosingBracketsStr = 'Закрывающих скобок больше, чем открывающих в %s';
  EmptyStringErrStr = 'Отсутствует значение';
  CircularReferenceErrStr = 'циклическая ссылка в выражении %s';
  ExponentShouldBeDimensionless='Показатель степени должен быть безразмерным';
  ExponentShouldBeReal='Нельзя возводить размерную величину в комплексную степень';
  VariableNodePropertyNotExistStr = 'Не найдено переменной "%s"';
  VariableNodeWrongTypeOfPropertyStr = 'Неверный тип переменной "%s"';
  WrongExpressionStr = 'Выражение "%s" не является числом или переменной';
  TrigFuncPrecisionLoss = 'Возможна потеря точности при выполнении триг. функции от %s = %s';
  VariableIsReadOnly='Переменная %s доступна только для чтения';
  UnknownOperatorVertLine = 'неизвестный оператор "|"';
  SomeCrapLeftOfAssign = 'Слева от = должна быть переменная';
  NoExpressionToConvertTo = 'отсутствует значение, которое надо сконвертировать в %s';
  LeftBracketOnWrongPlace = 'открывающая скобка не на своем месте';
  TwoOrMoreLexemsError = '2 или более лексических единиц, не связанных между собой';
  MathFuncNodeUnknownFunc = 'Неизвестная функция: %s';
  LeftSideExpressionIsConst='выражение слева от "=" является константой';
const
  EmptyEvaluationTreeErrStr = 'empty evaluation tree';
implementation

uses TypInfo,StrUtils,math,variants,simple_parser_lib,VarCmplx;

(*
    TEvaluationTreeNode
                          *)
function TEvaluationTreeNode.getValue: Real;
begin
  Result:=getVariantValue;
end;

function TEvaluationTreeNode.isIndependent: Boolean;
begin
  Result:=true;
end;
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

function TConstantNode.getVariantValue: Variant;
begin
  Result:=fValue;
end;

(*
    TConstantVariantNode
                            *)
constructor TConstantVariantNode.Create(aValue: Variant; owner: TComponent);
begin
  inherited Create(Owner);
  fValue:=aValue;
end;

function TConstantVariantNode.getVariantValue: Variant;
begin
  Result:=fValue;
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

function TAdditionNode.getVariantValue: Variant;
var i: Integer;
begin
  if ComponentCount=0 then Raise Exception.Create('AdditionNode: zero elements to add');
  //ошибка не должна возникать у пользователя
  Result:=(Components[0] as TEvaluationTreeNode).getVariantValue;
  for i:=1 to ComponentCount-1 do
    Result:=Result+(Components[i] as TEvaluationTreeNode).getVariantValue;
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

function TMultiplicationNode.getVariantValue: Variant;
var i: Integer;
  tmp: Variant;
begin
  Result:=1.00000;
  for i:=0 to ComponentCount-1 do begin
    tmp:=Result;
    Result:=tmp*(Components[i] as TEvaluationTreeNode).getVariantValue;
  end;
end;

(*
    TParNode
                      *)
function TParNode.getVariantValue: Variant;
var i: Integer;
  t: Variant;
begin
  if ComponentCount=0 then Raise Exception.Create('ParNode: zero elements');
  //ошибка не должна возникать у пользователя
  Result:=(Components[0] as TEvaluationTreeNode).getVariantValue;
  for i:=1 to ComponentCount-1 do begin
    t:=(Components[i] as TEvaluationTreeNode).getVariantValue;
    Result:=Result*t/(Result+t);
  end;
end;

(*
  TUnaryMinusNode
                      *)
function TUnaryMinusNode.getValue: Real;
begin
  Result:=-(Components[0] as TEvaluationTreeNode).getValue;
end;

function TUnaryMinusNode.getVariantValue: Variant;
begin
  Result:=-(Components[0] as TEvaluationTreeNode).getVariantValue;
end;

(*
  TInverseNode
                    *)
function TInverseNode.getValue: Real;
begin
  Result:=1/(Components[0] as TEvaluationTreeNode).getValue;
end;

function TInverseNode.getVariantValue: Variant;
begin
  Result:=1/(Components[0] as TEvaluationTreeNode).getVariantValue;
end;

(*
  TUnitConversionNode
                        *)
constructor TUnitConversionNode.Create(aUnitType: TPhysUnit; owner: TComponent);
begin
  inherited Create(owner);
  fUnitType:=aUnitType;
end;

function TUnitConversionNode.getVariantValue: Variant;
begin
  Result:=PhysUnitConvert((Components[0] as TEvaluationTreeNode).getVariantValue,fUnitType,true);
end;

(*
  TUnitAssignmentNode
                        *)
function TUnitAssignmentNode.getVariantValue: Variant;
begin
  Result:=PhysUnitCreateFromVariant((Components[0] as TEvaluationTreeNode).getVariantValue,fUnitType);
end;

(*
  TPowNode
              *)
function TPowNode.getValue: Real;
begin
  Result:=Power((Components[0] as TEvaluationTreeNode).getValue,(Components[1] as TEvaluationTreeNode).getValue);
end;

function TPowNode.getVariantValue: Variant;
var a,b,inst,tmp: Variant;
begin
  //возведение в степень физ. величины-это бред
  //возведение физ. величины в комплексную степень - тоже
  b:=(Components[1] as TEvaluationTreeNode).getVariantValue;
  if IsPhysUnit(b) then
    if IsDimensionless(b) then begin
      b:=PhysUnitConvert(b,PhysUnitData.Unity);
      tmp:=TVarWithUnitVarData(b).Data.instance;
      b:=tmp;
    end
    else
      Raise ESyntaxErr.Create(ExponentShouldBeDimensionless);
  a:=(Components[0] as TEvaluationTreeNode).getVariantValue;
  if IsPhysUnit(a) then begin
    if IsDimensionless(a) then begin
      inst:=TVarWithUnitVarData(a).Data.instance;
      //если a=()a.instance, то unassigned становится, видимо, рубит сук на котором сидит
      Result:=VarComplexSimplify(VarComplexPower(inst,b));
    end
    else begin
      b:=VarComplexSimplify(b);
      if VarIsComplex(b) then
        Raise ESyntaxErr.Create(ExponentShouldBeReal);
      //возведение размерной величины в действ. степень - уже лучше
      Result:=PhysUnitPower(a,b);
    end;
  end
  else Result:=VarComplexSimplify(VarComplexPower(a,b));
end;

(*
    TMathFuncNode
                    *)
constructor TMathFuncNode.Create(afunc: string; aowner: TComponent);
begin
  inherited Create(aowner);
  @func:=MethodAddress(afunc);
  if @func=nil then raise ESyntaxErr.CreateFmt(MathFuncNodeUnknownFunc,[afunc]);
end;

function TMathFuncNode.getValue: Real;
begin
  Result:=func((Components[0] as TEvaluationTreeNode).getValue);
end;

function TMathFuncNode.getVariantValue: Variant;
begin
  Result:=func((Components[0] as TEvaluationTreeNode).getVariantValue);
  //халтура, мы превращаем Variant в Real и опять в Variant!
  //уже нет
end;

function TMathFuncNode.HandleFuncOfUnits(V: Variant; funcname: string): Variant;
var tmp: Variant;
begin
  if IsPhysUnit(V) then begin
    tmp:=PhysUnitConvert(V,PhysUnitData.Unity);
    Result:=TVarWithUnitVarData(tmp).Data.instance;
  end
  else Result:=V;
end;

function TMathFuncNode.HandleTrigFunc(V: Variant): Variant;
var tmp: Variant;
begin
  tmp:=PhysUnitConvert(V,PhysUnitData.Radian);
  Result:=TVarWithUnitVarData(tmp).Data.instance;

  if Assigned(PhysUnitData.warningproc) then begin
    if VarIsNumeric(Result) then
      if system.Abs(Result)>=1e8 then
        PhysUnitData.warningproc(Format(TrigFuncPrecisionLoss,[V,tmp]));
    if VarIsComplex(Result) then
      if VarComplexAbs(Result)>=1e8 then
        PhysUnitData.warningproc(Format(TrigFuncPrecisionLoss,[V,tmp]));
  end;
end;

function TMathFuncNode.Ln(x: Variant): Variant; //натуральный
begin
  x:=HandleFuncOfUnits(x,'ln');
  if VarIsComplex(x) then Result:=VarComplexLn(x)
  else Result:=system.Ln(x);
end;

function TMathFuncNode.Lg2(x: Variant): Variant; //двоичный
begin
  x:=HandleFuncOfUnits(x,'lg2');
  if VarIsComplex(x) then Result:=VarComplexLog2(x)
  else Result:=math.Log2(x);
end;
function TMathFuncNode.Log2(x: Variant): Variant;
begin
  Result:=Lg2(x);
end;

function TMathFuncNode.Lg10(x: Variant): Variant; //десятичный
begin
  X:=HandleFuncOfUnits(x,'lg10');
  if VarIsComplex(x) then Result:=VarComplexLog10(x)
  else Result:=math.log10(x);
end;
function TMathFuncNode.Log10(x: Variant): Variant;
begin
  Result:=Lg10(x);
end;

function TMathFuncNode.Exp(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'exp');
  if VarIsComplex(x) then Result:=VarComplexExp(x)
  else Result:=System.Exp(x);
end;

//тригонометрия

function TMathFuncNode.Sin(x: Variant): Variant;
begin
  x:=HandleTrigFunc(x);
  if VarIsComplex(x) then Result:=VarComplexSin(x)
  else Result:=system.Sin(x);
end;
function TMathFuncNode.Cos(x: Variant): Variant;
begin
  x:=HandleTrigFunc(x);
  if VarIsComplex(x) then Result:=VarComplexCos(x)
  else Result:=system.Cos(x);
end;
function TMathFuncNode.Tan(x: Variant): Variant;
begin
  x:=HandleTrigFunc(x);
  if VarIsComplex(x) then Result:=VarComplexCos(x)
  else Result:=math.Tan(x);
end;
function TMathFuncNode.Tg(x: Variant): Variant;
begin
  Result:=Tan(x);
end;

function TMathFuncNode.Asin(x: Variant): Variant;
var tmp: Variant;
begin
  x:=HandleFuncOfUnits(x,'asin');
  if VarIsComplex(x) then tmp:=VarComplexArcsin(x)
  else tmp:=math.ArcSin(x);
  Result:=PhysUnitCreateFromVariant(tmp,PhysUnitData.Radian);
end;
function TMathFuncNode.Arcsin(x: Variant): Variant;
begin
  Result:=Asin(x);
end;
function TMathFuncNode.ACos(x: Variant): Variant;
var tmp: Variant;
begin
  x:=HandleFuncOfUnits(x,'acos');
  if VarIsComplex(x) then tmp:=VarComplexArcCos(x)
  else tmp:=math.ArcCos(x);
  Result:=PhysUnitCreateFromVariant(tmp,PhysUnitData.Radian);
end;
function TMathFuncNode.Arccos(x: Variant): Variant;
begin
  Result:=acos(x);
end;
function TMathFuncNode.ATan(x: Variant): Variant;
var tmp: Variant;
begin
  x:=HandleFuncOfUnits(x,'atan');
  if VarIsComplex(x) then tmp:=VarComplexArcTan(x)
  else tmp:=system.ArcTan(x);
  Result:=PhysUnitCreateFromVariant(tmp,PhysUnitData.Radian);
end;
function TMathFuncNode.Arctan(x: Variant): Variant;
begin
  Result:=atan(x);
end;
function TMathFuncNode.Arctg(x: Variant): Variant;
begin
  Result:=atan(x);
end;

function TMathFuncNode.sinh(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'sinh');
  if VarIsComplex(x) then Result:=VarComplexSinh(x)
  else Result:=math.Sinh(x);
end;
function TMathFuncNode.cosh(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'cosh');
  if VarIsComplex(x) then Result:=VarComplexCosh(x)
  else Result:=math.Cosh(x);
end;
function TMathFuncNode.tanh(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'tanh');
  if VarIsComplex(x) then Result:=VarComplexTanh(x)
  else Result:=math.Tanh(x);
end;
function TMathFuncNode.tgh(x: Variant): Variant;
begin
  Result:=tanh(x);
end;

function TMathFuncNode.asinh(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'asinh');
  if VarIsComplex(x) then Result:=VarComplexArcsinH(x)
  else Result:=math.ArcSinh(x);
end;
function TMathFuncNode.Arcsinh(x: Variant): Variant;
begin
  Result:=asinh(x);
end;
function TMathFuncNode.acosh(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'acosh');
  if VarIsComplex(x) then Result:=VarComplexArccosH(x)
  else Result:=math.ArcCos(x);
end;
function TMathFuncNode.Arccosh(x: Variant): Variant;
begin
  Result:=acosh(x);
end;
function TMathFuncNode.atanh(x: Variant): Variant;
begin
  X:=HandleFuncOfUnits(x,'atanh');
  if VarisComplex(x) then Result:=VarComplexArctanh(x)
  else Result:=math.ArcTanh(x);
end;
function TMathFuncNode.Arctanh(x: Variant): Variant;
begin
  Result:=atanh(x);
end;
function TMathFuncNode.Arctgh(x: Variant): Variant;
begin
  Result:=atanh(x);
end;

function TMathFuncNode.Sqrt(x: Variant): Variant;
begin
  //самый нетривиальный, ведь может работать с размерными величинами
  if IsPhysUnit(x) then Result:=PhysUnitPower(x,0.5)
  else if VarIsComplex(x) then Result:=VarComplexSqrt(x)
  else Result:=system.Sqrt(x);
end;

function TMathFuncNode.Abs(x: Variant): Variant;
begin
  if IsPhysUnit(x) then begin
    Result:=x;
    TVarWithUnitVarData(Result).Data.instance:=Abs(TVarWithUnitVarData(x).Data.instance);
  end
  else if VarIsComplex(x) then Result:=VarComplexAbs(x)
  else Result:=system.abs(x);
end;

function TMathFuncNode.Arg(x: Variant): Variant;
var t,t1: Variant;
begin
  if IsPhysUnit(x) then
    t:=TVarWithUnitVarData(x).Data.instance     //аргумент-вел. безразмерная
  else t:=x;
  if VarIsComplex(t) then t1:=VarComplexAngle(t)
  else if t>=0 then t1:=0 else t1:=pi;
  Result:=PhysUnitCreateFromVariant(t1,PhysUnitData.Radian);
end;

function TMathFuncNode.Conj(x: Variant): Variant;
begin //комплексное сопряжение
  if IsPhysUnit(x) then begin
    Result:=x;
    TVarWithUnitVarData(Result).data.instance:=VarComplexConjugate(TVarWithUnitVarData(x).Data.instance);
  end
  else Result:=VarComplexConjugate(x);
end;

function TMathFuncNode.Re(x: Variant): Variant;
begin
  if IsPhysUnit(x) then begin
    Result:=x;
    TVarWithUnitVarData(Result).Data.instance:=Re(TVarWithUnitVarData(x).Data.instance);
  end
  else if VarIsComplex(x) then Result:=x.Real
  else Result:=x;
end;

function TMathFuncNode.Im(x: Variant): Variant;
begin
  if IsPhysUnit(x) then begin
    Result:=x;
    TVarWithUnitVarData(Result).Data.instance:=Im(TVarWithUnitVarData(x).Data.instance);
  end
  else if VarIsComplex(x) then Result:=x.Imaginary
  else Result:=0.0;
end;


(*
    TVariableNode
                    *)
constructor TVariableNode.Create(aComponent: TComponent; aPropName: string; aOwner: TComponent);
begin
  inherited Create(aOwner);
  fComponent:=aComponent;
  fPropName:=aPropName;
//  getVariantValue;
end;

function TVariableNode.isIndependent: boolean;
begin
  Result:=false;
end;

function TVariableNode.getValue: Real;
begin
  if fComponent is TAbstractExpression then
    Result:=TAbstractExpression(fComponent).getValue
  else Result:=getVariantValue;
end;

function TVariableNode.getVariantValue: Variant;
var propInfo: PPropInfo;
    intf: IVariantProperties;
begin
  if fComponent is TAbstractExpression then
    Result:=TAbstractExpression(fComponent).getVariantValue
  else begin
    propInfo:=GetPropInfo(fComponent,fPropName);
    if propInfo=nil then
      if fComponent.GetInterface(IVariantProperties,intf)
        and intf.GetProperty(fPropName,Result) then Exit
      else
        raise ESyntaxErr.CreateFmt(VariableNodePropertyNotExistStr,[fPropName]);
    if PropInfo.PropType^.Kind=tkFloat then
      Result:=GetFloatProp(fComponent,fPropName)
    else if PropInfo.PropType^.Kind=tkInteger then
      Result:=GetOrdProp(fComponent,fPropName)
    else if (PropInfo.PropType^.Kind=tkClass) then
      Result:=TAbstractExpression(GetObjectProp(fComponent,fPropName,TAbstractExpression)).getVariantValue
    else if PropInfo.PropType^.Kind=tkVariant then
      Result:=GetVariantProp(fComponent,fPropName)
    else
      Raise ESyntaxErr.CreateFmt(VariableNodeWrongTypeOfPropertyStr,[fPropName]);
  end;
end;

procedure TVariableNode.Assign(value: Variant);
var propInfo: PPropInfo;
    intf: IVariantProperties;
begin
  if fcomponent is TAbstractExpression then
    TAbstractExpression(fComponent).SetString(value)
  else begin
    propInfo:=GetPropInfo(fComponent,fPropName);
    if propInfo=nil then
      if fComponent.GetInterface(IVariantProperties,intf)
        and intf.SetProperty(fPropName,value) then Exit
      else
        raise ESyntaxErr.CreateFmt(VariableNodePropertyNotExistStr,[fPropName]);
    if propInfo.SetProc=nil then
      Raise ESyntaxErr.CreateFmt(VariableIsReadOnly,[fPropName]);
    case propInfo.PropType^.Kind of
      tkFloat: SetFloatProp(fComponent,fPropName,value);
      tkInteger: SetOrdProp(fComponent,fPropName,value);
      tkVariant: SetVariantProp(fComponent,fPropName,value);
      tkClass: TAbstractExpression(GetObjectProp(fComponent,fPropName,TAbstractExpression)).SetString(value);
      else raise ESyntaxErr.CreateFmt(VariableNodeWrongTypeOfPropertyStr,[fPropName]);
    end;
  end;
end;

(*
    TAssignNode
                    *)
function TAssignNode.getVariantValue: Variant;
begin
  Result:=(Components[1] as TEvaluationTreeNode).getVariantValue;
  if Components[0] is TVariableNode then
    TVariableNode(Components[0]).Assign(Result)
  else
    Raise ESyntaxErr.Create(LeftSideExpressionIsConst);
end;

(*
    TFloatExpression
                      *)

constructor TAbstractExpression.Create(owner: TComponent);
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
  fLastErrorMsg:=EmptyStringErrStr;
end;

constructor TAbstractExpression.CreateZero(Owner: TComponent);
begin
  Create(Owner);
  SetString('0');
end;

destructor TAbstractExpression.Destroy;
begin
  fEvaluationTreeRoot.Free;
  inherited Destroy;
end;

procedure TAbstractExpression.SetString(value: string);
begin
  if value<>fstring then begin
    fstring:=value;
    fchanged:=true;
  end;
end;

procedure TAbstractExpression.SetRootComponent(value: TComponent);
begin
  fRootComponent:=value;
end;

function TAbstractExpression.getString: string;
begin
  Result:=fstring;
end;

procedure TAbstractExpression.ConstsAndVars(s: String; var treeNode: TEvaluationTreeNode);
var val: Extended;
    fComponent: TComponent;
    buRoot: TComponent;
    i: Integer;
begin
  if TryStrToFloat(s,val) then
    treeNode:=TConstantNode.Create(val,nil)
  else if uppercase(s)='PI' then treeNode:=TConstantNode.Create(pi,nil)
  else if uppercase(s)='E' then treeNode:=TConstantNode.Create(exp(1),nil)
  else if Assigned(fRootComponent) then begin
  //видать, переменная
    fComponent:=FindNestedComponent(fRootComponent,s);
    if Assigned(fComponent) and (fComponent is TAbstractExpression) then
      treeNode:=TVariableNode.Create(fComponent,'',nil)
    else begin
      i:=Length(s);
      while (i>0) and (s[i]<>'.') do dec(i);
      if uppercase(leftstr(s,4))='SELF' then begin
        buRoot:=fRootComponent;
        fRootComponent:=Owner;
        ConstsAndVars(rightstr(s,Length(s)-5),treeNode);
        fRootComponent:=buRoot;
        Exit;
      end;
      if i>0 then begin
        fComponent:=FindNestedComponent(fRootComponent,leftstr(s,i-1));
        if fComponent=nil then
          Raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
      end
      else
        fComponent:=fRootComponent;
      treeNode:=TVariableNode.Create(fComponent,RightStr(s,Length(s)-i),nil);
    end;
  end
  else raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
end;

function TAbstractExpression.fIsIndependent: Boolean;
begin
  if fchanged then MakeEvaluationTree;
  Result:=fIndependent;
end;

function TAbstractExpression.getCorrect: Boolean;
begin
  if fchanged then MakeEvaluationTree;
  Result:=fCorrect;
end;

(*
    TVariantExpression
                              *)
constructor TVariantExpression.CreateZero(Owner: TComponent; UnitRestriction: string);
begin
  CreateZero(Owner);
  fUnitRestriction:=PhysUnitData.StrToConvType(UnitRestriction);
end;

procedure TVariantExpression.SetUnitRestriction(unitRestriction: string);
begin
  fUnitRestriction:=PhysUnitData.StrToConvType(unitRestriction);
end;

procedure TVariantExpression.EnsureLexemsLen(i: Integer);
begin
  if Length(Lexems)<i then
    SetLength(Lexems,Length(Lexems)+i);
end;

function TVariantExpression.AddBracketsForAssign(text: string; var map: TIntegerArray): string;
var i,j,k,curbr: Integer;
begin
  Result:=text;
  i:=1;
  while i<Length(Result) do begin
    if Result[i]='=' then begin
    //вставляем все до знака равенства и его тоже
    //открывающую скобку перед переменной поставим на стадии лексического анализа
      k:=i;
      curbr:=0;
      while (k<Length(Result)) and (curbr>=0) do begin
        inc(k);
        case text[k] of
          '(': inc(curbr);
          ')': dec(curbr);
        end;
      end;
      Result:=StuffString(Result,k+1,0,')');
      SetLength(map,Length(Result)+2);
      for j:=Length(Result)+1 downto k do
        map[j]:=map[j-1];
    end;
    inc(i);
  end;
end;

procedure TVariantExpression.LexicalAnalysis;
var p: TPhysUnitParser;
    LIndex,i,brLevel: Integer;
    ch: char;
    str,str1: string;
    map: TIntegerArray;
begin
  LIndex:=-1;
  //первый проход - замена \deg на ° и добавление скобок для знака равенства
  //и вообще проверка на кол-во скобок, иначе сообщ. об ошибке невыразительное
  SetLength(map,length(fstring)+2);
  for i:=0 to length(map)-1 do
    map[i]:=i+1;

  str:=ConvertSpecCharsMap(fstring,map);

  str1:=AddBracketsForAssign(str,map);


  p:=TPhysUnitParser.Create(str1);
  p.delimiter:=' '#9;
  try
    while not p.eof do begin
      inc(LIndex);  //мы уверены, что хоть одну лексему заполучим
      EnsureLexemsLen(LIndex+1);

      Lexems[LIndex].startStr:=map[p.Pos-1];
      Lexems[LIndex].Ident:=p.getPhysUnitIdent;
      p.PutBack;
      //хочется по контексту понять, может ли здесь быть ед. изм?
      //после +,-,*,/,^,||,=,(,PhysUnit - точно нет
      //после числа - может быть
      //после ) - может быть
      if (LIndex>0) and (Lexems[LIndex-1].LType in [ltRightBracket,ltNumber,ltLeftSquareBracket]) then begin
        Lexems[LIndex].PhysUnit:=p.GetPhysUnit;
        if Assigned(Lexems[LIndex].PhysUnit) then begin
          Lexems[LIndex].LType:=ltPhysUnit;
          Lexems[LIndex].endStr:=map[p.Pos]-1;
          Continue;
        end;
      end;

      Lexems[LIndex].Ident:=p.getVarPathIdent;
      if Lexems[LIndex].Ident<>'' then
        Lexems[LIndex].LType:=ltIdent
      else begin
        ch:=p.getChar;
        case ch of
          ')': Lexems[LIndex].LType:=ltRightBracket;
          '(': Lexems[LIndex].LType:=ltLeftBracket;
          '+': Lexems[LIndex].LType:=ltPlus;
          '-': Lexems[LIndex].LType:=ltMinus;
          '*': Lexems[LIndex].LType:=ltMul;
          '/': if (not p.eof) and (p.nextChar='/') then begin
            //нашли комментарий, игнорируем все
              dec(LIndex);
              break;
            end
            else Lexems[LIndex].LType:=ltDiv;
          '^': Lexems[LIndex].LType:=ltPow;
          ']': begin
            if (LIndex>0) and (Lexems[LIndex-1].LType=ltLeftSquareBracket) then begin
              Lexems[LIndex].LType:=ltPhysUnit;
              Lexems[LIndex].PhysUnit:=PhysUnitData.Unity;
              inc(LIndex);
              EnsureLexemsLen(LIndex+1);
            end;
            Lexems[LIndex].LType:=ltRightSquareBracket;
            inc(LIndex);
            EnsureLexemsLen(LIndex+1);
            Lexems[LIndex].LType:=ltRightBracket;
            end;
          '[': begin
            if LIndex=0 then Raise ELexicalErr.CreateFMT(NoExpressionToConvertTo,[p.getIdent]);
            inc(LIndex);  //у нас еще скобочка появится где-то слева
            EnsureLexemsLen(LIndex+1);
            Lexems[LIndex].LType:=ltLeftSquareBracket;
            //теперь ищем скобочку.
            brLevel:=0; //условно
            for i:=LIndex-2 downto 0 do begin //LIndex-1 еще не заполнен
              Lexems[i+1]:=Lexems[i];
              Case Lexems[i].LType of
                ltLeftBracket:
                  if brLevel=0 then break
                  else dec(brLevel);
                ltRightBracket: inc(brLevel);
              end;
              if i=0 then Lexems[0].LType:=ltLeftBracket;
            end;
            //закрывающую скобку поставит ]
            end;
          '|': if (not p.eof) and (p.getChar='|') then
                Lexems[LIndex].LType:=ltPar
              else
                Raise ELexicalErr.Create(UnknownOperatorVertLine);
          '=':  if LIndex>0 then begin
                  Lexems[LIndex]:=Lexems[LIndex-1];
                  Lexems[LIndex-1].LType:=ltLeftBracket;
                  inc(LIndex);
                  EnsureLexemsLen(LIndex+1);
                  Lexems[LIndex].LType:=ltAssign;
                end;
          '\': begin
            Lexems[LIndex].Ident:=p.getIdent;
            Lexems[LIndex].LType:=ltPhysUnit;
            if Lexems[LIndex].Ident='deg' then
              Lexems[LIndex].PhysUnit:=PhysUnitData.StrToConvType('°');
            end;
          else begin
            p.PutBack;
            Lexems[LIndex].Num:=p.getVariantNum;
            Lexems[LIndex].LType:=ltNumber
          end;
        end;
      end;
      Lexems[LIndex].endStr:=map[p.Pos]-1;
    end;
  finally
    p.Free;
    SetLength(Lexems,LIndex+1);
  end;
end;

procedure TVariantExpression.MakeEvaluationTree;
begin
  FreeAndNil(fEvaluationTreeRoot);  //все дерево целиком сносится
  try
    LexicalAnalysis;
    if Length(Lexems)=0 then Raise ESyntaxErr.Create(EmptyStringErrStr);
    AssignOperators(0,Length(Lexems)-1,fEvaluationTreeRoot);
    fcorrect:=true;
    fIndependent:=fEvaluationTreeRoot.isIndependent;
  except
    on Ex: Exception do begin
      fLastErrorMsg:=Ex.message;
      fcorrect:=false;
    end;
    else
      raise;
  end;
  fchanged:=false;
end;

procedure TVariantExpression.AssignOperators(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var tmp: TEvaluationTreeNode;
begin
  if (b+1<e) and (Lexems[b+1].LType=ltAssign) then begin
    if (Lexems[b].LType<>ltIdent) then
      Raise ESyntaxErr.Create(SomeCrapLeftOfAssign);
    TreeNode:=TAssignNode.Create(nil);
    ConstsAndVars(b,b,tmp);
    TreeNode.InsertComponent(tmp);  //переменную вставили
    AssignOperators(b+2,e,tmp);
    TreeNode.InsertComponent(tmp);
  end
  else
    UnitConversionOperators(b,e,TreeNode);
end;

procedure TVariantExpression.UnitConversionOperators(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var term: TEvaluationTreeNode;
begin
  if (e>b+1) and (Lexems[e].LType=ltRightSquareBracket) and (Lexems[e-1].LType=ltPhysUnit)
  and (Lexems[e-2].LType=ltLeftSquareBracket) then
    if e>b+2 then begin
      UnitConversionOperators(b,e-3,term);
      TreeNode:=TUnitConversionNode.Create(Lexems[e-1].PhysUnit,nil);
      TreeNode.InsertComponent(term);
    end
    else Raise ESyntaxErr.CreateFmt(NoExpressionToConvertTo,[Lexems[e].PhysUnit.Caption.Caption])
  else
    PlusMinus(b,e,TreeNode);
end;

procedure TVariantExpression.PlusMinus(b,e: Integer; var Treenode: TEvaluationTreeNode);
var brCount,i,last_plus,signCount: Integer;
  children: array of TEvaluationTreeNode;
  temp: TEvaluationTreeNode;
  isNeg: boolean;
begin
  //двигаемся слева направо и ищем плюсы с минусами. Минусы могут быть унарными и бинарными
  //и скобок никто не отменял
  try
  brCount:=0;
  last_plus:=b;
  isNeg:=false;
  signCount:=0;
  temp:=nil;
  for i:=b to e do begin
    if (brCount=0) then begin
      if (Lexems[i].LType=ltPlus) or (Lexems[i].LType=ltMinus) then begin
        if i>b then begin
          SetLength(children,Length(children)+1);
          Par(last_plus,i-1,temp);
          if IsNeg then begin
            children[Length(children)-1]:=TUnaryMinusNode.Create(nil);
            children[Length(children)-1].InsertComponent(temp);
          end
          else
            children[Length(children)-1]:=temp;
        end;
        temp:=nil;
        last_plus:=i+1;
        isNeg:=(Lexems[i].LType=ltMinus);
        inc(signCount);
      end
    end;
    case Lexems[i].LType of
      ltRightBracket: dec(brCount);
      ltLeftBracket: inc(brCount);
    end;
    if brCount<0 then Raise ESyntaxErr.CreateFMT(TooManyClosingBracketsStr,[fstring]);
  end;
  if signCount=0 then Par(b,e,treeNode)
  else begin  //хоть один плюс или минус был, делаем последнюю веточку
    SetLength(children,Length(children)+1);
    Par(last_plus,e,temp);
    if IsNeg then begin
      children[Length(children)-1]:=TUnaryMinusNode.Create(nil);
      children[Length(children)-1].InsertComponent(temp);
    end
    else
      children[length(children)-1]:=temp;
    temp:=nil;
    //вот, все "дети" в сборе!
    treeNode:=TAdditionNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
  end;


  finally
    for i:=0 to Length(children)-1 do
      children[i].Free;
    temp.Free;
  end;
end;

procedure TVariantExpression.Par(b,e: Integer; var treeNode: TEvaluationTreeNode);
var i,brCount,last_plus: Integer;
    children: array of TEvaluationTreeNode;
begin
  brCount:=0;
  last_plus:=b;
  for i:=b to e do begin
    if brCount=0 then
      if Lexems[i].LType = ltPar then begin
        SetLength(children,Length(children)+1);
        MulDiv(last_plus,i-1,children[Length(children)-1]);
        last_plus:=i+1;
      end;
    case Lexems[i].LType of
      ltRightBracket: dec(brCount);
      ltLeftBracket: inc(brCount);
    end;
  end;
  if Length(children)=0 then MulDiv(b,e,treeNode)
  else begin
    treeNode:=TParNode.Create(nil);
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
    MulDiv(last_plus,e,children[0]);
    treeNode.InsertComponent(children[0]);
    children[0]:=nil;
  end;
end;


procedure TVariantExpression.MulDiv(b,e: Integer; var treeNode: TEvaluationTreeNode);
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
  try
  brCount:=0;
  last_plus:=b;
  isNeg:=false;
  for i:=b to e do begin
    if brCount=0 then begin
      if (Lexems[i].LType=ltMul) or (Lexems[i].LType=ltDiv) then begin
        SetLength(children,Length(children)+1);
        Pow(last_plus,i-1,temp);
        if isNeg then begin
          children[Length(children)-1]:=TInverseNode.Create(nil);  //позже закрепим
          children[Length(children)-1].InsertComponent(temp);
        end
        else
          children[length(children)-1]:=temp;
        last_plus:=i+1; //сразу за плюсом
        isNeg:=(Lexems[i].LType=ltDiv);
      end
    end;
    case Lexems[i].LType of
      ltRightBracket: dec(brCount);
      ltLeftBracket: inc(brCount);
    end;
  end;
  if Length(children)=0 then Pow(b,e,treeNode)
  else begin
    treeNode:=TMultiplicationNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
    Pow(last_plus,e,temp);
    if isNeg then begin
      children[0]:=TInverseNode.Create(nil);
      children[0].InsertComponent(temp);
    end
    else
      children[0]:=temp;
    treeNode.InsertComponent(children[0]);
    children[0]:=nil;
  end;

  finally
    for i:=0 to Length(children)-1 do
      children[i].Free;
  end;

end;

procedure TVariantExpression.Pow(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var i: Integer;
    brCount: Integer;
    term: TEvaluationTreeNode;
begin
  brCount:=0;
  for i:=b to e do begin
    if (Lexems[i].LType=ltPow) and (brCount=0) then begin
      treeNode:=TPowNode.Create(nil);
      PhysUnits(b,i-1,term);
      treeNode.InsertComponent(term);
      PhysUnits(i+1,e,term);
      treeNode.insertComponent(term);
      Exit;
    end;
    case Lexems[i].LType of
      ltLeftBracket: inc(brCount);
      ltRightBracket: dec(brCount);
    end;
  end;
  //если выполнение дошло досюда, значит, так и не встретили символа ^
  PhysUnits(b,e,treeNode);
end;

procedure TVariantExpression.PhysUnits(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var term: TEvaluationTreeNode;
begin
  if Lexems[e].LType=ltPhysUnit then
    if e>b then begin
      BracketsAndFuncs(b,e-1,term);
      TreeNode:=TUnitAssignmentNode.Create(Lexems[e].PhysUnit,nil);
      TreeNode.InsertComponent(term);
    end
    else begin
      //не к чему подставить величину, поэтому мы решаем, что здесь была переменная
      //ее имя мы на всякий случай держали в Ident
      Lexems[e].LType:=ltIdent;
      BracketsAndFuncs(b,e,TreeNode);
    end
  else
    BracketsAndFuncs(b,e,TreeNode);
end;

procedure TVariantExpression.BracketsAndFuncs(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var temp: TEvaluationTreeNode;
begin
  if b>e then raise ESyntaxErr.Create(EmptyStringErrStr);
  if Lexems[e].LType=ltRightBracket then begin
    if Lexems[b].LType=ltLeftBracket then
      AssignOperators(b+1,e-1,treeNode)
    else if (Lexems[b+1].LType=ltLeftBracket) and (Lexems[b].LType=ltIdent) then begin
      temp:=nil;
      try
        AssignOperators(b+2,e-1,temp);
        treeNode:=TMathFuncNode.Create(Lexems[b].Ident,nil);
        treeNode.InsertComponent(temp);
        temp:=nil;
      finally
        temp.Free;
      end;
      Exit;
    end
    else Raise ESyntaxErr.Create(LeftBracketOnWrongPlace);
  end
  else ConstsAndVars(b,e,treeNode);
end;

procedure TVariantExpression.ConstsAndVars(b,e: Integer; var treeNode: TEvaluationTreeNode);
var fComponent: TComponent;
    buRoot: TComponent;
    i: Integer;
    s: string;
begin
//должна остаться одна лексема
  if b<e then raise ESyntaxErr.Create(TwoOrMoreLexemsError);
  //остается b=e
  Case Lexems[b].LType of
    ltNumber: if IsPhysUnit(Lexems[b].Num) then
      treeNode:=TConstantVariantNode.Create(Lexems[b].Num,nil)
      else treeNode:=TConstantVariantNode.Create(PhysUnitCreateFromVariant(Lexems[b].Num,PhysUnitData.Unity),nil);
    ltIdent: begin
      s:=Lexems[b].Ident;
      if uppercase(s)='PI' then treeNode:=TConstantNode.Create(pi,nil)
      else if uppercase(s)='E' then treeNode:=TConstantNode.Create(exp(1),nil)
      else if (uppercase(s)='I') then treeNode:=TConstantVariantNode.Create(PhysUnitCreateFromVariant(VarComplexCreate(0,1),PhysUnitData.Unity),nil)
      else if Assigned(fRootComponent) then begin
      //видать, переменная
        fComponent:=FindNestedComponent(fRootComponent,s);
        if Assigned(fComponent) and (fComponent is TAbstractExpression) then
          treeNode:=TVariableNode.Create(fComponent,'',nil)
        else begin
          i:=Length(s);
          while (i>0) and (s[i]<>'.') do dec(i);
          if uppercase(leftstr(s,4))='SELF' then begin
            buRoot:=fRootComponent;
            fRootComponent:=Owner;
            ConstsAndVars(rightstr(s,Length(s)-5),treeNode);
            fRootComponent:=buRoot;
            Exit;
          end;
          if i>0 then begin
            fComponent:=FindNestedComponent(fRootComponent,leftstr(s,i-1));
            if fComponent=nil then
              Raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
          end
          else
            fComponent:=fRootComponent;
            treeNode:=TVariableNode.Create(fComponent,RightStr(s,Length(s)-i),nil);
          end;
        end
      else raise ESyntaxErr.CreateFmt(WrongExpressionStr,[s]);
    end;
    else raise ESyntaxErr.CreateFmt(WrongExpressionStr,[fstring]);
  end;
end;

function TVariantExpression.GetVariantValue: Variant;
begin
  if fchanged then MakeEvaluationTree;
  if fCorrect then begin
    Assert(Assigned(fEvaluationTreeRoot),EmptyEvaluationTreeErrStr);
    if fworking then Raise Exception.CreateFMT(CircularReferenceErrStr,[fstring]);
    fworking:=true;
    try
      if Assigned(fUnitRestriction) then
        Result:=PhysUnitConvert(fEvaluationTreeRoot.getVariantValue,fUnitRestriction)
      else
        Result:=fEvaluationTreeRoot.getVariantValue;
    finally
      fworking:=false;
    end;
  end
  else Raise Exception.Create(fLastErrorMsg);
end;

constructor TStandAloneVariantExpression.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  SetSubComponent(false);
end;

function TAbstractExpression.getValue: Real;
begin
  Result:=GetVariantValue;
end;

function TAbstractExpression.getIntegerValue: Integer;
begin
  Result:=Round(GetValue);
end;

initialization
  RegisterClasses([TVariantExpression,TStandAloneVariantExpression]);

end.
