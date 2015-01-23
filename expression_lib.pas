unit expression_lib;

interface

uses classes,Contnrs,SysUtils,ConvUtils;

type

ESyntaxErr=class(Exception);
ELexicalErr=class(Exception);

TEvaluationTreeNode=class(TComponent) //тогда сразу ему компоненты могут принадлежать-удобно
  public
    function getValue: Real; virtual; //по умолчанию берет getVariantValue и преобр. в Real
    function getVariantValue: Variant; virtual; abstract;
//    function getIntegerValue: Integer; virtual; abstract;
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
    destructor Destroy; override;
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
    fUnitType: TConvType;
  public
    constructor Create(aUnitType: TConvType; owner: TComponent); reintroduce; overload;
    function getVariantValue: Variant; override;
  end;

TUnitAssignmentNode=class(TUnitConversionNode)
  public
    function getVariantValue: Variant; override;
  end;

TMathFuncProc=function(X: Variant): Variant of object;

TMathFuncNode=class(TNonTerminalNode)
  private
    func: TMathFuncProc;
    function HandleFuncOfUnits(V: Variant; funcname: string): Variant;
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

    function Sind(x: Variant): Variant;
    function Cosd(x: Variant): Variant;
    function Tand(x: Variant): Variant;
    function Tgd(x: Variant): Variant;

    function Asin(x: Variant): Variant;
    function ACos(x: Variant): Variant;
    function ATan(x: Variant): Variant;
    function Arcsin(x: Variant): Variant;
    function Arccos(x: Variant): Variant;
    function Arctan(x: Variant): Variant;
    function Arctg(x: Variant): Variant;

    function asind(X: Variant): Variant;
    function acosd(X: Variant): Variant;
    function atand(X: Variant): Variant;
    function Arcsind(X: Variant): Variant;
    function Arccosd(X: Variant): Variant;
    function Arctand(x: variant): Variant;
    function Arctgd(x: Variant): Variant;

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

TVariableNode=class(TEvaluationTreeNode)
  private
    fComponent: TPersistent;
    fPropName: string;
  public
    constructor Create(aComponent: TComponent;aPropName: string; aOwner: TComponent); reintroduce; overload;
    function getValue: Real; override;
    function getVariantValue: Variant; override;
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
    function fIsIndependent: boolean;
    function getCorrect: boolean;
  protected
    procedure MakeEvaluationTree; virtual;
    procedure PlusMinus(s: string; var treeNode: TEvaluationTreeNode); virtual;
    procedure MulDiv(s: string; var treeNode: TEvaluationTreeNode); virtual;
    procedure Pow(s: string; var treeNode: TEvaluationTreeNode); virtual;
    procedure BracketsAndFuncs(s: string; var treeNode: TEvaluationTreeNode); virtual;
    procedure ConstsAndVars(s: string; var treeNode: TEvaluationTreeNode); virtual;
  public
    constructor Create(Owner: TComponent); override;
    constructor CreateZero(Owner: TComponent);

    destructor Destroy; override;
    procedure SetString(value: string);
    function getString: string;
    procedure SetRootComponent(value: TComponent);
    function getValue: Real;
    function getIntegerValue: Integer;
    property isCorrect: Boolean read GetCorrect;
    property errorMsg: string read fLastErrorMsg;
    property isIndependent: boolean read fIsIndependent;
  published
    property data: string read getString write SetString;
  end;

TLexemType=(ltLeftBracket,ltRightBracket,ltPlus,ltMinus,ltMul,ltDiv,ltPow,
  ltNumber,ltIdent,ltPhysUnit,ltPhysUnitConversion,ltPar,ltAssign);
TLexem=record
  LType: TLexemType;
  Num: Variant; //комплексное число тоже может быть
  Ident: string;
  PhysUnit: TConvType;
end;

TAssignValueToVariableProc = function (aname: string; avalue: Variant): Boolean;

TVariantExpression=class(TFloatExpression)  //
  private
    Lexems: array of TLexem;
    procedure EnsureLexemsLen(i: Integer);
  protected
    procedure LexicalAnalysis;
    procedure MakeEvaluationTree; override;
    procedure AssignOperators(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure UnitConversionOperators(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure PlusMinus(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
    procedure MulDiv(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
    procedure Par(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure Pow(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
    procedure PhysUnits(b,e: Integer; var treeNode: TEvaluationTreeNode);
    procedure BracketsAndFuncs(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
    procedure ConstsAndVars(b,e: Integer; var treeNode: TEvaluationTreeNode);reintroduce; overload;
  public
    AssignValueToVariableProc: TAssignValueToVariableProc;
    function GetVariantValue: Variant;

end;

TStandAloneFloatExpression = class (TFloatExpression)
  public
    constructor Create(Owner: TComponent); override;
end;

implementation

uses TypInfo,StrUtils,math,phys_units_lib,variants,simple_parser_lib,
  streamable_conv_units,VarCmplx;

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

destructor TConstantVariantNode.Destroy;
begin
//  fValue:=null;
  inherited Destroy;
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
begin
  Result:=1;
  for i:=0 to ComponentCount-1 do
    Result:=Result*(Components[i] as TEvaluationTreeNode).getVariantValue;
end;

(*
    TParNode
                      *)
function TParNode.getVariantValue: Variant;
var i: Integer;
  t: Variant;
begin
  if ComponentCount=0 then Raise Exception.Create('ParNode: zero elements');
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
constructor TUnitConversionNode.Create(aUnitType: TConvType; owner: TComponent);
begin
  inherited Create(owner);
  fUnitType:=aUnitType;
end;

function TUnitConversionNode.getVariantValue: Variant;
begin
  Result:=VarWithUnitConvert((Components[0] as TEvaluationTreeNode).getVariantValue,fUnitType);
end;

(*
  TUnitAssignmentNode
                        *)
function TUnitAssignmentNode.getVariantValue: Variant;
begin
  Result:=VarWithUnitCreateFromVariant((Components[0] as TEvaluationTreeNode).getVariantValue,fUnitType);
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
  //недостаточно абстрактное решение, ведь здесь оба числа преобр. в действ числа
  //правильнее выполнить ф-ию DoPower () для 1-го Variant'а.
//  Result:=Power((Components[0] as TEvaluationTreeNode).getVariantValue,(Components[1] as TEvaluationTreeNode).getVariantValue);
  //возведение в степень физ. величины-это бред
  //возведение физ. величины в комплексную степень - тоже
  b:=(Components[1] as TEvaluationTreeNode).getVariantValue;
  if IsVarWithUnit(b) then
    if IsDimensionless(b) then begin
      tmp:=TVAriantWithUnitVarData(b).Data.instance;
      b:=tmp;
    end
    else
      Raise ESyntaxErr.Create('Показатель степени должен быть безразмерным');
  a:=(Components[0] as TEvaluationTreeNode).getVariantValue;
  if IsVarWithUnit(a) then begin
    if IsDimensionless(a) then begin
      inst:=TVariantWithUnitVarData(a).Data.instance;
      //если a=()a.instance, то unassigned становится, видимо, рубит сук на котором сидит
      Result:=VarComplexSimplify(VarComplexPower(inst,b));
    end
    else begin
      b:=VarComplexSimplify(b);
      if VarIsComplex(b) then
        Raise ESyntaxErr.Create('Нельзя возводить размерную величину в комплексную степень');
      //возведение размерной величины в действ. степень - уже лучше
      Result:=VarWithUnitPower(a,b);
    end;
  end
  else Result:=VarComplexSimplify(VarComplexPower(a,b));
end;

(*
    TMathFuncNode
                    *)
constructor TMathFuncNode.Create(afunc: string; aowner: TComponent);
resourcestring
  MathFuncNodeUnknownFunc = 'Неизвестная функция: %s';
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
  if IsVarWithUnit(V) then begin
    if not IsDimensionLess(V) then
      tmp:=VarWithUnitConvert(V,duUnity)
    else
      tmp:=V;
    Result:=TVariantWithUnitVarData(tmp).Data.instance;
  end
  else Result:=V;
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
  x:=HandleFuncOfUnits(x,'sin');
  if VarIsComplex(x) then Result:=VarComplexSin(x)
  else Result:=system.Sin(x);
end;
function TMathFuncNode.Cos(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'cos');
  if VarIsComplex(x) then Result:=VarComplexCos(x)
  else Result:=system.Cos(x);
end;
function TMathFuncNode.Tan(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'tan');
  if VarIsComplex(x) then Result:=VarComplexCos(x)
  else Result:=math.Tan(x);
end;
function TMathFuncNode.Tg(x: Variant): Variant;
begin
  Result:=Tan(x);
end;

function TMathFuncNode.Sind(x: Variant): Variant;
begin
  Result:=Sin(x*pi/180);
end;
function TMathFuncNode.Cosd(x: Variant): Variant;
begin
  Result:=Cos(x*pi/180);
end;
function TMathFuncNode.Tand(x: Variant): Variant;
begin
  Result:=Tan(x*pi/180);
end;
function TMathFuncNode.Tgd(x: Variant): Variant;
begin
  Result:=Tand(x);
end;

function TMathFuncNode.Asin(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'asin');
  if VarIsComplex(x) then Result:=VarComplexArcsin(x)
  else Result:=math.ArcSin(x);
end;
function TMathFuncNode.Arcsin(x: Variant): Variant;
begin
  Result:=Asin(x);
end;
function TMathFuncNode.ACos(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'acos');
  if VarIsComplex(x) then Result:=VarComplexArcCos(x)
  else Result:=math.ArcCos(x);
end;
function TMathFuncNode.Arccos(x: Variant): Variant;
begin
  Result:=acos(x);
end;
function TMathFuncNode.ATan(x: Variant): Variant;
begin
  x:=HandleFuncOfUnits(x,'atan');
  if VarIsComplex(x) then Result:=VarComplexArcTan(x)
  else Result:=system.ArcTan(x);
end;
function TMathFuncNode.Arctan(x: Variant): Variant;
begin
  Result:=atan(x);
end;
function TMathFuncNode.Arctg(x: Variant): Variant;
begin
  Result:=atan(x);
end;

function TMathFuncNode.asind(X: variant): Variant;
begin
  Result:=Asin(X)*180/pi;
end;
function TMathFuncNode.arcsind(x: Variant): Variant;
begin
  Result:=asind(x);
end;
function TMathFuncNode.acosd(x: Variant): Variant;
begin
  Result:=Acos(x)*180/pi;
end;
function TMathFuncNode.Arccosd(x: Variant): Variant;
begin
  Result:=Acosd(x);
end;
function TMathFuncNode.atand(x: Variant): Variant;
begin
  Result:=atan(x)*180/pi;
end;
function TMathFuncNode.Arctand(x: Variant): Variant;
begin
  Result:=atand(x);
end;
function TMathFuncNode.Arctgd(x: Variant): Variant;
begin
  Result:=atand(x);
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
  if IsVarWithUnit(x) then Result:=VarWithUnitPower(x,0.5)
  else if VarIsComplex(x) then Result:=VarComplexSqrt(x)
  else Result:=system.Sqrt(x);
end;

function TMathFuncNode.Abs(x: Variant): Variant;
begin
  if IsVarWithUnit(x) then begin
    Result:=x;
    TVariantWithUnitVarData(Result).Data.instance:=Abs(TVariantWithUnitVarData(x).Data.instance);
  end
  else if VarIsComplex(x) then Result:=VarComplexAbs(x)
  else Result:=system.abs(x);
end;

function TMathFuncNode.Arg(x: Variant): Variant;
var t: Variant;
begin
  if IsVarWithUnit(x) then
    t:=TVariantWithUnitVarData(x).Data.instance     //аргумент-вел. безразмерная
  else t:=x;
  if VarIsComplex(t) then Result:=VarComplexAngle(t)
  else if t>=0 then Result:=0 else Result:=pi;
end;

function TMathFuncNode.Conj(x: Variant): Variant;
begin //комплексное сопряжение
  if IsVarWithUnit(x) then begin
    Result:=x;
    TVariantWithUnitVarData(Result).data.instance:=VarComplexConjugate(TVariantWithUnitVarData(x).Data.instance);
  end
  else Result:=VarComplexConjugate(x);
end;

function TMathFuncNode.Re(x: Variant): Variant;
begin
  if IsVarWithUnit(x) then begin
    Result:=x;
    TVariantWithUnitVarData(Result).Data.instance:=Re(TVariantWithUnitVarData(x).Data.instance);
  end
  else if VarIsComplex(x) then Result:=x.Real
  else Result:=x;
end;

function TMathFuncNode.Im(x: Variant): Variant;
begin
  if IsVarWithUnit(x) then begin
    Result:=x;
    TVariantWithUnitVarData(Result).Data.instance:=Im(TVariantWithUnitVarData(x).Data.instance);
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
  getVariantValue;
end;

function TVariableNode.isIndependent: boolean;
begin
  Result:=false;
end;

resourcestring
  VariableNodePropertyNotExistStr = 'Не найдено переменной "%s"';
  VariableNodeWrongTypeOfPropertyStr = 'Неверный тип переменной "%s"';

function TVariableNode.getValue: Real;
var propInfo: PPropInfo;
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
    else if (PropInfo.PropType^.Kind=tkClass) then
      Result:=TFloatExpression(GetObjectProp(fComponent,fPropName,TFloatExpression)).getValue
    else if PropInfo.PropType^.Kind=tkVariant then
      Result:=GetVariantProp(fComponent,fPropName)
    else
      Raise ESyntaxErr.CreateFmt(VariableNodeWrongTypeOfPropertyStr,[fPropName]);
  end;
end;

function TVariableNode.getVariantValue: Variant;
var propInfo: PPropInfo;
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
    else if (PropInfo.PropType^.Kind=tkClass) then
      Result:=TFloatExpression(GetObjectProp(fComponent,fPropName,TFloatExpression)).getValue
    else if PropInfo.PropType^.Kind=tkVariant then
      Result:=GetVariantProp(fComponent,fPropName)
    else
      Raise ESyntaxErr.CreateFmt(VariableNodeWrongTypeOfPropertyStr,[fPropName]);
  end;
end;

(*
    TFloatExpression
                      *)
resourcestring
  CircularReferenceErrStr = 'циклическая ссылка в выражении %s';
  EmptyEvaluationTreeErrStr = 'пустое дерево синтаксического разбора';
  EmptyStringErrStr = 'Отсутствует значение';

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
  fLastErrorMsg:=EmptyStringErrStr;
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
  try
  brCount:=0;
  last_plus:=1;
  isNeg:=false;
  signCount:=0;
  temp:=nil;
  for i:=1 to Length(s) do begin
    if brCount=0 then begin
      if ((s[i]='+') or (s[i]='-')) and ((i<=1) or (uppercase(s[i-1])<>'E')) then begin
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
        temp:=nil;
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

procedure TFloatExpression.MulDiv(s: string; var treeNode: TEvaluationTreeNode);
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
//  try
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
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
    Pow(RightStr(s,Length(s)-last_plus+1),temp);
    if isNeg then begin
      children[0]:=TInverseNode.Create(nil);
      children[0].InsertComponent(temp);
    end
    else
      children[0]:=temp;
    treeNode.InsertComponent(children[0]);
    children[0]:=nil;
  end;
(*
  finally
    for i:=0 to Length(children)-1 do
      children[i].Free;
  end;
  *)
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

resourcestring
  WrongExpressionStr = 'Выражение "%s" не является числом или переменной';

procedure TFloatExpression.ConstsAndVars(s: String; var treeNode: TEvaluationTreeNode);
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
    if Assigned(fComponent) and (fComponent is TFloatExpression) then
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

function TFloatExpression.getValue: Real;
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

(*
    TVariantExpression
                              *)
procedure TVariantExpression.EnsureLexemsLen(i: Integer);
begin
  if Length(Lexems)<i then
    SetLength(Lexems,Length(Lexems)+i);
end;
procedure TVariantExpression.LexicalAnalysis;
var p: TSimpleParser;
    LIndex,i,brLevel: Integer;
    ch: char;
begin
  LIndex:=-1;
  p:=TSimpleParser.Create(fstring);
  p.delimiter:=' '#9;
  try
    while not p.eof do begin
      inc(LIndex);  //мы уверены, что хоть одну лексему заполучим
      EnsureLexemsLen(LIndex+1);
      Lexems[LIndex].PhysUnit:=p.GetPhysUnit;
      if Lexems[LIndex].PhysUnit<>CIllegalConvType then
        Lexems[LIndex].LType:=ltPhysUnit
      else begin
        Lexems[LIndex].Ident:=p.getIdent;
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
            '[': begin
                  inc(LIndex);  //у нас еще скобочка появится где-то слева
                  EnsureLexemsLen(LIndex+1);
                  Lexems[LIndex].LType:=ltPhysUnitConversion;
                  Lexems[LIndex].PhysUnit:=p.getPhysUnit;
                  if Lexems[LIndex].PhysUnit=CIllegalConvType then
                    Lexems[LIndex].PhysUnit:=duUnity;
                  if p.eof or (p.getChar<>']') then
                    Raise ELexicalErr.CreateFmt('%s: [Unit] was expected',[fstring]);
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
                  //и осталось закрывающую скобочку поставить
                  inc(LIndex);
                  EnsureLexemsLen(LIndex+1);
                  Lexems[LIndex].LType:=ltRightBracket;
                end;
            '|': if (not p.eof) and (p.getChar='|') then
                    Lexems[LIndex].LType:=ltPar
                  else
                    Raise ELexicalErr.Create('неизвестный оператор |');
            '=': Lexems[LIndex].LType:=ltAssign;
            else begin
              p.PutBack;
              Lexems[LIndex].Num:=p.getVariantNum;
              Lexems[LIndex].LType:=ltNumber
            end;
          end;
        end;
      end;
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
//    UnitConversionOperators(0,Length(Lexems)-1,fEvaluationTreeRoot);
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
var i,brCount: Integer;
begin
  brCount:=0;
(*
  for i:=b to e do begin
    Case Lexems[i].LType of
      ltLeftBracket: inc(brCount);
      ltRightBracket: dec(brCount);
      ltAssign: if brCount=0 then begin
        if (i=b) or (Lexems[i-1].LType<>ltIdent) then
          Raise ESyntaxErr.Create('Слева от = должна быть переменная');


      end;
    end;
  end;
  *)
  UnitConversionOperators(b,e,TreeNode);
end;

procedure TVariantExpression.UnitConversionOperators(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var term: TEvaluationTreeNode;
begin
  if Lexems[e].LType=ltPhysUnitConversion then
    if e>b then begin
      UnitConversionOperators(b,e-1,term);
      TreeNode:=TUnitConversionNode.Create(Lexems[e].PhysUnit,nil);
      TreeNode.InsertComponent(term);
    end
    else Raise ESyntaxErr.CreateFmt('no expression to convert to %s',[ConvTypeToDescription(Lexems[e].PhysUnit)])
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
    else Raise ESyntaxErr.CreateFmt('no expression to assign unit %s',[ConvTypeToDescription(Lexems[e].PhysUnit)])
  else
    BracketsAndFuncs(b,e,TreeNode);
end;

procedure TVariantExpression.BracketsAndFuncs(b,e: Integer; var TreeNode: TEvaluationTreeNode);
var temp: TEvaluationTreeNode;
begin
  if b>e then raise ESyntaxErr.Create(EmptyStringErrStr);
  if Lexems[e].LType=ltRightBracket then begin
    if Lexems[b].LType=ltLeftBracket then
      UnitConversionOperators(b+1,e-1,treeNode)
    else if (Lexems[b+1].LType=ltLeftBracket) and (Lexems[b].LType=ltIdent) then begin
      temp:=nil;
      try
        UnitConversionOperators(b+2,e-1,temp);
        treeNode:=TMathFuncNode.Create(Lexems[b].Ident,nil);
        treeNode.InsertComponent(temp);
        temp:=nil;
      finally
        temp.Free;
      end;
      Exit;
    end
    else Raise ESyntaxErr.CreateFmt('%s: left bracket on wrong place',[fstring]);
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
  if b>e then raise ESyntaxErr.Create(EmptyStringErrStr);
  if b<e then raise ESyntaxErr.Create('2 or more lexems without operator in between');
  //остается b=e
  Case Lexems[b].LType of
    ltNumber: treeNode:=TConstantVariantNode.Create(VarWithUnitCreateFromVariant(Lexems[b].Num,duUnity),nil);
    ltIdent: begin
      s:=Lexems[b].Ident;
      if uppercase(s)='PI' then treeNode:=TConstantNode.Create(pi,nil)
      else if uppercase(s)='E' then treeNode:=TConstantNode.Create(exp(1),nil)
      else if (uppercase(s)='I') then treeNode:=TConstantVariantNode.Create(VarWithUnitCreateFromVariant(VarComplexCreate(0,1),duUnity),nil)
      else if Assigned(fRootComponent) then begin
      //видать, переменная
        fComponent:=FindNestedComponent(fRootComponent,s);
        if Assigned(fComponent) and (fComponent is TFloatExpression) then
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
    else raise ESyntaxErr.CreateFmt('Number or identifier expected, %s found',[fstring]);
  end;
end;

function TVariantExpression.GetVariantValue: Variant;
begin
  if fchanged then MakeEvaluationTree;
  if fCorrect then begin
    if Assigned(fEvaluationTreeRoot) then begin
      if fworking then Raise Exception.CreateFMT(CircularReferenceErrStr,[fstring]);
      fworking:=true;
      Result:=fEvaluationTreeRoot.getVariantValue;
      fworking:=false;
    end
    else Raise Exception.Create(EmptyEvaluationTreeErrStr);
  end
  else Raise Exception.Create(fLastErrorMsg);
end;


(*
    TStandAloneFloatExpression
                                  *)
constructor TStandAloneFloatExpression.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  SetSubComponent(false);
end;

initialization
  RegisterClasses([TFloatExpression, TStandAloneFloatExpression]);
end.
