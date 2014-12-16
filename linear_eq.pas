unit linear_eq;

interface

uses Variants,classes,streamable_component_list;

type

TSLEQStatus = (slOneSolution,slNoSolution,slManySolutions);

IEquationNode=interface //отобразить красиво свое значение, чтоб не заморачивать
//интерфейс решения лин. уравнений
  function ShowNodeName: string;
  procedure SetValue(value: Variant);
  function ShowValue(value: Variant): string;
end;

TVariableForEq=record
  reference: IEquationNode;
  coeff: Variant; //могут быть комплексные числа
end;

TVariableForEqArray=array of TVariableForEq;

IKirhgofSLEQ=interface
  procedure SetRootComponent(comp: TComponent);
  procedure AddEquation(vars: TVariableForEqArray; equals: Variant);
  procedure SetTolerance(value: real);
  function GetVariable(p: IEquationNode): Variant;
  function GetStatus: TSLEQStatus;
  procedure Solve;
  function GetEquationsAsString: string;
  function GetSolutionAsString: string;
end;

IAbstractSLEQ=interface
  procedure SetDimensions(NumOfVars,NumOfEqs: Integer);
  procedure SetMatrix(i,j: Integer; value: Variant);
  procedure SetTolerance(value: real);
  function GetMatrix(i,j: Integer): Variant;
  function GetVariable(i: Integer): Variant;
  procedure SetVariableName(i: Integer; value: string);
  function GetVariableName(i: Integer): string;
  function GetStatus: TSLEQStatus;
  procedure Solve;
  property Matrix[i,j: Integer]: Variant read GetMatrix write SetMatrix;
  property VariableName[i: Integer]: string read GetVariableName write SetVariableName;
end;


TManySolutionsDataType = class(TPersistent)
protected
  function Multiplier(value: Real): string;
public
  InitValue: Real;
  Vars: array of Real;
  tolerance: Real;
  VarNames: array of string;
  procedure Assign(Source: TPersistent); override;
  procedure Add(value: TManySolutionsDataType);
  procedure Sub(value: TManySolutionsDataType);
  procedure Mul(value: Real); overload;
  procedure Mul(value: TManySolutionsDataType); overload;
  procedure Divide(value: Real); overload;
  procedure Divide(value: TManySolutionsDataType); overload;
  function IsPlainNumber: boolean;
//  function AreProportional(other: TManySolutionsDataType): boolean;
//  procedure SetString(value: string);
  function GetString: string;
end;


TManySolutionsVarData = packed record
  VType: TVarType;
  Reserved1, Reserved2, Reserved3: Word;
  Ref: TManySolutionsDataType;  //много свободного места, но неудобно
  Reserved4: LongInt; //удобнее все в классе хранить.
end;

TManySolutionsVariantType=class(TCustomVariantType)
protected
  function RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
  function LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
public
  procedure Clear(var V: TVarData); override;
  procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
  procedure Cast(var Dest: TVarData; const Source: TVarData); override;
  procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  procedure UnaryOp(var Right: TVarData; const Operator: Integer); override;
  procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
end;

TSimpleGaussLEQ=class(TInterfacedObject,IAbstractSLEQ)
  private
    fmatrix: array of array of Variant;
    fIndexes: array of Integer;
    fInvIndexes: array of Integer;
    fVariables: array of Variant;
    fVariableNames: array of string;
    fNumOfVars,fNumOfEqs: Integer;
    ftolerance: Real;
    fstatus: TSLEQStatus;
  protected
    procedure SwitchRows(row1,row2: Integer);
    procedure SwitchCols(col1,col2: Integer);
    procedure SolveOneSolution;
    procedure SolveManySolutions;
  public
    procedure SetDimensions(NumOfVars,NumOfEqs: Integer);
    procedure SetMatrix(i,j: Integer; value: Variant);
    procedure SetTolerance(value: real);
    function GetMatrix(i,j: Integer): Variant;
    function GetVariable(i: Integer): Variant;
    function GetStatus: TSLEQStatus;
    procedure SetVariableName(i: Integer; value: string);
    function GetVariableName(i: Integer): string;
    procedure Solve;
//    property Matrix[i,j: Integer]: Real read GetMatrix write SetMatrix;
end;

TSimpleGaussLEQForKirhgof = class(TInterfacedObject,IKirhgofSLEQ)
  private
    fSolver: TSimpleGaussLEQ;
    fList: TInterfaceList;
    fRootComponent: TComponent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetRootComponent(c: TComponent);
    procedure AddEquation(vars: TVariableForEqArray; equals: Variant);
    procedure SetTolerance(value: real);
    function GetVariable(p: IEquationNode): Variant;
    function GetStatus: TSLEQStatus;
    procedure Solve;
    function GetEquationsAsString: string;
    function GetSolutionAsString: string;
  end;

TSimulationType = -$7FFFFFFF-1..$7FFFFFFF;

TSweep = class (TComponent)
  private
    fEnabled,fIsLog: Boolean;
    fVariable: TComponent;
    fMinVal,fMaxVal,fIncr: Real;
  published
    property Enabled: Boolean read fEnabled write fEnabled default false;
    property Variable: TComponent read fVariable write fVariable;
    property MinVal: Real read fMinVal write fMinVal;
    property MaxVal: Real read fMaxVal write fMaxVal;
    property Incr: Real read fIncr write fIncr;
    property isLog: Boolean read fIsLog write fIsLog default false;
end;

TAnalysis = class (TComponent)
  private
    fSimulationType: TSimulationType;
    fVarsOfInterest: TStreamableComponentList;
    fPrimary, fSecondary: TSweep;
  public
    constructor Create(Owner: TComponent); override;
  published
    property SimulationType: TSimulationType read fSimulationType write fSimulationType;
    property VarsOfInterest: TStreamableComponentList read fVarsOfInterest write fVarsOfInterest;
    property PrimarySweep: TSweep read fPrimary write fPrimary;
    property SecondarySweep: TSweep read fSecondary write fSecondary;
end;

function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;

function GetLengthSquared(value: Variant): Real;

function RegisterSimulationType(description: string): TSimulationType;

var stTransient,stAC,stDC, stUndefined : TSimulationType;

implementation

uses streaming_class_lib,sysUtils,varCmplx;

var ManySolutionsVariantType: TManySolutionsVariantType;
    AnalysisTypes: TStrings;

(*
      TSimpleGaussLEQ
                              *)
procedure TSimpleGaussLEQ.SetDimensions(NumOfVars,NumOfEqs: Integer);
var i,j: Integer;
begin
  SetLength(fmatrix,NumOfVars+1,NumOfEqs);
  SetLength(findexes,NumOfVars);
  SetLength(fInvIndexes,NumOfVars);
  SetLength(fvariables,NumOfVars);
  SetLength(fVariableNames,NumOfVars);
  if NumOfVars>fNumOfVars then
    for i:=0 to fNumOfEqs-1 do begin
      fmatrix[NumOfVars,i]:=fmatrix[fNumOfVars,i];
      for j:=fNumOfVars to NumOfVars-1 do
        fmatrix[j,i]:=0;
    end;  //очистили место справа от исх. матрицы
  if NumOfEqs>fNumOfEqs then
    for i:=0 to NumOfVars do
      for j:=fNumOfEqs to NumOfEqs-1 do
        fmatrix[i,j]:=0;
  fNumOfVars:=NumOfVars;
  fNumOfEqs:=NumOfEqs;
  for i:=0 to fNumOfVars-1 do begin
    findexes[i]:=i;
    fInvIndexes[i]:=i;
  end;

end;

procedure TSimpleGaussLEQ.SetMatrix(i,j: Integer; value: Variant);
begin
  fmatrix[i,j]:=value;
end;

procedure TSimpleGaussLEQ.SetTolerance(value: Real);
begin
  ftolerance:=value;
end;

function TSimpleGaussLEQ.GetMatrix(i,j: Integer): Variant;
begin
  Result:=fmatrix[i,j];
end;

function TSimpleGaussLEQ.GetStatus: TSLEQStatus;
begin
  Result:=fstatus;
end;

function TSimpleGaussLEQ.GetVariable(i: Integer): Variant;
begin
  Result:=fVariables[findexes[i]];
end;

procedure TSimpleGaussLEQ.SetVariableName(i: Integer; value: string);
begin
  fVariableNames[i]:=value;
end;

function TSimpleGaussLEQ.GetVariableName(i: Integer): string;
begin
  Result:=fVariableNames[i];
end;

procedure TSimpleGaussLEQ.Solve;
var i,j,k: Integer;
  max_elem: Real;
  row_num,col_num: Integer;
  ratio: Variant;
begin
  //обеспечиваем нули в нижнем треугольнике
  for j:=0 to fNumOfEqs-1 do begin
    //ищем макс. элем не выше и не левее (j,j)
    max_elem:=-1;
    row_num:=-1;  //при попытке заменить такие элем. выругается на index out of bounds
    col_num:=-1;
    for i:=j to fNumOfVars-1 do
      for k:=j to fNumOfEqs-1 do
        if GetLengthSquared(fmatrix[i,k])>max_elem then begin
          max_elem:=GetLengthSquared(fmatrix[i,k]);
          row_num:=k;
          col_num:=i;
        end;
    if max_elem<=ftolerance then begin  //сплошь одни нули, не можем новый диаг. элем найти
      for i:=j to fNumOfEqs-1 do
        if GetLengthSquared(fmatrix[fNumOfVars,i])>ftolerance then begin
          fstatus:=slNoSolution;  //получилось уравнение вида 0=1 - все тлен
          Exit;
        end;
      fNumOfEqs:=j; //несколько нижних уравнений имеют вид 0=0 - выкидываем их
      break;
    end;
    SwitchRows(j,row_num);
    SwitchCols(j,col_num);
    //элем (j,j) - лучший из лучших!
    ratio:=fmatrix[j,j]; //чтобы знак не потерять, вверху же абс. знач
    if GetLengthSquared(ratio-1)>ftolerance then begin
      ratio:=1/ratio;
      fmatrix[j,j]:=1;
      for i:=j+1 to fNumOfVars do
        fmatrix[i,j]:=fmatrix[i,j]*ratio;
    end;

    //вычитаем строку из нижних, чтобы получить нули в столбце j
    for i:=j+1 to fNumOfEqs-1 do begin
      if GetLengthSquared(fmatrix[j,i])<=ftolerance then continue; //довольно частый случай
      ratio:=fmatrix[j,i];
      fmatrix[j,i]:=0;
      for k:=j+1 to fNumOfVars do
        fmatrix[k,i]:=fmatrix[k,i]-ratio*fmatrix[k,j];
    end;
  end;
  //получаем матрицу, с ед. диаг. элементами от 0-го до (fNumOfEqs-1)-го
  //с нулевыми элем. в нижнем треугольнике.
  if fNumOfEqs=fNumOfVars then begin
    fstatus:=slOneSolution;
    SolveOneSolution;
  end
  else begin
    fstatus:=slManySolutions;
    SolveManySolutions;
  end;
end;

procedure TSimpleGaussLEQ.SwitchRows(row1,row2: Integer);
var i: Integer;
begin
  if row1<>row2 then
    for i:=0 to fNumOfVars do
      SwapVariants(fmatrix[i,row1],fmatrix[i,row2]);
end;

procedure TSimpleGaussLEQ.SwitchCols(col1,col2: Integer);
var i: Integer;
begin
  if col1<>col2 then begin
    SwapIntegers(fInvIndexes[col1],fInvIndexes[col2]);
    SwapIntegers(findexes[fInvIndexes[col1]],findexes[fInvIndexes[col2]]);
    for i:=0 to fNumOfEqs-1 do
      SwapVariants(fmatrix[col1,i],fmatrix[col2,i]);
  end;
end;

procedure TSimpleGaussLEQ.SolveOneSolution;
var i,j: Integer;
    val: Variant;
begin
  for j:=fNumOfEqs-1 downto 0 do begin
    val:=fmatrix[fNumOfVars,j];
    for i:=j+1 to fNumOfVars-1 do
      val:=val-fvariables[i]*fmatrix[i,j];
    fvariables[j]:=val;
  end;
end;


procedure TSimpleGaussLEQ.SolveManySolutions;
var i,j: Integer;
    val: TManySolutionsDataType;
begin
  for j:=fNumOfEqs to fNumOfVars-1 do begin
    val:=TManySolutionsDataType.Create;
    SetLength(val.Vars,fNumOfVars-fNumOfEqs);
    SetLength(val.VarNames,fNumOfVars-fNumOfEqs);
    val.Vars[j-fNumOfEqs]:=1;
    for i:=fNumOfEqs to fNumOfVars-1 do
      if fvariableNames[i]<>'' then val.VarNames[i-fNumOfEqs]:=fvariableNames[i];
    fvariables[j]:=VarManySolutionsDataCreate(val);
  end;
  SolveOneSolution;
end;

(*
      TManySolutionsDataType
                                  *)
procedure TManySolutionsDataType.Assign(Source: TPersistent);
var s: TManySolutionsDataType absolute Source;
begin
  if Source is TManySolutionsDataType then begin
    Vars:=Copy(s.Vars);
    InitValue:=s.InitValue;
  end
  else
    inherited Assign(Source);
end;

function TManySolutionsDataType.Multiplier(value: Real): string;
begin
  if abs(value-1)<=tolerance then
    Result:=''
  else
    Result:=FloatToStr(value)+'*';
end;

function TManySolutionsDataType.GetString: string;
var i: Integer;
begin
  if abs(InitValue)<=tolerance then Result:=''
  else Result:=FloatToStr(InitValue);
  for i:=0 to Length(vars)-1 do begin
    if abs(vars[i])<=tolerance then continue;
    if vars[i]>0 then
      if Result='' then Result:=Multiplier(vars[i])
      else Result:=Result+'+'+Multiplier(vars[i])
    else Result:=Result+'-'+Multiplier(-vars[i]);
    if (i>=Length(varNames)) or (varNames[i]='') then
      Result:=Result+'a'+IntToStr(i+1)
    else
      Result:=Result+varNames[i];
  end;
  if Result='' then Result:='0';
end;

function TManySolutionsDataType.IsPlainNumber: Boolean;
var i: Integer;
begin
  Result:=true;
  for i:=0 to Length(vars)-1 do
    if abs(vars[i])>tolerance then begin
      Result:=false;
      break;
    end;
end;
(*
function TManySolutionsDataType.AreProportional(other: TManySolutionDataType): Boolean;
var i: Integer;
begin

end;
*)
procedure TManySolutionsDataType.Add(value: TManySolutionsDataType);
var i: Integer;
begin
  InitValue:=InitValue+value.InitValue;
  if Length(value.Vars)>Length(vars) then
    SetLength(vars,Length(value.Vars));
  for i:=0 to Length(value.Vars)-1 do
    Vars[i]:=Vars[i]+value.vars[i];
end;

procedure TManySolutionsDataType.Sub(value: TManySolutionsDataType);
var i: Integer;
begin
  InitValue:=InitValue-value.InitValue;
  if Length(value.Vars)>Length(vars) then
    SetLength(vars,Length(value.Vars));
  for i:=0 to Length(value.Vars)-1 do
    Vars[i]:=Vars[i]-value.vars[i];
end;

procedure TManySolutionsDataType.Mul(value: Real);
var i: Integer;
begin
  InitValue:=InitValue*value;
  for i:=0 to Length(Vars)-1 do
    Vars[i]:=Vars[i]*value;
end;

procedure TManySolutionsDataType.Mul(value: TManySolutionsDataType);
begin
  if value.IsPlainNumber then Mul(value.InitValue)
  else raise Exception.Create('ManySolutionsDataType: multiplication of 2 fundamental solutions is unacceptable!');
end;

procedure TManySolutionsDataType.Divide(value: Real);
var i: Integer;
begin
  InitValue:=InitValue/value;
  for i:=0 to Length(Vars)-1 do
    Vars[i]:=Vars[i]/value;
end;

procedure TManySolutionsDataType.Divide(value: TManySolutionsDataType);
begin
  if value.IsPlainNumber then Divide(value.InitValue)
  else raise Exception.Create('ManySolutionsDataType: division of 2 fundamental solutions is unacceptable!');
end;


(*
      TManySolutionsVariantType
                                    *)
procedure TManySolutionsVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  with TManySolutionsVarData(Dest) do begin
    dest.VType:=VarType;
    ref:=TManySolutionsDataType.Create;
    //либо строка, либо целое, либо с плавающей точкой.
    case Source.VType of
      varSmallInt: Ref.InitValue:=Source.VSmallInt;
      varInteger: Ref.InitValue:=Source.VInteger;
      varSingle: Ref.InitValue:=Source.VSingle;
      varDouble: Ref.InitValue:=Source.VDouble;
      varCurrency: Ref.InitValue:=Source.VCurrency;
      varShortInt: Ref.InitValue:=Source.VShortInt;
      varByte: Ref.InitValue:=Source.VByte;
      varWord: Ref.InitValue:=Source.VWord;
      varLongWord: Ref.InitValue:=Source.VLongWord;
      varInt64: Ref.InitValue:=Source.VInt64;
//      varstring:  //а так ли уж нужно? или строго его и сделать?
      else
        ref.Free;
        RaiseCastError;
    end;
  end;
end;

procedure TManySolutionsVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var LTemp: TVarData;
begin
  with TManySolutionsVarData(Source) do begin
    if Source.VType = VarType then //бывает еще не определенный Variant
      case AVarType of
        varOleStr:
          VarDataFromOleStr(Dest, Ref.GetString);
        varString:
          VarDataFromStr(Dest, Ref.GetString);
      else
        VarDataInit(LTemp);
        try
          VarDataFromStr(Ltemp,Ref.GetString);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
          VarDataClear(LTemp);
        end;
      end
    else
      inherited;
  end;
end;

procedure TManySolutionsVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
begin
if Right.VType=VarType then
  if Left.VType=varString then
    if operator=opAdd then
      Variant(Left):=Variant(Left)+TManySolutionsVarData(Right).Ref.GetString
    else RaiseInvalidOp
  else if Left.VType=varType then
    case operator of
      opAdd: TManySolutionsVarData(Left).Ref.Add(TManySolutionsVarData(Right).Ref);
      opSubtract: TManySolutionsVarData(Left).Ref.Sub(TManySolutionsVarData(Right).Ref);
      opMultiply: TManySolutionsVarData(Left).Ref.Mul(TManySolutionsVarData(Right).Ref);
      opDivide: TManySolutionsVarData(Left).Ref.Divide(TManySolutionsVarData(Right).Ref);
      else RaiseInvalidOp;
    end
    else RaiseInvalidOp
else
  RaiseInvalidOp;
end;

function TManySolutionsVariantType.RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  RequiredVarType := VarType;
  Result := True;
end;

function TManySolutionsVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;
  Result := True;
end;

procedure TManySolutionsVariantType.UnaryOp(var Right: TVarData; const Operator: TVarOp);
begin
//имеет смысл только для '-'
  if (Right.VType=VarType) and (Operator=opNegate) then
    TManySolutionsVarData(Right).Ref.Mul(-1)
  else
    RaiseInvalidOp;
end;

procedure TManySolutionsVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TManySolutionsVarData(Dest) do
    begin
      VType := VarType;
      Ref:=TManySolutionsDataType.Create;
      Ref.Assign(TManySolutionsVarData(Source).Ref);
    end;
end;

procedure TManySolutionsVariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  TManySolutionsVarData(V).Ref.free;
end;

(*
    TSimpleGaussLEQForKirhgof
                                  *)
constructor TSimpleGaussLEQForKirhgof.Create;
begin
  inherited Create;
  fSolver:=TSimpleGaussLEQ.Create;
  fList:=TInterfaceList.Create;
end;

destructor TSimpleGaussLEQForKirhgof.Destroy;
begin
  fList.Free;
  fSolver.Free;
  inherited Destroy;
end;

procedure TSimpleGaussLEQForKirhgof.Solve;
var i: Integer;
begin
  fSolver.Solve;
  for i:=0 to flist.Count-1 do
    IEquationNode(flist[i]).SetValue(fSolver.GetVariable(i));
end;

procedure TSimpleGaussLEQForKirhgof.SetTolerance(value: Real);
begin
  fSolver.SetTolerance(value);
end;

procedure TSimpleGaussLEQForKirhgof.AddEquation(vars: TVariableForEqArray; equals: Variant);
var i,j: Integer;
begin
  if Length(vars)=0 then begin
    if GetLengthSquared(equals)>fsolver.ftolerance then
      //0=1 - сразу нет решений
      Raise Exception.Create('AddEquation: 0=1 type adding, it''s absurd')
    else
      //0=0 - игнорируем
      Exit;
  end;
  fSolver.SetDimensions(fSolver.fNumOfVars,fSolver.fNumOfEqs+1);
  for i:=0 to Length(vars)-1 do begin
    j:=fList.IndexOf(vars[i].reference);
    if j=-1 then begin
      j:=fList.Add(vars[i].reference);
      fSolver.SetDimensions(fSolver.fNumOfVars+1,fSolver.fNumOfEqs);
      fSolver.fVariableNames[j]:=vars[i].reference.ShowNodeName;
    end;
    fSolver.SetMatrix(j,fSolver.fNumOfEqs-1,vars[i].coeff);
  end;
  fSolver.SetMatrix(fSolver.fNumOfVars,fSolver.fNumOfEqs-1,equals);
end;

function TSimpleGaussLEQForKirhgof.GetVariable(p: IEquationNode): Variant;
begin
  Result:=fSolver.GetVariable(fList.IndexOf(p));
end;

function TSimpleGaussLEQForKirhgof.GetStatus: TSLEQStatus;
begin
  Result:=fSolver.GetStatus;
end;

function TSimpleGaussLEQForKirhgof.GetEquationsAsString: string;
var i,j: Integer;
    notfirst: boolean;
begin
  Result:='';
  for j:=0 to fsolver.fNumOfEqs-1 do begin
    notfirst:=false;
    for i:=0 to fsolver.fNumOfVars-1 do
      if GetLengthSquared(fsolver.GetMatrix(i,j))>fsolver.ftolerance then begin
        if (not VarIsNumeric(fsolver.GetMatrix(i,j))) or ((fsolver.GetMatrix(i,j)>0) and notfirst) then
          Result:=Result+'+'
        else if  VarIsNumeric(fsolver.GetMatrix(i,j)) and (fsolver.GetMatrix(i,j)<0) then
          Result:=Result+'-';
        notfirst:=true;
        Result:=Result+fsolver.GetVariableName(i);
        if VarIsNumeric(fsolver.GetMatrix(i,j)) and ((abs(fsolver.GetMatrix(i,j))-1)>fsolver.ftolerance) then
          Result:=Result+'*'+FloatToStr(abs(fsolver.GetMatrix(i,j)))
        else if not VarIsNumeric(fsolver.GetMatrix(i,j)) then
          Result:=Result+'*'+fsolver.GetMatrix(i,j);
      end;
    Result:=Result+'='+VarToStr(fsolver.GetMatrix(fsolver.fNumOfVars,j))+';'+#13#10;
  end;
end;

function TSimpleGaussLEQForKirhgof.GetSolutionAsString: string;
var i: Integer;
    s: string;
begin
  if fsolver.GetStatus=slNoSolution then
    result:='No solution'
  else begin
  (*
    Result:='(';
    for i:=0 to fsolver.fNumOfVars-1 do begin
      s:=fsolver.GetVariable(i);
      Result:=Result+s;
      if i<fsolver.fNumOfVars-1 then Result:=Result+';';
    end;
    Result:=Result+')';
    *)
    Result:='';
    for i:=0 to fsolver.fNumOfVars-1 do begin
      s:=fsolver.GetVariable(i);
      Result:=Result+IEquationNode(flist[i]).ShowValue(fsolver.GetVariable(i));
      if i<fsolver.fNumOfVars-1 then Result:=Result+#13#10;
    end;
  end;
end;

procedure TSimpleGaussLEQForKirhgof.SetRootComponent(c: TComponent);
begin
  fRootComponent:=c;
end;

(*
      TAnalysis
                    *)
constructor TAnalysis.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fVarsOfInterest:=TStreamableComponentList.Create(self);
  fVarsOfInterest.SetSubComponent(true);
  fPrimary:=TSweep.Create(self);
  fPrimary.SetSubComponent(true);
  fSecondary:=TSweep.Create(self);
  fSecondary.SetSubComponent(true);
end;



(*
    Фабрики рабочим!
                          *)
function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;
begin
  VarClear(Result);
  TManySolutionsVarData(Result).VType:=ManySolutionsVariantType.VarType;
  TManySolutionsVarData(Result).Ref:=data;
end;

function GetLengthSquared(value: Variant): Real;
begin
  if VarIsNumeric(value) then
   result:=Sqr(value)
  else if VarIsComplex(value) then
    result:=VarComplexAbsSqr(value)
  else
    result:=0;
end;

function RegisterSimulationType(description: string): TSimulationType;
var L: Integer;
begin
  Result:=AnalysisTypes.Add(description);
end;

function AnalysisTypeToName(Int: LongInt; var Ident: string): Boolean;
begin
  Result:=(Int>=0) and (Int<AnalysisTypes.Count);
  if Result then Ident:=AnalysisTypes[Int];
end;

function NameToAnalysisType(const Ident: string; var Int: LongInt): Boolean;
begin
  Int:=AnalysisTypes.IndexOf(ident);
  Result:=(Int>0);
end;

initialization
  ManySolutionsVariantType:=TManySolutionsVariantType.Create;
  RegisterClasses([TAnalysis]);
  AnalysisTypes:=TStringList.Create;
  RegisterIntegerConsts(TypeInfo(TSimulationType),NameToAnalysisType,AnalysisTypeToName);
  stUndefined:=RegisterSimulationType('Undefined');
  stTransient:=RegisterSimulationType('Transient');
  stAC:=RegisterSimulationType('AC');
  stDC:=RegisterSimulationType('DC');

finalization
  FreeAndNil(ManySolutionsVariantType);
  FreeAndNil(AnalysisTypes);
end.
