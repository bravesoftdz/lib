unit linear_eq;

interface

uses classes,variants,VariantWrapper;

type

TSLEQStatus = (slOneSolution,slNoSolution,slManySolutions);

IEquationNode=interface //отобразить красиво свое значение, чтоб не заморачивать
['{5E7FBCDD-61C6-4860-8AFC-F8B2F47B439E}']
//интерфейс решения лин. уравнений
  function ShowNodeName: string;
  function ShowNodeUnit: string;
  procedure SetValue(value: Variant);
  function GetValue: Variant;
  function ShowValue(value: Variant): string;
  property value: Variant read GetValue write SetValue;
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
  function GetInvMatrix(i,j: Integer): Variant;
  function GetVariable(i: Integer): Variant;
  procedure SetVariableName(i: Integer; value: string);
  function GetVariableName(i: Integer): string;
  function GetStatus: TSLEQStatus;
  procedure Solve;
  procedure InvertMatrix;
  property Matrix[i,j: Integer]: Variant read GetMatrix write SetMatrix;
  property InvMatrix[i,j: Integer]: Variant read GetInvMatrix;
  property VariableName[i: Integer]: string read GetVariableName write SetVariableName;
end;

TManySolutionsDataType = class(TAbstractWrapperData)
protected
  function Multiplier(value: Real): string;
public
  InitValue: Real;
  Vars: array of Real;
  tolerance: Real;
  VarNames: array of string;
  procedure Assign(Source: TPersistent); override;
  procedure DoAdd(value: TAbstractWrapperData); override;
  procedure DoSubtract(right: TAbstractWrapperData); override;
  procedure Mul(value: Real);
  procedure DoMultiply(right: TAbstractWrapperData); override;
  procedure Divide(value: Real);
  procedure DoDivide(right: TAbstractWrapperData); override;
  procedure Negate; override;
  function IsPlainNumber: boolean;
//  function AreProportional(other: TManySolutionsDataType): boolean;
//  procedure SetString(value: string);
  function AsString: string; override;
end;

TManySolutionsVariantType=class(TAbstractWrapperVariantType)
public
  procedure Cast(var Dest: TVarData; const Source: TVarData); override;
  procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
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
    finvMatrix: array of array of Variant;
  protected
    procedure SwitchRows(row1,row2: Integer);
    procedure SwitchCols(col1,col2: Integer);
    procedure FullSwitchRows(row1,row2: Integer);
    procedure SolveOneSolution;
    procedure SolveManySolutions;
    procedure SolveInvOneSolution;
    procedure SolveInvManySolutions;
  public
    destructor Destroy; override; //for debug purposes
    procedure SetDimensions(NumOfVars,NumOfEqs: Integer);
    procedure SetMatrix(i,j: Integer; value: Variant);
    procedure SetTolerance(value: real);
    function GetMatrix(i,j: Integer): Variant;
    function GetInvMatrix(i,j: Integer): Variant;
    function GetVariable(i: Integer): Variant;
    function GetStatus: TSLEQStatus;
    procedure SetVariableName(i: Integer; value: string);
    function GetVariableName(i: Integer): string;
    procedure Solve;
    procedure InvertMatrix;
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

function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;
function VarIsManySolutions(V: Variant): Boolean;
function VarManySolutionsIsNumber(V: Variant): Boolean;

function GetLengthSquared(value: Variant): Real;

implementation

uses varCmplx,streaming_class_lib,sysUtils,new_phys_unit_lib;

var ManySolutionsVariantType: TManySolutionsVariantType;

//    AllThreadsStopped: TEvent;

(*
    Фабрики рабочим!
                          *)
function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;
begin
  VarClear(Result);
  TWrapperVarData(Result).VType:=ManySolutionsVariantType.VarType;
  TWrapperVarData(Result).data:=data;
end;

function VarIsManySolutions(V: Variant): Boolean;
begin
  Result:=TWrapperVarData(V).VType=ManySolutionsVariantType.VarType;
end;

function VarManySolutionsIsNumber(V: Variant): Boolean;
begin
  Result:=VarIsNumeric(V) or (VarIsManySolutions(V) and
    (TWrapperVarData(V).Data as TManySolutionsDataType).IsPlainNumber);
end;

function GetLengthSquared(value: Variant): Real;
begin
  if VarIsNumeric(value) then
   result:=Sqr(value)
  else if VarIsComplex(value) then
    result:=VarComplexAbsSqr(value)
  else Result:=value.GetLengthSquared;  //если не поддерживается - выругается, делов-то!
end;

(*
      TSimpleGaussLEQ
                              *)
destructor TSimpleGaussLEQ.Destroy;
begin
  inherited Destroy;
end;

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

function TSimpleGaussLEQ.GetInvMatrix(i,j: Integer): Variant;
begin
  //i здесь - номер уравнения по сути
  //j - номер переменной, до их перепутывания
  if findexes[j]>=fNumOfEqs then
    Result:=0.0
  else
    Result:=finvmatrix[i,findexes[j]];
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
    if (not IsDimensionless(ratio)) or (GetLengthSquared(ratio-1)>ftolerance) then begin
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

procedure TSimpleGaussLEQ.InvertMatrix;
var i,j,k: Integer;
  max_elem: Real;
  row_num,col_num: Integer;
  ratio: Variant;
begin
  SetLength(finvMatrix,fNumOfEqs,fNumOfEqs);
  for i:=0 to fNumOfEqs-1 do
    for j:=0 to fNumOfEqs-1 do
      if i=j then finvMatrix[i,j]:=1.0
      else fInvMatrix[i,j]:=0.0;
  //преобразуем исходную матрицу в единичную
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
    FullSwitchRows(j,row_num);
    SwitchCols(j,col_num);
    //элем (j,j) - лучший из лучших!
    ratio:=fmatrix[j,j]; //чтобы знак не потерять, вверху же абс. знач
    if GetLengthSquared(ratio-1)>ftolerance then begin
      ratio:=1/ratio;
      fmatrix[j,j]:=1;
      for i:=j+1 to fNumOfVars do
        fmatrix[i,j]:=fmatrix[i,j]*ratio;
      for i:=0 to fNumOfEqs-1 do
        finvmatrix[i,j]:=finvmatrix[i,j]*ratio;        
    end;

    //вычитаем строку из всех прочих, чтобы получить нули в столбце j
    for i:=0 to fNumOfEqs-1 do
      if i<>j then begin
        if GetLengthSquared(fmatrix[j,i])<=ftolerance then continue; //довольно частый случай
        ratio:=fmatrix[j,i];
        fmatrix[j,i]:=0;
        for k:=j+1 to fNumOfVars do
          fmatrix[k,i]:=fmatrix[k,i]-ratio*fmatrix[k,j];
        for k:=0 to fNumOfEqs-1 do
          finvmatrix[k,i]:=finvmatrix[k,i]-ratio*finvmatrix[k,j];
      end;
  end;
  //получаем единичную матрицу слева (возможно, нулевые строки и столбцы)
  if fNumOfEqs=fNumOfVars then begin
    fstatus:=slOneSolution;
    SolveInvOneSolution;
  end
  else begin
    fstatus:=slManySolutions;
    SolveInvManySolutions;
  end;
end;

procedure TSimpleGaussLEQ.SwitchRows(row1,row2: Integer);
var i: Integer;
begin
  if row1<>row2 then
    for i:=0 to fNumOfVars do
      SwapVariants(fmatrix[i,row1],fmatrix[i,row2]);
end;

procedure TSimpleGaussLEQ.FullSwitchRows(row1,row2: Integer);
var i: Integer;
begin
  if row1<>row2 then begin
    for i:=0 to fNumOfEqs-1 do
      SwapVariants(finvMatrix[i,row1],finvmatrix[i,row2]);
    SwitchRows(row1,row2);
  end;
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

procedure TSimpleGaussLEQ.SolveInvOneSolution;
begin
  SolveOneSolution;
end;

procedure TSimpleGaussLEQ.SolveInvManySolutions;
begin
  SolveManySolutions;
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

function TManySolutionsDataType.AsString: string;
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
procedure TManySolutionsDataType.DoAdd(value: TAbstractWrapperData);
var i: Integer;
    v: TManySolutionsDataType absolute value;
begin
  if value is TManySolutionsDataType then begin
    InitValue:=InitValue+v.InitValue;
    if Length(v.Vars)>Length(vars) then
      SetLength(vars,Length(v.Vars));
    for i:=0 to Length(v.Vars)-1 do
      Vars[i]:=Vars[i]+v.vars[i];
  end
  else inherited;
end;

procedure TManySolutionsDataType.DoSubtract(right: TAbstractWrapperData);
var i: Integer;
    v: TManySolutionsDataType absolute right;
begin
  if right is TManySolutionsDataType then begin
    InitValue:=InitValue-v.InitValue;
    if Length(v.Vars)>Length(vars) then
      SetLength(vars,Length(v.Vars));
    for i:=0 to Length(v.Vars)-1 do
      Vars[i]:=Vars[i]-v.vars[i];
  end
  else inherited;
end;

procedure TManySolutionsDataType.Mul(value: Real);
var i: Integer;
begin
  InitValue:=InitValue*value;
  for i:=0 to Length(Vars)-1 do
    Vars[i]:=Vars[i]*value;
end;

procedure TManySolutionsDataType.Negate;
begin
  Mul(-1);
end;

procedure TManySolutionsDataType.DoMultiply(right: TAbstractWrapperData);
var v: TManySolutionsDataType absolute right;
begin
  if right is TManySolutionsDataType then begin
    if v.IsPlainNumber then Mul(v.InitValue)
    else raise Exception.Create('ManySolutionsDataType: multiplication of 2 fundamental solutions is unacceptable!');
  end
  else inherited;
end;

procedure TManySolutionsDataType.Divide(value: Real);
var i: Integer;
begin
  InitValue:=InitValue/value;
  for i:=0 to Length(Vars)-1 do
    Vars[i]:=Vars[i]/value;
end;

procedure TManySolutionsDataType.DoDivide(right: TAbstractWrapperData);
var v: TManySolutionsDataType absolute right;
begin
  if right is TManySolutionsDataType then begin
    if v.IsPlainNumber then Divide(v.InitValue)
    else raise Exception.Create('ManySolutionsDataType: division of 2 fundamental solutions is unacceptable!');
  end
  else inherited;
end;


(*
      TManySolutionsVariantType
                                    *)
procedure TManySolutionsVariantType.Cast(var Dest: TVarData; const Source: TVarData);
var d: TManySolutionsDataType;
begin
  with TWrapperVarData(Dest) do begin
    dest.VType:=VarType;
    d:=TManySolutionsDataType.Create;
    //либо строка, либо целое, либо с плавающей точкой.
    case Source.VType of
      varSmallInt: d.InitValue:=Source.VSmallInt;
      varInteger: d.InitValue:=Source.VInteger;
      varSingle: d.InitValue:=Source.VSingle;
      varDouble: d.InitValue:=Source.VDouble;
      varCurrency: d.InitValue:=Source.VCurrency;
      varShortInt: d.InitValue:=Source.VShortInt;
      varByte: d.InitValue:=Source.VByte;
      varWord: d.InitValue:=Source.VWord;
      varLongWord: d.InitValue:=Source.VLongWord;
      varInt64: d.InitValue:=Source.VInt64;
//      varstring:  //а так ли уж нужно? или строго его и сделать?
      else
        d.Free;
        RaiseCastError;
    end;
    data:=d;
  end;
end;

procedure TManySolutionsVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var LTemp: TVarData;
begin
  with TWrapperVarData(Source) do begin
    if Source.VType = VarType then //бывает еще не определенный Variant
      case AVarType of
        varOleStr:
          VarDataFromOleStr(Dest, data.AsString);
        varString:
          VarDataFromStr(Dest, data.AsString);
      else
        VarDataInit(LTemp);
        try
          VarDataFromStr(Ltemp,data.AsString);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
          VarDataClear(LTemp);
        end;
      end
    else
      inherited;
  end;
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
//  for i:=0 to fSolver.fNumOfVars-1 do
//    fSolver.SetMatrix(i,fSolver.fNumOfEqs-1,);
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


initialization
  ManySolutionsVariantType:=TManySolutionsVariantType.Create;
finalization
  FreeAndNil(ManySolutionsVariantType);
end.
