unit linear_eq;

interface

uses Variants,classes;

type

TSLEQStatus = (slOneSolution,slNoSolution,slManySolutions);

IAbstractSLEQ=interface
  procedure SetDimensions(NumOfVars,NumOfEqs: Integer);
  procedure SetMatrix(i,j: Integer; value: Real);
  procedure SetTolerance(value: real);
  function GetMatrix(i,j: Integer): Real;
  function GetVariable(i: Integer): Variant;
  function GetStatus: TSLEQStatus;
  procedure Solve;
  property Matrix[i,j: Integer]: Real read GetMatrix write SetMatrix;
end;


TManySolutionsDataType = class(TPersistent)
 public
  Vars: array of Real;
  procedure Assign(Source: TPersistent); override;
end;


TManySolutionsVarData = packed record
  VType: TVarType;
  InitValue: Extended;
  Ref: TManySolutionsDataType;  //вот все и влезло!
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
    fmatrix: array of array of Real;
    fIndexes: array of Integer;
    fVariables: array of Variant;
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
    procedure SetMatrix(i,j: Integer; value: Real);
    procedure SetTolerance(value: real);
    function GetMatrix(i,j: Integer): Real;
    function GetVariable(i: Integer): Variant;
    function GetStatus: TSLEQStatus;
    procedure Solve;
    property Matrix[i,j: Integer]: Real read GetMatrix write SetMatrix;
end;



implementation

uses streaming_class_lib,sysUtils; //ради SwapFloats

var ManySolutionsVariantType: TManySolutionsVariantType;

(*
      TSimpleGaussLEQ
                              *)

procedure TSimpleGaussLEQ.SetDimensions(NumOfVars,NumOfEqs: Integer);
var i: Integer;
begin
  SetLength(fmatrix,NumOfVars+1,NumOfEqs);
  SetLength(findexes,NumOfVars);
  SetLength(fvariables,NumOfVars);
  fNumOfVars:=NumOfVars;
  fNumOfEqs:=NumOfEqs;
  for i:=0 to fNumOfVars-1 do
    findexes[i]:=i;
end;

procedure TSimpleGaussLEQ.SetMatrix(i,j: Integer; value: Real);
begin
  fmatrix[i,j]:=value;
end;

procedure TSimpleGaussLEQ.SetTolerance(value: Real);
begin
  ftolerance:=value;
end;

function TSimpleGaussLEQ.GetMatrix(i,j: Integer): Real;
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

procedure TSimpleGaussLEQ.Solve;
var i,j,k: Integer;
  max_elem: Real;
  row_num,col_num: Integer;
  ratio: Real;
begin
  //обеспечиваем нули в нижнем треугольнике
  for j:=0 to fNumOfEqs-1 do begin
    //ищем макс. элем не выше и не левее (j,j)
    max_elem:=-1;
    row_num:=-1;  //при попытке заменить такие элем. выругается на index out of bounds
    col_num:=-1;
    for i:=j to fNumOfVars-1 do
      for k:=j to fNumOfEqs-1 do
        if abs(fmatrix[i,k])>max_elem then begin
        max_elem:=abs(fmatrix[j,i]);
        row_num:=k;
        col_num:=i;
        end;
    if max_elem<=ftolerance then begin  //сплошь одни нули, не можем новый диаг. элем найти
      for i:=j to fNumOfEqs-1 do
        if abs(fmatrix[fNumOfVars,i])>ftolerance then begin
          fstatus:=slNoSolution;  //получилось уравнение вида 0=1 - все тлен
          Exit;
        end;
      fNumOfEqs:=j; //несколько нижних уравнений имеют вид 0=0 - выкидываем их
      break;
    end;
    SwitchRows(j,row_num);
    SwitchCols(j,col_num);
    //элем (j,j) - лучший из лучших!
    max_elem:=fmatrix[j,j]; //чтобы знак не потерять, вверху же абс. знач
    if abs(max_elem-1)>ftolerance then begin
      ratio:=1/max_elem;
      fmatrix[j,j]:=1;
      for i:=j+1 to fNumOfVars do
        fmatrix[i,j]:=fmatrix[i,j]*ratio;
    end;

    //вычитаем строку из нижних, чтобы получить нули в столбце j
    for i:=j+1 to fNumOfEqs-1 do begin
      if abs(fmatrix[j,i])<=ftolerance then continue; //довольно частый случай
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
      SwapFloats(fmatrix[i,row1],fmatrix[i,row2]);
end;

procedure TSimpleGaussLEQ.SwitchCols(col1,col2: Integer);
var i: Integer;
begin
  if col1<>col2 then begin
    SwapIntegers(findexes[col1],findexes[col2]);
    for i:=0 to fNumOfEqs-1 do
      SwapFloats(fmatrix[col1,i],fmatrix[col2,i]);
  end;
end;

procedure TSimpleGaussLEQ.SolveOneSolution;
var i,j: Integer;
    val: Real;
begin
  for j:=fNumOfEqs-1 downto 0 do begin
    val:=fmatrix[fNumOfVars,j];
    for i:=j+1 to fNumOfVars-1 do
      val:=val-fvariables[i]*fmatrix[i,j];
    fvariables[j]:=val;
  end;
end;


procedure TSimpleGaussLEQ.SolveManySolutions;
begin


end;

(*
      TManySolutionsDataType
                                  *)
procedure TManySolutionsDataType.Assign(Source: TPersistent);
var s: TManySolutionsDataType absolute Source;
begin
  if Source is TManySolutionsDataType then
    Vars:=Copy(s.Vars)
  else
    inherited Assign(Source);
end;



(*
      TManySolutionsVariantType
                                    *)
procedure TManySolutionsVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  //либо строка, либо целое, либо с плавающей точкой.
//  case Source.VType of


end;

procedure TManySolutionsVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin


end;

procedure TManySolutionsVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
begin


end;

function TManySolutionsVariantType.RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin

end;

function TManySolutionsVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin

end;

procedure TManySolutionsVariantType.UnaryOp(var Right: TVarData; const Operator: TVarOp);
begin


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


initialization
  ManySolutionsVariantType:=TManySolutionsVariantType.Create;
finalization
  FreeAndNil(ManySolutionsVariantType);
end.
