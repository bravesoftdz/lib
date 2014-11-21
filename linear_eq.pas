unit linear_eq;

interface

uses Variants,classes;

type

TSLEQStatus = (slOneSolution,slNoSolution,slManySolutions);

TVariableForEq=record
  reference: Pointer;
  coeff: Variant; //����� ���� ����������� �����
end;

TVariableForEqArray=array of TVariableForEq;

IKirhgofSLEQ=interface
  procedure AddEquation(vars: TVariableForEqArray; equals: Variant);
  procedure SetTolerance(value: real);
  function GetVariable(p: Pointer): Variant;
  function GetStatus: TSLEQStatus;
  procedure Solve;
end;

IAbstractSLEQ=interface
  procedure SetDimensions(NumOfVars,NumOfEqs: Integer);
  procedure SetMatrix(i,j: Integer; value: Real);
  procedure SetTolerance(value: real);
  function GetMatrix(i,j: Integer): Real;
  function GetVariable(i: Integer): Variant;
  procedure SetVariableName(i: Integer; value: string);
  function GetVariableName(i: Integer): string;
  function GetStatus: TSLEQStatus;
  procedure Solve;
  property Matrix[i,j: Integer]: Real read GetMatrix write SetMatrix;
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
  Ref: TManySolutionsDataType;  //����� ���������� �����, �� ��������
  Reserved4: LongInt; //������� ��� � ������ �������.
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
    procedure SetMatrix(i,j: Integer; value: Real);
    procedure SetTolerance(value: real);
    function GetMatrix(i,j: Integer): Real;
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
    fList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEquation(vars: TVariableForEqArray; equals: Variant);
    procedure SetTolerance(value: real);
    function GetVariable(p: Pointer): Variant;
    function GetStatus: TSLEQStatus;
    procedure Solve;
  end;

function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;

implementation

uses streaming_class_lib,sysUtils; //���� SwapFloats

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
  SetLength(fVariableNames,NumOfVars);
  if NumOfVars>fNumOfVars then
    for i:=0 to NumOfEqs-1 do begin
      fmatrix[NumOfVars,i]:=fmatrix[fNumOfVars,i];
      fmatrix[fNumOfVars,i]:=0;
    end;
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
  ratio: Real;
begin
  //������������ ���� � ������ ������������
  for j:=0 to fNumOfEqs-1 do begin
    //���� ����. ���� �� ���� � �� ����� (j,j)
    max_elem:=-1;
    row_num:=-1;  //��� ������� �������� ����� ����. ���������� �� index out of bounds
    col_num:=-1;
    for i:=j to fNumOfVars-1 do
      for k:=j to fNumOfEqs-1 do
        if abs(fmatrix[i,k])>max_elem then begin
          max_elem:=abs(fmatrix[i,k]);
          row_num:=k;
          col_num:=i;
        end;
    if max_elem<=ftolerance then begin  //������ ���� ����, �� ����� ����� ����. ���� �����
      for i:=j to fNumOfEqs-1 do
        if abs(fmatrix[fNumOfVars,i])>ftolerance then begin
          fstatus:=slNoSolution;  //���������� ��������� ���� 0=1 - ��� ����
          Exit;
        end;
      fNumOfEqs:=j; //��������� ������ ��������� ����� ��� 0=0 - ���������� ��
      break;
    end;
    SwitchRows(j,row_num);
    SwitchCols(j,col_num);
    //���� (j,j) - ������ �� ������!
    max_elem:=fmatrix[j,j]; //����� ���� �� ��������, ������ �� ���. ����
    if abs(max_elem-1)>ftolerance then begin
      ratio:=1/max_elem;
      fmatrix[j,j]:=1;
      for i:=j+1 to fNumOfVars do
        fmatrix[i,j]:=fmatrix[i,j]*ratio;
    end;

    //�������� ������ �� ������, ����� �������� ���� � ������� j
    for i:=j+1 to fNumOfEqs-1 do begin
      if abs(fmatrix[j,i])<=ftolerance then continue; //�������� ������ ������
      ratio:=fmatrix[j,i];
      fmatrix[j,i]:=0;
      for k:=j+1 to fNumOfVars do
        fmatrix[k,i]:=fmatrix[k,i]-ratio*fmatrix[k,j];
    end;
  end;
  //�������� �������, � ��. ����. ���������� �� 0-�� �� (fNumOfEqs-1)-��
  //� �������� ����. � ������ ������������.
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
    //���� ������, ���� �����, ���� � ��������� ������.
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
//      varstring:  //� ��� �� �� �����? ��� ������ ��� � �������?
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
    if Source.VType = VarType then //������ ��� �� ������������ Variant
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
//����� ����� ������ ��� '-'
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
  fList:=TList.Create;
end;

destructor TSimpleGaussLEQForKirhgof.Destroy;
begin
  fList.Free;
  fSolver.Free;
  inherited Destroy;
end;

procedure TSimpleGaussLEQForKirhgof.Solve;
begin
  fSolver.Solve;
end;

procedure TSimpleGaussLEQForKirhgof.SetTolerance(value: Real);
begin
  fSolver.SetTolerance(value);
end;

procedure TSimpleGaussLEQForKirhgof.AddEquation(vars: TVariableForEqArray; equals: Variant);
var i,j: Integer;
begin
  fSolver.SetDimensions(fSolver.fNumOfVars,fSolver.fNumOfEqs+1);
  for i:=0 to Length(vars)-1 do begin
    j:=fList.IndexOf(vars[i].reference);
    if j=-1 then begin
      j:=fList.Add(vars[i].reference);
      fSolver.SetDimensions(fSolver.fNumOfVars+1,fSolver.fNumOfEqs);
    end;
    fSolver.SetMatrix(j,fSolver.fNumOfEqs-1,vars[i].coeff);
  end;
  fSolver.SetMatrix(fSolver.fNumOfVars,fSolver.fNumOfEqs-1,equals);
end;

function TSimpleGaussLEQForKirhgof.GetVariable(p: Pointer): Variant;
begin
  Result:=fSolver.GetVariable(fList.IndexOf(p));
end;

function TSimpleGaussLEQForKirhgof.GetStatus: TSLEQStatus;
begin
  Result:=fSolver.GetStatus;
end;


(*
    ������� �������!
                          *)
function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;
begin
  VarClear(Result);
  TManySolutionsVarData(Result).VType:=ManySolutionsVariantType.VarType;
  TManySolutionsVarData(Result).Ref:=data;
end;

initialization
  ManySolutionsVariantType:=TManySolutionsVariantType.Create;
finalization
  FreeAndNil(ManySolutionsVariantType);
end.
