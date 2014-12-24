unit linear_eq;

interface

uses Variants,classes,streamable_component_list,streaming_class_lib;

type

TSLEQStatus = (slOneSolution,slNoSolution,slManySolutions);

IEquationNode=interface //���������� ������� ���� ��������, ���� �� ������������
['{5E7FBCDD-61C6-4860-8AFC-F8B2F47B439E}']
//��������� ������� ���. ���������
  function ShowNodeName: string;
  procedure SetValue(value: Variant);
  function GetValue: Variant;
  function ShowValue(value: Variant): string;
  property value: Variant read GetValue write SetValue;
end;

TVariableForEq=record
  reference: IEquationNode;
  coeff: Variant; //����� ���� ����������� �����
end;

TVariableForEqArray=array of TVariableForEq;

TSimulationType = -$7FFFFFFF-1..$7FFFFFFF;

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

IObjectForAnalysis=interface
  ['{303E2364-6CBD-4AAB-BFE1-7C728F0BAF6A}']
  procedure RunSimulation(SimulationType: TSimulationType);
  function isSeparationEnabledBy(variable: IEquationNode): Boolean;
  //��, ����� � ������� ���� ������ ���� IEquationNode, ����� ������� ���. ���� ���� � ��.
  function Implementor: TStreamingClass;
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

TSweep = class (TComponent)
  private
    fEnabled,fIsLog: Boolean;
    fVariable: IEquationNode;
    fMinVal,fMaxVal,fIncr: Real;
  public
    function NumberOfPoints: Integer;
    function GetPoint(index: Integer): Real;
  published
    property Enabled: Boolean read fEnabled write fEnabled default false;
    property Variable: IEquationNode read fVariable write fVariable;
    property MinVal: Real read fMinVal write fMinVal;
    property MaxVal: Real read fMaxVal write fMaxVal;
    property Incr: Real read fIncr write fIncr;
    //���� ���., �� Incr ����� ����� ����� �� ������ ��� ����� �� ������ (����� ��� � �������)
    property isLog: Boolean read fIsLog write fIsLog default false;
end;

TAnalysis = class (TStreamingClass)
  private
    fSimulationType: TSimulationType;
    fVarsOfInterest: TStreamableComponentList;
    fSweeps: array [0..1] of TSweep;
    fOrigin: TAnalysis; //�� ������ ���� �����
    fThread: TThread; //�����, � ��� �����������
    fIndex: Integer; //����� �����
    fProgress: array of Integer;  //�������� ���������� � �������
    fClones: array of TAnalysis;
    offsets: array of Integer;
    fsweepIndex: Integer;

    fData: array of array of array of Variant; //�������� ����������� �����, � ����� ���������.
    fPrimaryCount,fSecondaryCount: Integer; //�� ��� �� ������������ � ����� � � ������
    procedure RunThread(Origin: TAnalysis; index: Integer);
    procedure AppendThreadResults(clone: TAnalysis; exMessage: string ='');
  protected
    procedure OnThreadTerminate(Sender: TObject); //���������� ����� ���� �������� �� �������
    procedure ShowProgress(index,value: Integer);
  public
    onReady: TNotifyEvent;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Run;  //������� ����� �����, ��������� ��������� sweep ��� ������� � ��������� � ��� ������
    procedure SaveToTextFile(FileName: string);
  published
    property SimulationType: TSimulationType read fSimulationType write fSimulationType;
    property VarsOfInterest: TStreamableComponentList read fVarsOfInterest write fVarsOfInterest;
    property PrimarySweep: TSweep read fSweeps[0] write fSweeps[0];
    property SecondarySweep: TSweep read fSweeps[1] write fSweeps[1];
end;

TAnalysisThread = class (TThread)
  private
    fAnalysis: TAnalysis;
    fObject: IObjectForAnalysis;
    fPercentDone: Integer;
    fExceptionMessage: string;
  protected
    procedure Execute; override;
    procedure Progress;
  public
    constructor Create(aAnalysis: TAnalysis);
end;

function VarManySolutionsDataCreate(data: TManySolutionsDataType): Variant;

function GetLengthSquared(value: Variant): Real;

function RegisterSimulationType(description: string): TSimulationType;

var stTransient,stAC,stDC, stUndefined : TSimulationType;
    NumberOfAnalysisThreads: Integer;

implementation

uses sysUtils,varCmplx,math,command_class_lib;

var ManySolutionsVariantType: TManySolutionsVariantType;
    AnalysisTypes: TStrings;
//    AllThreadsStopped: TEvent;

(*
    ������� �������!
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
    end;  //�������� ����� ������ �� ���. �������
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
  //������������ ���� � ������ ������������
  for j:=0 to fNumOfEqs-1 do begin
    //���� ����. ���� �� ���� � �� ����� (j,j)
    max_elem:=-1;
    row_num:=-1;  //��� ������� �������� ����� ����. ���������� �� index out of bounds
    col_num:=-1;
    for i:=j to fNumOfVars-1 do
      for k:=j to fNumOfEqs-1 do
        if GetLengthSquared(fmatrix[i,k])>max_elem then begin
          max_elem:=GetLengthSquared(fmatrix[i,k]);
          row_num:=k;
          col_num:=i;
        end;
    if max_elem<=ftolerance then begin  //������ ���� ����, �� ����� ����� ����. ���� �����
      for i:=j to fNumOfEqs-1 do
        if GetLengthSquared(fmatrix[fNumOfVars,i])>ftolerance then begin
          fstatus:=slNoSolution;  //���������� ��������� ���� 0=1 - ��� ����
          Exit;
        end;
      fNumOfEqs:=j; //��������� ������ ��������� ����� ��� 0=0 - ���������� ��
      break;
    end;
    SwitchRows(j,row_num);
    SwitchCols(j,col_num);
    //���� (j,j) - ������ �� ������!
    ratio:=fmatrix[j,j]; //����� ���� �� ��������, ������ �� ���. ����
    if GetLengthSquared(ratio-1)>ftolerance then begin
      ratio:=1/ratio;
      fmatrix[j,j]:=1;
      for i:=j+1 to fNumOfVars do
        fmatrix[i,j]:=fmatrix[i,j]*ratio;
    end;

    //�������� ������ �� ������, ����� �������� ���� � ������� j
    for i:=j+1 to fNumOfEqs-1 do begin
      if GetLengthSquared(fmatrix[j,i])<=ftolerance then continue; //�������� ������ ������
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
      //0=1 - ����� ��� �������
      Raise Exception.Create('AddEquation: 0=1 type adding, it''s absurd')
    else
      //0=0 - ����������
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



function GetObjectForAnalysis(obj: TComponent): IObjectForAnalysis;
begin
  while assigned(obj) do begin
    if obj.GetInterface(IObjectForAnalysis,Result) then Exit;
    obj:=obj.Owner;
  end;
  Raise Exception.Create('GetObjectForAnalysis: didn''t find IObjectForAnalysis');
end;

(*
      TSweep
                    *)
function TSweep.NumberOfPoints: Integer;
var numberOfDecades: Integer;
    pow,range: Real;
begin
  if enabled then
    if isLog then
      if Incr>0 then begin  //����� �� ������
        numberOfDecades:=1;
        pow:=10;
        range:=MaxVal/MinVal;
        while range>=pow do begin
          inc(numberOfDecades);
          pow:=pow*10;
        end;
        Result:=numberOfDecades*Round(incr)+1;
      end
      else begin
        numberOfDecades:=1;
        pow:=2;
        range:=MaxVal/MinVal;
        while range>pow do begin
          inc(numberOfDecades);
          pow:=pow*2;
        end;
        Result:=numberOfDecades*Round(-incr)+1;
      end
    else Result:=Ceil((MaxVal-MinVal)/Incr+1)
  else Result:=1; //����� �� ����� ���������� �����
end;

function TSweep.GetPoint(index: Integer): Real;
begin
  if enabled then
    if isLog then
      if Incr>0 then
        Result:=MinVal*power(10,index/incr)
      else
        Result:=MinVal*power(2,index/(-incr))
    else
      Result:=MinVal+index*Incr
  else Raise Exception.Create('TSweep.GetPoint: can''t return value when disabled');
end;

(*
      TAnalysis
                    *)
constructor TAnalysis.Create(Owner: TComponent);
var i: Integer;
begin
  inherited Create(Owner);
  fVarsOfInterest:=TStreamableComponentList.Create(self);
  fVarsOfInterest.SetSubComponent(true);
  fVarsOfInterest.Name:='VarsOfInterest';
  for i:=0 to 1 do begin
    fSweeps[i]:=TSweep.Create(self);
    fSweeps[i].SetSubComponent(true);
  end;
end;

destructor TAnalysis.Destroy;
var i: Integer;
begin
  FreeAndNil(fThread);
  for i:=0 to Length(fclones)-1 do
    if Assigned(fclones[i]) and Assigned(fclones[i].fThread) then
      fclones[i].fThread.Terminate;

  //�� ��� �� �� ������ ���������!
  inherited Destroy;
end;

procedure TAnalysis.Run;
var clone: TStreamingClass;
    source: TStreamingClass;
    numOfPoints: Integer;
    i,count,pointsPerThread: Integer;
    our_copy: TAnalysis;
begin
  SetLength(fdata,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,VarsOfInterest.Count);
  fPrimaryCount:=0;
  fSecondaryCount:=0;
  //� ������� �� ������ �� ������� ���� ������������?
  if SecondarySweep.Enabled then fsweepIndex:=1 else fsweepIndex:=0;
  //���������� ��������� - ���������� �� ������ �� ������ ���������� sweep
  numOfPoints:=fSweeps[fsweepIndex].NumberOfPoints;
  count:=min(NumberOfAnalysisThreads,numOfPoints);
  SetLength(fProgress,count);
  SetLength(fClones,count);
  SetLength(offsets,count);
  pointsPerThread:=Round(NumOfPoints/NumberOfAnalysisThreads);
  source:=GetObjectForAnalysis(self).Implementor;

  for i:=0 to count-1 do begin
    clone:=TStreamingClass.CloneComponent(source) as TStreamingClass;
    //������ ���� � ��� ���� � ������ ���������
    our_copy:=clone.FindComponent(Name) as TAnalysis;
    our_copy.fSweeps[fsweepIndex].MinVal:=fSweeps[fsweepIndex].GetPoint(i*pointsPerThread);
    offsets[i]:=i*pointsPerThread;
    if i=count-1 then
      our_copy.fSweeps[fsweepIndex].MaxVal:=fSweeps[fsweepIndex].GetPoint(NumOfPoints-1)
    else
      our_copy.fSweeps[fsweepIndex].MaxVal:=fSweeps[fsweepIndex].GetPoint((i+1)*pointsPerThread-1);

    clone.saveFormat:=fCyr;
    clone.SaveToFile('clone'+IntToStr(i)+'.txt');

    fClones[i]:=our_copy;
    our_copy.RunThread(self,i); //�����������, � ����� ������� ��� �����, ����� ����������
  end;
end;


procedure TAnalysis.RunThread(origin: TAnalysis; index: Integer);
begin
  fOrigin:=origin;
  fIndex:=index;
  fThread:=TAnalysisThread.Create(self);
end;

procedure TAnalysis.OnThreadTerminate(Sender: TObject);
var ExMessage: string;
begin
  ExMessage:=(fThread as TAnalysisThread).fExceptionMessage;
  fThread:=nil;
  fOrigin.AppendThreadResults(self,ExMessage);
end;

procedure TAnalysis.ShowProgress(index,value: Integer);
var i: Integer;
    s: string;
begin
  fProgress[index]:=value;
  for i:=0 to Length(fProgress)-1 do begin
    if fProgress[i]<0 then
      s:=s+'Err'
    else
      s:=s+IntToStr(fProgress[i])+'%';
    if i<Length(fProgress)-1 then
      s:=s+'  ';
  end;

  if FindOwner is TAbstractDocument then
    TAbstractDocument(FindOwner).DoneStatusPanel.Text:=s;
end;

procedure TAnalysis.AppendThreadResults(clone: TAnalysis; exMessage: string = '');
var i,j,k: Integer;
    isReady: Boolean;
    obj: TComponent;
    iobj: IObjectForAnalysis;
begin
  fPrimaryCount:=offsets[clone.fIndex];
  fSecondaryCount:=0;
  if fSweepIndex=1 then SwapIntegers(fPrimaryCount,fSecondaryCount);
  if exMessage='' then begin
    //������������� ������
    for i:=0 to clone.PrimarySweep.NumberOfPoints-1 do
      for j:=0 to clone.SecondarySweep.NumberOfPoints-1 do
        for k:=0 to clone.VarsOfInterest.Count-1 do
          if not VarIsEmpty(clone.fData[i,j,k]) then
            fdata[i+fPrimaryCount,j+fSecondaryCount,k]:=clone.fData[i,j,k];
  end;
  //GetObjectForAnalysis(clone).Implementor.Free; //��, ��� � ��������� ����
//�������� ����� ����� �������
  iobj:=GetObjectForAnalysis(clone); //����� ����� ������� ����� ����� ����� ���� ������
  obj:=iobj.Implementor;
  iobj:=nil; //���������� ���������� �������� ������ �� 1, ����������...
  obj.Free; //����� � �������
//������, �������� ������� ����� ������������ ������
  isReady:=true;
  for i:=0 to Length(fclones)-1 do begin
    if clone=fclones[i] then
      fclones[i]:=nil;
    if fclones[i]<>nil then isReady:=false;
  end;
  if isReady and Assigned(onReady) then OnReady(self);
(*
  if exMessage<>'' then
    Raise Exception.Create(ExMessage);
    *)
end;

procedure TAnalysis.SaveToTextFile(FileName: string);
var F: TextFile;
    s,v,strType: string;
    i,j,k: Integer;
begin
  AssignFile(F,FileName);
  try
    Rewrite(F);
    WriteLn(F,name);
    if AnalysisTypeToName(fSimulationType,strType) then
      WriteLn(F,'type: '+strType);
    if SecondarySweep.Enabled then begin
      WriteLn(F,'Secondary sweep: enabled');
      WriteLn(F,'Variable: '+SecondarySweep.variable.ShowNodeName);
      s:='Values: '+FloatToStr(SecondarySweep.fMinVal)+' .. '+FloatToStr(SecondarySweep.fMaxVal)+', step '+FloatToStr(SecondarySweep.fIncr);
      if SecondarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
    end;
      WriteLn(F,'Primary sweep:');
      WriteLn(F,'Variable: '+PrimarySweep.variable.ShowNodeName);
      s:='Values: '+FloatToStr(PrimarySweep.fMinVal)+' .. '+FloatToStr(PrimarySweep.fMaxVal)+', step '+FloatToStr(PrimarySweep.fIncr);
      if PrimarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
  //��������� �����
  for i:=0 to SecondarySweep.NumberOfPoints-1 do begin
    if SecondarySweep.Enabled then
      WriteLn(F,SecondarySweep.variable.ShowValue(SecondarySweep.GetPoint(i)));
    for j:=0 to PrimarySweep.NumberOfPoints-1 do begin
      s:=FloatToStr(PrimarySweep.GetPoint(j))+#9;
      for k:=0 to VarsOfInterest.Count-1 do begin
        v:=fdata[j,i,k];
        s:=s+v+#9;
      end;
      WriteLn(F,s);
    end;
  end;



  finally
    CloseFile(F);
  end;
end;


(*
        TAnalysisThread
                            *)
constructor TAnalysisThread.Create(aAnalysis: TAnalysis);
begin
  inherited Create(true);
  fAnalysis:=aAnalysis;
  fObject:=GetObjectForAnalysis(fAnalysis);
  FreeOnTerminate:=true;
  onTerminate:=fAnalysis.OnThreadTerminate;
//  Priority:=tpIdle;
  Resume;
end;

procedure TAnalysisThread.Execute;
var i,j,k: Integer;
    node: IEquationNode;
begin
  try
//������� ���� - �� secondarySweep, ���� �� ����
  with fAnalysis do begin
    SetLength(fData,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,VarsOfInterest.Count);
    for j:=0 to SecondarySweep.NumberOfPoints-1 do begin
      if SecondarySweep.Enabled then
        SecondarySweep.Variable.SetValue(SecondarySweep.GetPoint(j));
      for i:=0 to fAnalysis.PrimarySweep.NumberOfPoints-1 do begin
        if PrimarySweep.Enabled then
          PrimarySweep.Variable.SetValue(PrimarySweep.GetPoint(i));
        fObject.RunSimulation(SimulationType);
        for k:=0 to VarsOfInterest.Count-1 do
          if VarsOfInterest[k].GetInterface(IEquationNode,node) then begin
            if VarIsEmpty(node.value) then Raise Exception.CreateFMT('TAnalysisThread.Execute: unassigned value in node %s', [node.ShowNodeName]);
            fData[i,j,k]:=node.value;
          end;
        fPercentDone:=Round((j+i/PrimarySweep.NumberOfPoints)/SecondarySweep.NumberOfPoints*100);
        Synchronize(Progress);
      end;
    end;
  end;
  fPercentDone:=100;
  Synchronize(Progress);
  fObject:=nil;
  except
    on Ex: Exception do
      fExceptionMessage:= Ex.Message;
  end;
end;

procedure TAnalysisThread.Progress;
begin
  if fExceptionMessage<>'' then fPercentDone:=-1;
  fAnalysis.fOrigin.ShowProgress(fAnalysis.fIndex,fPercentDone);
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
  NumberOfAnalysisThreads:=4;
finalization
  FreeAndNil(ManySolutionsVariantType);
  FreeAndNil(AnalysisTypes);
end.
