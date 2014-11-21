unit VariantMatrixLib;

interface
uses streaming_class_lib,variants,TypInfo,SysUtils,classes;

function VarMatrix: TVarType;

type

TMatrix=class(TStreamingClass)
private
  fRows,fCols: Integer;
  fData: array of Real; //одномерный как-то надежнее, можно ускорить выполнение
  fTolerance: Real;
  procedure SetData(i,j: Integer; Value: Real);
  function GetData(i,j: Integer): Real;

  function GetAsString: string;
  procedure SetAsString(text: string);

  procedure SwitchRows(row1,row2: Integer);
  procedure SwitchCols(col1,col2: Integer);

protected

public
  constructor Create(owner: TComponent); overload; override;
  constructor Create(text: string); reintroduce; overload;
  constructor CopyFrom(const AData: TMatrix);
  constructor CreateUnity(size: Integer);
  constructor CreateZero(rows,cols: Integer);
  procedure Assign(source: TPersistent); override;

  function isNumber: Boolean;
  function isVector: Boolean;

  procedure SetSize(rows,cols: Integer); //тупо меняет размеры, не заботясь о сохранности данных
  procedure Resize(rows,cols: Integer); //данные сохраняются, справа и снизу появл. нули
  procedure DoAdd(term: TMatrix);
  procedure DoSubtract(term: TMatrix);
  procedure DoMultiply(term: TMatrix);
//  procedure DoLeftMultiply(term: TMatrix);
  procedure Invert;
  function CreateInvertedMatrix: TMatrix;
  property Data[i,j: Integer]: Real read GetData write SetData; default;
published
  property AsString: string read GetAsString write SetAsString;
  property Rows: Integer read fRows;
  property Cols: Integer read fCols;
  property Tolerance: Real read fTolerance write fTolerance;
end;

TMatrixVarData = packed record
  VType: TVarType;
  Reserved1, Reserved2, Reserved3: Word;
  VMatrix: TMatrix;
  Reserved4: LongInt;
end;

TMatrixVariantType=class(TPublishableVariantType)
protected
  function GetInstance(const V: TVarData): TObject; override;
  function LeftPromotion(const V: TVarData; const Operator: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
  procedure Simplify(var V: TVarData);
public
  function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
  procedure Clear(var V: TVarData); override;
  procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
  procedure Cast(var Dest: TVarData; const Source: TVarData); override;
  procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
end;


function VarMatrixCreate(rows,cols: Integer): Variant; overload;
function VarMatrixCreate(text: string): Variant; overload;
function VarMatrixCreate(matrix: TMatrix): Variant; overload;
function VarUnityMatrix(size: Integer): Variant;
function VarXRotationMatrix(angle: Real): Variant;
function VarYRotationMatrix(angle: Real): Variant;
function VarZRotationMatrix(angle: Real): Variant;


implementation

uses simple_parser_lib,vector_lib;

var
MatrixVariantType: TMatrixVariantType;

function TMatrixVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result:=TMatrixVarData(V).VMatrix;
end;

procedure TMatrixVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TMatrixVarData(V).VMatrix);
end;

procedure TMatrixVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TMatrixVarData(Dest) do
    begin
      VType := VarType;
      VMatrix := TMatrix.CopyFrom(TMatrixVarData(Source).VMatrix);
    end;
end;

procedure TMatrixVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
  matrix: TMatrix;
begin
  matrix:=TMatrixVarData(Source).VMatrix;
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, matrix.AsString);
      varString:
        VarDataFromStr(Dest, matrix.AsString);
      varSingle,varDouble,varCurrency,varInteger:
        if matrix.isNumber then begin
          VarDataInit(LTemp);
          LTemp.VType:=varDouble;
          LTemp.VDouble:=Matrix[1,1];
          VarDataCastTo(Dest,LTemp,AVarType);
        end
        else RaiseCastError;
    else

      if (AVarType=VarVector) and Matrix.isVector then
        Variant(Dest):=VarVectorCreate(matrix[1,1],matrix[1,2],matrix[1,3])
      else begin
        VarDataInit(LTemp);
        try
          VarDataFromStr(Ltemp,TMatrixVarData(Source).VMatrix.AsString);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
          VarDataClear(LTemp);
        end;
      end;
    end
  else
    inherited;
end;

procedure TMatrixVariantType.Cast(var Dest: TVarData; const Source: TVarData);
var matrix: TMatrix;
    vector: TVector;
begin
  matrix:=TMatrix.CreateZero(1,1);
  TMatrixVarData(Dest).VMatrix:=matrix;
  case Source.VType of
    varDouble: matrix[1,1]:=Source.VDouble;
    varSingle: matrix[1,1]:=Source.VSingle;
    varCurrency: matrix[1,1]:=Source.VCurrency;
    varInteger: matrix[1,1]:=Source.VInteger;
  else
    if Source.VType=VarVector then begin
      matrix.SetSize(3,1);
      vector:=TVectorVarData(source).VVector;
      matrix[1,1]:=vector.X;
      matrix[1,2]:=vector.Y;
      matrix[1,3]:=vector.Z;
    end
    else TMatrixVarData(Dest).VMatrix.AsString:=VarDataToStr(Source);
  end;
  Dest.VType:=varType;
end;

procedure TMatrixVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
begin
  if Right.VType = VarType then
    case Left.VType of
      varString:
      case Operator of
        opAdd:
        Variant(Left) := Variant(Left) + TMatrixVarData(Right).VMatrix.AsString;
      else
        RaiseInvalidOp;
      end;
    else
      if Left.VType = VarType then
        case Operator of
          opAdd: begin
            TMatrixVarData(Left).VMatrix.DoAdd(TMatrixVarData(Right).VMatrix);
            Simplify(Left);
            end;
          opSubtract: begin
            TMatrixVarData(Left).VMatrix.DoSubtract(TMatrixVarData(Right).VMatrix);
            Simplify(Left);
            end;
          opMultiply: begin
            TMatrixVarData(Left).VMatrix.DoMultiply(TMatrixVarData(Right).VMatrix);
            Simplify(Left);
            end;
        else
          RaiseInvalidOp;
        end
      else
        RaiseInvalidOp;
    end
  else
    RaiseInvalidOp;
end;

function TMatrixVariantType.DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  if (Name='CELLS') and (Length(Arguments)=2) then begin
    Dest.VType:=varDouble;
    Dest.VDouble:=TMatrixVarData(V).VMatrix[Variant(Arguments[0]),Variant(Arguments[1])];
    Result:=true;
  end
  else
    Result:=false;
end;

function TMatrixVariantType.LeftPromotion(const V: TVarData;
  const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  { TypeX Op Complex }
  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;

  Result := True;
end;

procedure TMatrixVariantType.Simplify(var V: TVarData);
var matrix: TMatrix;
begin
//попытаемся привести к более простому виду
  matrix:=TMatrixVarData(V).VMatrix;
  if matrix.isNumber then CastTo(V,V,varDouble);
  if matrix.isVector then CastTo(V,V,varVector);


end;
  (*
              TMatrix
                                  *)
procedure TMatrix.Assign(source: TPersistent);
var s: TMatrix absolute source;
begin
  if source is TMatrix then begin
    fCols:=s.fCols;
    fRows:=s.fRows;
    fData:=Copy(s.fData);
  end
  else inherited Assign(source);
end;

constructor TMatrix.Create(owner: TComponent);
begin
  inherited Create(owner);
  SetSubComponent(true);
end;

constructor TMatrix.CopyFrom(const AData: TMatrix);
begin
  Create(nil);
  Assign(AData);
end;

constructor TMatrix.Create(text: string);
begin
  Create(nil);
  AsString:=text;
end;

constructor TMatrix.CreateZero(rows,cols: Integer);
var i: Integer;
begin
  Create(nil);
  SetSize(rows,cols);
  for i:=0 to Length(fData)-1 do fData[i]:=0;
end;

constructor TMatrix.CreateUnity(size: Integer);
var i: Integer;
begin
  CreateZero(size,size);
  for i:=1 to size do Data[i,i]:=1;
end;

procedure TMatrix.SetSize(rows,cols: Integer);
begin
  frows:=rows;
  fcols:=cols;
  SetLength(fdata,frows*fcols);
end;

procedure TMatrix.Resize(rows,cols: Integer);
var temp: TMatrix;
    i,j: Integer;
begin
  temp:=TMatrix.Create(self);
  SetSize(rows,cols);
  for j:=1 to temp.fRows do
    for i:=1 to temp.fCols do
      Data[i,j]:=temp[i,j];
  temp.Free;
end;


procedure TMatrix.SetData(i,j: Integer; value: Real);
begin
  fData[i-1+(j-1)*fCols]:=value;
end;

function TMatrix.GetData(i,j: Integer): Real;
begin
  Result:=fData[i-1+(j-1)*fCols];
end;


function TMatrix.GetAsString: string;
var s: string;
    i,j: Integer;
begin
  s:='(';
  for j:=1 to fRows do begin
    s:=s+'(';
    for i:=1 to fCols do begin
      s:=s+FloatToStr(Data[i,j]);
      if i<fCols then s:=s+';';
    end;
    s:=s+')';
    if j<fRows then s:=s+';';
  end;
  s:=s+')';
  Result:=s;
end;

procedure TMatrix.SetAsString(text: string);
var P: TSimpleParser;
    i,j,k: Integer;
    c: char;
begin
  P:=TSimpleParser.Create(text);
  p.delimiter:='';
  c:=P.NextChar;
  if c<>'(' then begin
    //одно-единственное число
    SetSize(1,1);
    Data[1,1]:=P.getFloat;
    Exit;
  end;
  //начинается с открывающей скобочки
  P.getChar;
  i:=1;
  j:=1; //начальные размеры
  SetSize(1,1);
  repeat
    c:=P.nextChar;
    if c=')' then break;
    if c=';' then begin
      P.getChar;
      inc(j);
      Resize(j,i);
      continue;
    end;
    if c='(' then begin
      //вложенная скобка
      P.getChar; //пропускаем скобку
      k:=1;
      repeat
        c:=P.NextChar;
        if c=')' then begin
          P.getChar;
          break;
        end;
        if c=';' then begin
          P.getChar;
          inc(k);
          if k>i then begin
            i:=k;
            Resize(j,i);
          end;
          continue;
        end
        else Data[k,j]:=P.getFloat;
      until false;
    end
    else Data[1,j]:=P.getFloat;
  until false;

end;

function TMatrix.isNumber: Boolean;
begin
  Result:=(fCols=1) and (fRows=1);
end;

function TMatrix.isVector: Boolean;
begin
  Result:=(fCols=1) and (fRows=3);
end;

procedure TMatrix.DoAdd(term: TMatrix);
var i,j: Integer;
begin
  if (fCols=term.fCols) and (fRows=term.fRows) then begin
    for j:=1 to fRows do
      for i:=1 to fCols do
        Data[i,j]:=Data[i,j]+term[i,j];
  end
  else Raise Exception.Create('TMatrix.DoAdd: sizes of matrices are not equal');
end;

procedure TMatrix.DoSubtract(term: TMatrix);
var i,j: Integer;
begin
  if (fCols=term.fCols) and (fRows=term.fRows) then begin
    for j:=1 to fRows do
      for i:=1 to fCols do
        Data[i,j]:=Data[i,j]-term[i,j];
  end
  else Raise Exception.Create('TMatrix.DoSubtract: sizes of matrices are not equal');
end;

procedure TMatrix.DoMultiply(term: TMatrix); //умножение на матрицу справа
var i,j,k: Integer;
    temp: TMatrix;
    x: Real;
begin
  if (fCols=term.fRows) then begin
    temp:=TMatrix.CopyFrom(self);
    SetSize(fRows,term.fCols);
    for j:=1 to fRows do begin
      for i:=1 to fCols do begin
        data[i,j]:=0;
        for k:=1 to term.fRows do data[i,j]:=data[i,j]+temp[k,j]*term[i,k];
      end;
    end;
    temp.Free;
  end
  else if term.isNumber then begin
    x:=term[1,1];
    for j:=1 to fRows do
      for i:=1 to fCols do
        data[i,j]:=data[i,j]*x;
  end
  else if isNumber then begin
    x:=data[1,1];
    Assign(term);
    for j:=1 to fRows do
      for i:=1 to fCols do
        data[i,j]:=data[i,j]*x;
  end
  else Raise Exception.Create('TMatrix.DoMultiply: wrong sizes');
end;

procedure TMatrix.Invert;
var temp: TMatrix;
begin
  temp:=CreateInvertedMatrix;
  fdata:=Copy(temp.fData);
  temp.Free;
end;

function TMatrix.CreateInvertedMatrix: TMatrix;
var temp: TMatrix;
    i,j,k: Integer;
    max_elem: Real;
    row_num,col_num: Integer;
    ratio: Real;
begin
  if frows<>fcols then Raise Exception.Create('TMatrix.CreateInvertedMatrix: source matrix is non-square');
  Result:=TMatrix.CreateUnity(frows);
  temp:=TMatrix.Clone(self);
  try
    for j:=1 to frows do begin
      //ищем макс. элемент не выше и не левее (j,j)
(*
      max_elem:=-1;
      row_num:=-1;  //при попытке заменить такие элем. выругается на index out of bounds
      col_num:=-1;
      for i:=j to frows do
        for k:=j to frows do
          if abs(temp[i,k])>max_elem then begin
            max_elem:=abs(temp[j,i]);
            row_num:=k;
            col_num:=i;
          end;
      if max_elem<=ftolerance then  //сплошь одни нули, не можем новый диаг. элем найти
        raise Exception.Create('TMatrix.CreateInvertedMatrix: source matrix has zero determinant');
      temp.SwitchRows(j,row_num);
      Result.SwitchRows(j,row_num);
      temp.SwitchCols(j,col_num);
      Result.SwitchCols(j,col_num);
      //элем (j,j) - лучший из лучших!
      *)
      max_elem:=temp[j,j]; //чтобы знак не потерять, вверху же абс. знач
      if abs(max_elem-1)>ftolerance then begin
        ratio:=1/max_elem;
(*
        data[j,j]:=1;
        for i:=j+1 to fRows do
          data[i,j]:=data[i,j]*ratio;
*)
        for i:=1 to fRows do begin
          temp[i,j]:=temp[i,j]*ratio;
          Result.data[i,j]:=Result.data[i,j]*ratio;
        end;
      end;
      //теперь вычитаем нашу строку из всех остальных, чтобы остались нули в данном столбце
      for i:=1 to frows do begin
        if (i=j) or (abs(temp[j,i])<=ftolerance) then continue; //довольно частый случай
        ratio:=temp[j,i];
(*
        data[j,i]:=0;
        for k:=j+1 to fRows do
          data[k,i]:=data[k,i]-ratio*data[k,j];
          *)
         for k:=1 to fRows do begin
          temp[k,i]:=temp[k,i]-ratio*temp[k,j];
          Result.data[k,i]:=Result.data[k,i]-ratio*Result.data[k,j];
         end;
      end;

    end;
  finally
    temp.Free;
  end;
end;

procedure TMatrix.SwitchRows(row1,row2: Integer);
var i: Integer;
    bias1,bias2: Integer;
begin
  //data[i,j] <=> fData[i-1+(j-1)*fCols]
  //и нумерация от 1 до rows/cols
  if row1<>row2 then begin
    bias1:=(row1-1)*fCols-1;
    bias2:=(row2-1)*fCols-1;
    for i:=1 to fcols do
      SwapFloats(fdata[i+bias1],fdata[i+bias2]);
//    SwapFloats(data[i,row1],data[i,row2]);
  end;

end;

procedure TMatrix.SwitchCols(col1,col2: Integer);
var i: Integer;
    bias1,bias2: Integer;
begin
  if col1<>col2 then begin
    bias1:=col1-1-fcols;
    bias2:=col2-1-fcols;
    for i:=1 to frows do
      SwapFloats(fdata[i*fcols+bias1],fdata[i*fcols+bias2]);
  end;
end;




(*
          'Fabric'
                        *)


procedure VarMatrixCreateInto(var ADest: Variant; const AMatrix: TMatrix);
begin
  VarClear(ADest);
  TMatrixVarData(ADest).VType := VarMatrix;
  TMatrixVarData(ADest).VMatrix := AMatrix;
end;

function VarMatrixCreate(rows,cols: Integer): Variant;
begin
  VarMatrixCreateInto(Result,TMatrix.CreateZero(rows,cols));
end;

function VarMatrixCreate(text: string): Variant;
begin
  VarMatrixCreateInto(Result,Tmatrix.Create(text));
end;

function VarMatrixCreate(matrix: TMatrix): Variant;
begin
  VarMatrixCreateInto(Result,Tmatrix.CopyFrom(matrix));
end;

function VarUnityMatrix(size: Integer): Variant;
begin
  VarMatrixCreateInto(Result,TMatrix.CreateUnity(size));
end;

function VarMatrix: TVarType;
begin
  Result:=MatrixVariantType.VarType;
end;

function VarXRotationMatrix(angle: Real): Variant;
var m: TMatrix;
    si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  m:=TMatrix.CreateUnity(3);
  m[2,2]:=co;
  m[3,3]:=co;
  m[3,2]:=-si;
  m[2,3]:=si;
  VarMatrixCreateInto(Result,m);
end;

function VarYRotationMatrix(angle: Real): Variant;
var m: TMatrix;
    si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  m:=TMatrix.CreateUnity(3);
  m[1,1]:=co;
  m[3,3]:=co;
  m[3,1]:=si;
  m[1,3]:=-si;
  VarMatrixCreateInto(Result,m);
end;

function VarZRotationMatrix(angle: Real): Variant;
var m: TMatrix;
    si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  m:=TMatrix.CreateUnity(3);
  m[1,1]:=co;
  m[2,2]:=co;
  m[2,1]:=-si;
  m[1,2]:=si;
  VarMatrixCreateInto(Result,m);
end;

initialization
RegisterClass(TMatrix);
MatrixVariantType:=TMatrixVariantType.Create;

finalization
FreeAndNil(MatrixVariantType);

end.
