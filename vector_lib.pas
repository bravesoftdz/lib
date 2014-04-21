unit vector_lib;

interface
uses Classes,streaming_class_lib,sysUtils,simple_parser_lib,quaternion_lib,variants,TypInfo;

type

TVector=class(TStreamingClass)
  private
    fx,fy,fz: Real;
    function vector2str: string;
    procedure str2vector(str: string);
    function getLength: Real;
  public
    constructor Create(owner: TComponent); overload; override;
    constructor Create(ax: Real=0;ay: Real=0;az: Real=0); reintroduce; overload;
    constructor Create(str: string); reintroduce; overload;
    constructor CopyFrom(vector: TVector);

    procedure Clear; override;
    procedure VectorAssign(source: TVector); 
    procedure Assign(source: TPersistent); overload; override;
    procedure Assign(_x,_y,_z: Real); reintroduce; overload;

    procedure add(source: TVector);
    procedure sub(by: TVector);

    procedure normalize;
    procedure negate;

    procedure Mul(by: Real);
    procedure Vector_multiply(by: TVector);
    procedure Ortogonalize(axis: TVector);
    function ProjectionLiesOnVectorItself(axis: TVector): boolean;

    procedure rotateX(angle: Real);
    procedure rotateZ(angle: Real);
    procedure rotateY(angle: Real);

    procedure rotate_by_quat(q: TAbstractQuaternion);

    function Length_squared: Real;

    class function scalar_product(M0,M1: TVector): Real;
    class function cos_between(M0,M1: TVector): Real;
    class function line_distance(M0,M1: TVector): Real;
    class function distance_between(M0,M1: TVector): Real;
    class function distance_squared(M0,M1: TVector): Real;
    property Length: Real read getLength;
  published
    property X: Real read fX write fX;
    property Y: Real read fY write fY;
    property Z: Real read fZ write fZ;
    property Value: string read vector2str write str2vector stored false;

end;

TVectorVarData = packed record
  VType: TVarType;
  Reserved1, Reserved2, Reserved3: Word;
  VVector: TVector;
  Reserved4: LongInt;
end;

TVectorVariantType=class(TPublishableVariantType)
protected
  function GetInstance(const V: TVarData): TObject; override;
  function RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
  function LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
public
  function DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
  procedure Clear(var V: TVarData); override;
  procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
  procedure Cast(var Dest: TVarData; const Source: TVarData); override;
  procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  procedure UnaryOp(var Right: TVarData; const Operator: Integer); override;
  procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
end;


function VarVector: TVarType;
function VarVectorCreate(x: Real=0; y:Real=0; z:Real=0): Variant; overload;
function VarVectorCreate(str: string): Variant; overload;
function VarVectorCreate(vector: TVector): Variant; overload;

function LineDistance(p1,p2: Variant): Real;
function VectorMul(p1,p2: Variant): Variant;
function VectorLength(p: Variant): Real;

implementation

var VectorVariantType: TVectorVariantType;

constructor TVector.Create(owner: TComponent);
begin
  inherited Create(owner);
  SetSubComponent(true);
end;

constructor TVector.Create(ax: Real=0; ay: Real=0; az: Real=0);
begin
  Create(nil);
  x:=ax;
  y:=ay;
  z:=az;
end;

constructor TVector.Create(str: string);
begin
  Create(nil);
  value:=str;
end;

constructor TVector.CopyFrom(vector: TVector);
begin
  Create(nil);
  Assign(vector);
end;

procedure TVector.Clear;
begin
  x:=0;
  y:=0;
  z:=0;
end;

procedure TVector.Assign(source: TPersistent);
var t: TVector;
begin
  if source is TVector then begin
    t:=source as TVector;
    x:=t.x;
    y:=t.y;
    z:=t.z;
  end
  else Inherited Assign(source);
end;

procedure TVector.VectorAssign(source: TVector);
begin
  x:=source.X;
  y:=source.Y;
  z:=source.Z;
end;

procedure TVector.Assign(_x,_y,_z: Real);
begin
  x:=_x;
  y:=_y;
  z:=_z;
end;

function TVector.vector2str: string;
begin
  result:='('+FloatToStr(x)+';'+FloatToStr(y)+';'+FloatToStr(z)+')';
end;

procedure TVector.str2vector(str: string);
var p: TSimpleParser;
    ch: char;
begin
  p:=TSimpleParser.Create(str);
  ch:=p.getChar;
  if ch<>'(' then Raise Exception.Create('Tvector.str2vector: first character is"'+ ch+'" not "("');
  x:=p.getFloat;
  if p.getChar<>';' then Raise Exception.Create('Tvector.str2vector: wrong separator between x and y, must be ";"');
  y:=p.getFloat;
  if p.getChar<>';' then Raise Exception.Create('Tvector.str2vector: wrong separator between y and z, must be ";"');
  z:=p.getFloat;
  if p.getChar<>')' then Raise Exception.Create('Tvector.str2vector: wrong character after z value, must be ")"');
  if not p.eof then Raise Exception.Create('Tvector.str2vector: end of file expected');
  p.Free;
end;

procedure TVector.add(source: TVector);
begin
  x:=x+source.x;
  y:=y+source.y;
  z:=z+source.z;
end;

procedure TVector.sub(by: TVector);
begin
  x:=x-by.x;
  y:=y-by.y;
  z:=z-by.z;
end;

procedure TVector.normalize;
var norm: Real;
begin
  norm:=x*x+y*y+z*z;
  assert(norm>0,name+': vector normalize: zero-length vector');
  norm:=sqrt(norm);
  x:=x/norm;
  y:=y/norm;
  z:=z/norm; 
end;

procedure TVector.negate;
begin
  x:=-x;
  y:=-y;
  z:=-z;
end;

function TVector.getLength: Real;
begin
  result:=sqrt(x*x+y*y+z*z);
end;

function TVector.Length_squared: Real;
begin
  Result:=x*x+y*y+z*z;
end;

procedure TVector.Mul(by: Real);
begin
  x:=x*by;
  y:=y*by;
  z:=z*by;
end;

procedure TVector.Vector_multiply(by: TVector);
var xt,yt: Real;
begin
  xt:=x;
  yt:=y;
  x:=yt*by.z-z*by.y;
  y:=z*by.x-xt*by.z;
  z:=xt*by.y-yt*by.x;
end;
(*
procedure TVector.Ortogonalize(axis: TVector);
var a: Real;
    v: TVector;
begin
  //вычитаем из вектора его проекцию на axis
  a:=TVector.scalar_product(self,axis)/axis.Length_squared;
  v:=TVector.CopyFrom(axis);
  v.Mul(a);
  Sub(v);
  v.Free;
end;
*)  //хреновая версия - возится с памятью на ровном месте, надо сделать только в лок. перем.

procedure TVector.Ortogonalize(axis: TVector);
var a: Real;
begin //этот вариант должен быть существенно быстрее
  a:=TVector.scalar_product(self,axis)/axis.Length_squared;
  x:=x-a*axis.x;
  y:=y-a*axis.Y;
  z:=z-a*axis.Z;
end;


function TVector.ProjectionLiesOnVectorItself(axis: TVector): boolean;
var k: Real;
begin
  //проверяем, что "наш" вектор, давая проекцию на v, ляжет на него, т.е в единицах
  //v будет составлять от 0 до 1 длины
  k:=TVector.scalar_product(self,axis)/axis.Length_squared;
  Result:=(k>=0) and (k<=1);
end;

procedure TVector.rotateX(angle: Real);
var t,si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  t:=y;
  y:=y*co-z*si;
  z:=t*si+z*co;
end;


procedure TVector.rotateZ(angle: Real);
var t,si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  t:=x;
  x:=x*co-y*si;
  y:=t*si+y*co;
end;

procedure TVector.rotateY(angle: Real);
var t,si,co: Real;
begin
  si:=sin(angle);
  co:=cos(angle);
  t:=z;
  z:=z*co-x*si;
  x:=t*si+x*co;
end;


class function TVector.scalar_product(M0,M1: TVector): Real;
begin
  result:=M0.x*M1.x+M0.y*M1.y+M0.z*M1.z;
end;

class function TVector.cos_between(M0,M1: TVector): Real;
begin
  result:=scalar_product(M0,M1)/M0.Length/M1.Length;
end;

class function TVector.distance_between(M0,M1: TVector): Real;
begin
  Result:=Sqrt(sqr(M0.X-M1.X)+Sqr(M0.Y-M1.Y)+Sqr(M0.Z-M1.Z));
end;

class function TVector.distance_squared(M0,M1: TVector): Real;
begin
  Result:=sqr(M0.X-M1.X)+sqr(M0.Y-M1.Y)+sqr(M0.Z-M1.Z);
end;

class function TVector.line_distance(M0,M1: TVector): Real;
//M0, M1 - координаты точек
//найти расстояние от начала координат до отрезка, который их соединяет
var t: TVector;
    k: Real;
begin
  t:=TVector.Create;
  t.Assign(M1);
  t.sub(M0);
  k:=-scalar_product(t,M0)/scalar_product(t,t);
  if k<=0 then result:=M0.Length
  else if k>=1 then result:=M1.Length
    else begin
      t.Mul(-k);
      t.sub(M0);
      result:=t.Length;
    end;
  t.Free;
end;

procedure TVector.rotate_by_quat(q: TAbstractQuaternion);
var t,n: TQuaternion;
begin
  t:=TQuaternion.Create(nil);
  t.Assign(q);
  t.conjugate;
  n:=TQuaternion.Create(nil,0,x,y,z); //наш родной вектор
  t.left_mul(n);
  t.left_mul(q);
  x:=t.x;
  y:=t.y;
  z:=t.z;
  t.Free;
  n.Free;
end;

(*
            TVectorVariantType
                                          *)
function TVectorVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result:=TVectorVarData(V).VVector;
end;

procedure TVectorVariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  FreeAndNil(TVectorVarData(V).VVector);
end;

procedure TVectorVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TVectorVarData(Dest) do
    begin
      VType := VarType;
      VVector := TVector.Create;
      VVector.Assign(TVectorVarData(source).VVector);
    end;
end;

procedure TVectorVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  TVectorVarData(Dest).VVector:=TVector.Create;
  TVectorVarData(Dest).VVector.Value:=VarDataToStr(Source);
  //вряд ли кто-то будет подсовывать числа, а если даже подсунет
  //они преобразуются в строку, а в этой строке вектор по-любому не выйдет.
  Dest.VType:=varType;
end;

procedure TVectorVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then //бывает еще не определенный Variant
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TVectorVarData(Source).VVector.Value);
      varString:
        VarDataFromStr(Dest, TVectorVarData(Source).VVector.Value);
      varSingle,varDouble,varCurrency,varInteger:
        RaiseCastError;
    else
      VarDataInit(LTemp);
      try
        VarDataFromStr(Ltemp,TVectorVarData(Source).VVector.Value);
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end
  else
    inherited;
end;

function TVectorVariantType.RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  if Operator=opMultiply then
    Case V.VType of
      varInteger, varSingle,varDouble,varCurrency,varShortInt,varByte,varWord,varLongWord: RequiredVarType:=V.VType;
    else RequiredVarType:=VarType;
    end
  else RequiredVarType:=VarType;
  Result:=True;
end;

function TVectorVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  if Operator=opMultiply then
    Case V.VType of
      varInteger, varSingle,varDouble,varCurrency,varShortInt,varByte,varWord,varLongWord: RequiredVarType:=V.VType;
    else RequiredVarType:=VarType;
    end
  else  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType:=varString
  else
    RequiredVarType := VarType;

  Result:=True;
end;

procedure TVectorVariantType.UnaryOp(var Right: TVarData; const Operator: Integer);
begin
  if (Right.VType=VarType) and (Operator=opNegate) then
    TVectorVarData(Right).VVector.Mul(-1)
  else
    RaiseInvalidOp;
end;

procedure TVectorVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
var LTemp: TVarData;
begin
  if Right.VType = VarType then
    case Left.VType of
      varString:
        case Operator of
          opAdd:
            Variant(Left) := Variant(Left) + TVectorVarData(Right).VVector.Value;
        else
          RaiseInvalidOp;
        end;

      varInteger, varSingle,varDouble,varCurrency,varShortInt,varByte,varWord,varLongWord:
        case Operator of
          opMultiply:
          begin
            VarDataInit(LTemp);
            try
              VarDataCastTo(LTemp, Left, varDouble);
              Variant(Left):=VarVectorCreate;
              TVectorVarData(Left).VVector.Assign(TVectorVarData(right).VVector);
              TVectorVarData(Left).VVector.Mul(LTemp.VDouble);
            finally
              VarDataClear(LTemp);
            end;
          end;
        else RaiseInvalidOp;
        end;
        else
          if Left.VType = VarType then
            case Operator of
              opAdd:
                TVectorVarData(Left).VVector.Add(TVectorVarData(Right).VVector);
              opSubtract:
                TVectorVarData(Left).VVector.Sub(TVectorVarData(Right).VVector);
              opMultiply:
              //скаляр. умножение
                Variant(Left):=TVector.scalar_product(TVectorVarData(Left).VVector,TVectorVarData(Right).VVector);
//                RaiseInvalidop;
            else
              RaiseInvalidOp;
            end
          else
            RaiseInvalidOp;
    end
  else
    if Operator=opMultiply then begin
      VarDataInit(LTemp);
      try
        VarDataCastTo(LTemp, Right, varDouble);
        TVectorVarData(Left).VVector.Mul(LTemp.VDouble);
      finally
        VarDataClear(LTemp);
      end;
    end
    else RaiseInvalidOp;
end;

function TVectorVariantType.DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result:=true;
  If (Name='ROTATEBYX') and (Length(Arguments)=1) then
    TVectorVarData(V).VVector.rotateX(Variant(Arguments[0]))
  else if (Name='ROTATEBYY') and (Length(Arguments)=1) then
    TVectorVarData(V).VVector.rotateY(Variant(Arguments[0]))
  else if (Name='ROTATEBYZ') and (Length(Arguments)=1) then
    TVectorVarData(V).VVector.rotateZ(Variant(Arguments[0]))
  else if (Name='VECTORMUL') and (Length(Arguments)=1) and (Arguments[0].VType=VarType) then
    TVectorVarData(V).VVector.Vector_multiply(TVectorVarData(Arguments[0]).vvector)
  else if (Name='NORMALIZE') and (Length(Arguments)=0) then
    TVectorVarData(V).VVector.normalize
  else Result:=False;
end;

(*
            'Fabric'
                              *)

function VarVector: TVarType;
begin
  Result:=VectorVariantType.VarType;
end;

procedure VarVectorCreateInto(var ADest: Variant; const AVector: TVector);
begin
  VarClear(ADest);
  TVectorVarData(ADest).VType := VarVector;
  TVectorVarData(ADest).VVector := AVector;
end;

function VarVectorCreate(X: Real=0; Y: Real=0; Z: Real=0): Variant;
begin
  VarVectorCreateInto(Result,TVector.Create(X,Y,Z));
end;

function VarVectorCreate(str: string): Variant;
begin
  VarVectorCreateInto(Result,TVector.Create(str));
end;

function VarVectorCreate(vector: TVector): Variant;
begin
  VarVectorCreateInto(Result,TVector.CopyFrom(vector));
end;

function LineDistance(p1,p2: Variant): Real;
begin
  if (TVectorVarData(p1).VType=varVector) and (TVectorVarData(p2).VType=varVector) then begin
    Result:=TVector.line_distance(TVectorVarData(p1).VVector,TVectorVarData(p2).VVector);
  end
  else Raise Exception.Create('LineDistance: wrong variant type, should be vector');
end;

function VectorMul(p1,p2: Variant): Variant;
var v: TVector;
begin
  v:=TVector.Create(nil);
  v.Assign(TVectorVarData(p1).VVector);
  v.Vector_multiply(TVectorVarData(p2).VVector);
  VarVectorCreateInto(Result,v);
end;

function VectorLength(p: Variant): Real;
begin
  Result:=TVectorVarData(p).VVector.Length;
end;

initialization
  RegisterClass(TVector);
  VectorVariantType:=TVectorVariantType.Create;
finalization
  FreeAndNil(VectorVariantType);
end.
