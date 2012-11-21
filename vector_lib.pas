unit vector_lib;

interface
uses Classes,streaming_class_lib,sysUtils,simple_parser_lib,quaternion_lib;
type

TVector=class(TStreamingClass)
  private
    function vector2str: string;
    procedure str2vector(str: string);
  public
    x,y,z: Real;

//    constructor Create; overload;
//    constructor Create(owner: TComponent;_x: Real=0;_y: Real=0;_z: Real=0); overload; override;
    constructor Create(owner: TComponent=nil); override;

    procedure Assign(source: TPersistent); overload; override;
    procedure Assign(_x,_y,_z: Real); overload;

    procedure add(source: TVector);
    procedure sub(by: TVector);

    procedure normalize;
    function Length: Real;
    procedure Mul(by: Real);
    procedure Vector_multiply(by: TVector);

    procedure rotateX(angle: Real);
    procedure rotateZ(angle: Real);
    procedure rotateY(angle: Real);

    procedure rotate_by_quat(q: TQuaternion);

    class function scalar_product(M0,M1: TVector): Real;
    class function cos_between(M0,M1: TVector): Real;
    class function line_distance(M0,M1: TVector): Real;
  published
    property Value: string read vector2str write str2vector;
end;

implementation
(*
constructor TVector.Create;
begin
  inherited Create;
end;
*)
// constructors, copy constructors and streaming procedures
constructor TVector.Create(owner: TComponent);
begin
  inherited Create(owner);
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

procedure TVector.Assign(_x,_y,_z: Real);
begin
  x:=_x;
  y:=_y;
  z:=_z;
end;

function TVector.vector2str: string;
var str: string;
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
  assert(norm>0,'vector normalize: zero-length vector');
  norm:=sqrt(norm);
  x:=x/norm;
  y:=y/norm;
  z:=z/norm; 
end;

function TVector.Length: Real;
begin
  result:=sqrt(x*x+y*y+z*z);
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

class function TVector.line_distance(M0,M1: TVector): Real;
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

procedure TVector.rotate_by_quat(q: TQuaternion);
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

end.
 