unit quaternion_lib;

interface

uses classes,streaming_class_lib,sysUtils;

type
  TAbstractQuaternion=class(TStreamingClass)
    protected
      procedure set_A(val: Real); virtual; abstract;
      procedure set_X(val: Real); virtual; abstract;
      procedure set_Y(val: Real); virtual; abstract;
      procedure set_Z(val: Real); virtual; abstract;
      function get_A: Real; virtual; abstract;
      function get_X: Real; virtual; abstract;
      function get_Y: Real; virtual; abstract;
      function get_Z: Real; virtual; abstract;
    public
      procedure Clear; override;
      procedure left_mul(by: TAbstractQuaternion);
      procedure right_mul(by: TAbstractQuaternion);
      procedure conjugate;
      procedure normalize;
      procedure Assign(Source:TPersistent);overload; override;
      procedure Assign(na,nx,ny,nz: Real); reintroduce; overload;
      function toStr: string;

      property a: Real read get_A write set_A;
      property x: Real read get_X write set_X;
      property y: Real read get_Y write set_Y;
      property z: Real read get_Z write set_Z;
  end;

  Tquaternion=class(TAbstractQuaternion)
    private
      _a,_x,_y,_z: Real;
    protected
      procedure set_A(val: Real); override;
      procedure set_X(val: Real); override;
      procedure set_Y(val: Real); override;
      procedure set_Z(val: Real); override;
      function get_A: Real; override;
      function get_X: Real; override;
      function get_Y: Real; override;
      function get_Z: Real; override;
    public
     Constructor Create(owner: TComponent); overload; override;
     Constructor Create(owner: TComponent;na,nx,ny,nz: Real);reintroduce; overload; 
    end;

  TstupidQuat=class(TAbstractQuaternion)
    private
      _a,_x,_y,_z: Single;
    protected
      procedure set_A(val: Real); override;
      procedure set_X(val: Real); override;
      procedure set_Y(val: Real); override;
      procedure set_Z(val: Real); override;
      function get_A: Real; override;
      function get_X: Real; override;
      function get_Y: Real; override;
      function get_Z: Real; override;
    public
     Constructor Create(owner: TComponent); overload; override;
     Constructor Create(owner: TComponent;na,nx,ny,nz: Real);reintroduce; overload; 
    end;

implementation
(*
    TAbstractQuaternion
                          *)

function TAbstractQuaternion.toStr: string;
begin
  result:=FloatToStr(a)+#9+FloatToStr(x)+'i'+#9+FloatToStr(y)+'j'+#9+FloatToStr(z)+'k';
end;

procedure TAbstractQuaternion.Clear;
begin
  a:=0;
  x:=0;
  y:=0;
  z:=0;
end;

procedure TAbstractQuaternion.conjugate;
begin
  x:=-x;
  y:=-y;
  z:=-z;
end;

procedure TAbstractQuaternion.normalize;
var n: Real;
begin
  n:=a*a+x*x+y*y+z*z;
  assert(n>0,'normalize: quaternion has zero length');
  n:=sqrt(n);
  a:=a/n;
  x:=x/n;
  y:=y/n;
  z:=z/n;
end;

procedure TAbstractQuaternion.right_mul(by: TAbstractQuaternion); //умножение справа
var at,xt,yt: Real; //врем. значения
begin
  at:=a;
  a:=a*by.a-x*by.x-Y*by.y-Z*by.Z;
  xt:=x;
  X:=at*by.X+X*by.A+Y*by.Z-Z*by.Y;
  yt:=y;
  Y:=at*by.Y-xt*by.Z+Y*by.A+Z*by.X;
  Z:=at*by.Z+xt*by.Y-yt*by.X+Z*by.A
end;

procedure TAbstractQuaternion.left_mul(by: TAbstractQuaternion); //умножение слева
var at,xt,yt: Real; //врем. значения
begin
  at:=a;
  a:=by.a*a-by.x*x-by.y*Y-by.z*z;
//  _a:=_a*by._a-_x*by._x-_Y*by._y-_Z*by._Z;
  xt:=x;
  x:=by.a*x+by.x*at+by.y*z-by.z*y;
//  _X:=at*by._X+_X*by._A+_Y*by._Z-_Z*by._Y;
  yt:=y;
  y:=by.a*y-by.x*z+by.y*at+by.z*xt;
//  _Y:=at*by._Y-xt*by._Z+_Y*by._A+_Z*by._X;
  z:=by.a*z+by.x*yt-by.y*xt+by.z*at;
//  _Z:=at*by._Z+xt*by._Y-yt*by._X+_Z*by._A
end;

procedure TAbstractQuaternion.Assign(Source: TPersistent);
var q: TAbstractQuaternion absolute Source;
begin
  if Source is TAbstractQuaternion then begin
    a:=q.a;
    x:=q.x;
    y:=q.y;
    z:=q.z;
  end
  else
    inherited Assign(Source);
end;

procedure TAbstractQuaternion.Assign(na,nx,ny,nz: Real);
begin
  a:=na;
  x:=nx;
  y:=ny;
  z:=nz;
end;

(*
      TQuaternion
                        *)

constructor Tquaternion.Create(owner: TComponent);
begin
  inherited Create(owner);
end;

constructor Tquaternion.Create(owner: TComponent; na,nx,ny,nz: Real);
begin
  inherited Create(owner);
  a:=na;
  x:=nx;
  y:=ny;
  z:=nz;
end;

procedure TQuaternion.set_A(val: Real);
begin
  _a:=val;
end;

procedure TQuaternion.set_X(val: Real);
begin
  _x:=val;
end;

procedure TQuaternion.set_Y(val: Real);
begin
  _y:=val;
end;

procedure TQuaternion.set_Z(val: Real);
begin
  _z:=val;
end;

function TQuaternion.get_A: Real;
begin
  Result:=_a;
end;

function TQuaternion.get_X: Real;
begin
  Result:=_x;
end;

function TQuaternion.get_Y: Real;
begin
  Result:=_y;
end;

function TQuaternion.get_Z: Real;
begin
  Result:=_z;
end;



constructor TStupidQuat.Create(owner: TComponent);
begin
  inherited Create(owner);
end;

constructor TStupidQuat.Create(owner: TComponent; na,nx,ny,nz: Real);
begin
  inherited Create(owner);
  _a:=na;
  _x:=nx;
  _y:=ny;
  _z:=nz;
end;

procedure TStupidQuat.set_A(val: Real);
begin
  _a:=val;
end;

procedure TStupidQuat.set_X(val: Real);
begin
  _x:=val;
end;

procedure TStupidQuat.set_Y(val: Real);
begin
  _y:=val;
end;

procedure TStupidQuat.set_Z(val: Real);
begin
  _z:=val;
end;

function TStupidQuat.get_A: Real;
begin
  Result:=_a;
end;

function TStupidQuat.get_X: Real;
begin
  Result:=_x;
end;

function TStupidQuat.get_Y: Real;
begin
  Result:=_y;
end;

function TStupidQuat.get_Z: Real;
begin
  Result:=_z;
end;



end.
