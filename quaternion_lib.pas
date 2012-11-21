unit quaternion_lib;

interface

uses classes,streaming_class_lib,sysUtils;

type
  Tquaternion=class(TstreamingClass)
    private
      _a,_x,_y,_z: Real;
    public
     Constructor Create(owner: TComponent); overload; override;
     Constructor Create(owner: TComponent;na,nx,ny,nz: Real); overload;
     procedure left_mul(by: Tquaternion);
     procedure right_mul(by: TQuaternion);
     procedure conjugate;
     procedure normalize;
     procedure Assign(Source:TPersistent);overload; override;
     procedure Assign(na,nx,ny,nz: Real); overload;
     function toStr: string;
    published
      property a: Real read _a write _a;
      property x: Real read _x write _x;
      property y: Real read _y write _y;
      property z: Real read _z write _z;
  end;
implementation

constructor Tquaternion.Create(owner: TComponent);
begin
  inherited Create(owner);
end;

constructor Tquaternion.Create(owner: TComponent; na,nx,ny,nz: Real);
begin
  inherited Create(owner);
  _a:=na;
  _x:=nx;
  _y:=ny;
  _z:=nz;
end;

procedure Tquaternion.Assign(Source: TPersistent);
var q: Tquaternion;
begin
  if Source is Tquaternion then begin
    q:=Source as Tquaternion;
    _a:=q._a;
    _x:=q._x;
    _y:=q._y;
    _z:=q._z;
  end
  else
    inherited Assign(Source);
end;

procedure TQuaternion.Assign(na,nx,ny,nz: Real);
begin
  _a:=na;
  _x:=nx;
  _y:=ny;
  _z:=nz;
end;



function Tquaternion.toStr: string;
begin
  result:=FloatToStr(_a)+#9+FloatToStr(_x)+'i'+#9+FloatToStr(_y)+'j'+#9+FloatToStr(_z)+'k';
end;

procedure Tquaternion.conjugate;
begin
  _x:=-_x;
  _y:=-_y;
  _z:=-_z;
end;

procedure Tquaternion.normalize;
var n: Real;
begin
  n:=_a*_a+_x*_x+_y*_y+_z*_z;
  assert(n>0,'normalize: quaternion has zero length');
  n:=sqrt(n);
  _a:=_a/n;
  _x:=_x/n;
  _y:=_y/n;
  _z:=_z/n;
end;

procedure Tquaternion.right_mul(by: Tquaternion); //умножение справа
var at,xt,yt,zt: Real; //врем. значения
begin
  at:=_a;
  _a:=_a*by._a-_x*by._x-_Y*by._y-_Z*by._Z;
  xt:=_x;
  _X:=at*by._X+_X*by._A+_Y*by._Z-_Z*by._Y;
  yt:=_y;
  _Y:=at*by._Y-xt*by._Z+_Y*by._A+_Z*by._X;
  _Z:=at*by._Z+xt*by._Y-yt*by._X+_Z*by._A
end;

procedure TQuaternion.left_mul(by: TQuaternion); //умножение слева
var at,xt,yt,zt: Real; //врем. значения
begin
  at:=_a;
  _a:=by._a*_a-by._x*_x-by._y*_Y-by._z*_z;
//  _a:=_a*by._a-_x*by._x-_Y*by._y-_Z*by._Z;
  xt:=_x;
  _x:=by._a*_x+by._x*at+by._y*_z-by._z*_y;
//  _X:=at*by._X+_X*by._A+_Y*by._Z-_Z*by._Y;
  yt:=_y;
  _y:=by._a*_y-by._x*_z+by._y*at+by._z*xt;
//  _Y:=at*by._Y-xt*by._Z+_Y*by._A+_Z*by._X;
  _z:=by._a*_z+by._x*yt-by._y*xt+by._z*at;
//  _Z:=at*by._Z+xt*by._Y-yt*by._X+_Z*by._A
end;

end.
