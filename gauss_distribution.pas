unit gauss_distribution;

interface

uses math;

function gauss_rnd(): Real;
function old_gauss_rnd(): Real;
procedure initialize_gauss_rnd();

implementation
var x1: Real;
    wh: boolean;
//    P: array [0..31] of Real = (0, 0.849, 0.970, 0.855, 0.994, 0.995, 0.933, 0.923, 0.727, 1, 0.691, 0.454, 0.287, 0.174, 0.101, 0.057, 0.067, 0.161, 0.236, 0.285, 0.308, 0.304, 0.280, 0.241, 0.197, 0.152, 0.112, 0.079, 0.052, 0.033, 0.020, 0.086);
    P: array [0..32] of Real;
    Q: array [0..32] of Real;
    Y: array [0..32] of Real;
    Z: array [0..32] of Real;
    S: array [0..32] of Real;
    D: array [16..30] of Real =(0.505, 0.773, 0.876, 0.939, 0.986, 0.995, 0.987, 0.979, 0.972,0.966, 0.960, 0.954, 0.948, 0.942, 0.936);
    E: array [16..30] of Real = (25, 12.5, 8.33, 6.25, 5, 4.06, 3.37, 2.86, 2.47, 2.16, 1.92, 1.71, 1.54, 1.40, 1.27);
(* старый метод полярных координат *)
(*
function gauss_rnd(): Real;
var v1,v2,s :Real;
begin
    gauss_rnd:=sqrt(-2*ln(Random))*sin(2*PI*Random);
end;
*)

//метод полярных координат
function old_gauss_rnd(): Real;
var v1,v2,s :Real;
begin
  if wh then begin
    wh:=false;
    old_gauss_rnd:=x1;
    end
  else begin
    repeat
    v1:=2*random-1;
    v2:=2*random-1;
    s:=v1*v1+v2*v2;
    until s<1;
    s:=sqrt(-2*ln(s)/s);
    x1:=v1*s;
    wh:=true;
    old_gauss_rnd:=v2*s;
  end;
end;

(* метод прямоугольника-клина-хвоста *)
procedure initialize_gauss_rnd();
var i,j: Integer;
//    s: array [1..16] of Real;
    pt: array [1..32] of Real;
    n: array [1..32] of Integer;
    bound,t: Integer;
    t1: Real;
begin
  for i:=1 to 16 do begin
    s[i]:=(i-1)/5;
  end;
  for i:=1 to 15 do begin
    pt[i]:=sqrt(2/25/pi)*exp(-i*i/50);
  end;
  pt[16]:=0.002102341;
  pt[17]:=0.005016008;
  pt[18]:=0.00736044;
  pt[19]:=0.008918817;
  pt[20]:=0.009611999;
  pt[21]:=0.009496745;
  pt[22]:=0.008735036;
  pt[23]:=0.007546401;
  pt[24]:=0.006157882;
  pt[25]:=0.004763988;
  pt[26]:=0.003503532;
  pt[27]:=0.002454011;
  pt[28]:=0.001639508;
  pt[29]:=0.001045935;
  pt[30]:=0.000637725;
  pt[31]:=0.002699796;
  pt[32]:=0;
  for i:=1 to 32 do n[i]:=i;
  //теперь получаем Y и P по алгоритму Уолкера
  //сначала отсортируем p;n по возрастанию
  //методом пузырька, торопиться некуда
  bound:=32;
  repeat
  t:=0;
  for i:=1 to Bound-1 do begin
    if pt[i]>pt[i+1] then begin
      t1:=pt[i];
      pt[i]:=pt[i+1];
      pt[i+1]:=t1;

      t:=n[i];
      n[i]:=n[i+1];
      n[i+1]:=t;

      t:=i;
    end;
  end;
  bound:=t;
  until t=1;
  for i:=32 downto 1 do begin
    P[n[1]]:=32*pt[1];
    Y[n[1]]:=n[i];

    pt[1]:=pt[i]-(1/32-pt[1]);
    n[1]:=i;
    n[1]:=n[i];
    //надо его поставить на нужное место
    j:=1;
    while pt[j]>pt[j+1] do begin
      t1:=pt[j];
      pt[j]:=pt[j+1];
      pt[j+1]:=t1;

      t:=n[j];
      n[j]:=n[j+1];
      n[j+1]:=t;
      inc(j);
      if j=32 then break;
    end;

  end;
  P[0]:=P[32];
  Y[0]:=Y[32];
  for i:=0 to 31 do begin
    Z[i]:=1/(5-5*P[i]);
    Y[i]:=Y[i]/5-Z[i];
  end;
  for i:=1 to 15 do begin
    Q[i]:=1/(5*P[i]);
  end;

end;

function gauss_rnd(): Real;
var u :Cardinal;
    f: Real;
    sign,j: Integer;
    res: Real;
    u1,v,t :Real;
label M4;
begin
  u:=random(4294967295);
  sign:=(u and $80000000) shr 31;
  j:=(u and $7C000000) shr 26;
  u:=u and $03FFFFFF;
  f:=u/$03FFFFFF;

  if f>=P[j] then res:=Y[j]+f*Z[j]
  else begin
    if j<=15 then res:=S[j]+f*Q[j]
    else begin
      if j<31 then begin
        m4:
         u1:=Random;
         v:=Random;
         if u1<v then begin
          t:=u1;
          u1:=v;
          v:=t;
         end;
         res:=S[j-15]+0.2*u1;
         if v>D[j] then begin
          if v>u1+E[j]*((exp(S[j-14]*S[j-14]-res*res)/2)-1) then goto m4;
         end;
      end
      else begin
        repeat
          res:=sqrt(9-2*ln(Random));
        until res*Random<3;
      end;
    end;
  end;

  if sign=1 then res:=-res;
  gauss_rnd:=res;
end;


end.
