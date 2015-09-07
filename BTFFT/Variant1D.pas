unit Variant1D;

interface

uses Classes;

type

  VariantTernary1D = class(TPersistent)
    private
      data: array of Variant;
      fq,fqmin1: Integer; //число тритов
      fT,fN: Integer;   //полное число элементов (total) и макс/мин значение (-N;N)
      base: Integer;  //смещение нулевого отсчета
      function getValue(i: Integer): Variant;
      procedure set_value(i: Integer; value: Variant);
    public
      procedure Assign(asource: TPersistent); override;
      procedure Set_Length(aT: Integer);
      procedure inversion;
      property Value[i: Integer]: Variant read getValue write set_value; default;
      property N: Integer read fN;
      property T: Integer read fT;
      procedure GeneralFFT(isInverse: Boolean);
      procedure FFT;
      procedure Convolute(response: VariantTernary1D);
      procedure CyclicConvolute(response: VariantTernary1D);
      procedure LinearConvolute(response: VariantTernary1D);
      procedure inverseFFT;
      function AsString: string;
      constructor Create;
    end;

implementation
uses math,streaming_class_lib,VarCmplx,Variants;

(*
    VariantTernary1D
                        *)
constructor VariantTernary1D.Create;
begin
  inherited Create;
  Set_Length(0);
end;

procedure VariantTernary1D.Set_Length(aT: Integer);
begin
  assert(aT>-1,'Set_Length: negative argument');
  if aT=0 then begin
    fq:=-1;
    fqmin1:=-2;
    fT:=0;
    fN:=-1;
    SetLength(data,0);
  end
  else begin
    fq:=math.Ceil(ln(aT)/ln(3));
    fqmin1:=fq-1;
    fT:=Round(power(3,fq));
    fN:=(fT-1) div 2;
    SetLength(data,T);
    base:=fN;
  end;
end;

function VariantTernary1D.getValue(i: Integer): Variant;
begin
  assert((i<=fN) and (i>=-fN),'getValue index out of range');
  Result:=data[base+i];
end;

procedure VariantTernary1D.set_Value(i: Integer; value: Variant);
begin
  assert((i<=fN) and (i>=-fN),'set_Value index out of range');
  data[base+i]:=value;
end;

procedure VariantTernary1D.inversion;
var i,k,j,ma,ik: Integer;
begin
  i:=0;
  ma:=fN-2;
  ik:=fT div 3;
  for j:=1 to ma do begin
    k:=ik;
    i:=i+k;
    while i>fN do begin
      i:=i-3*k;
      k:=k div 3;
      i:=i+k;
    end;
    if (j<i) then begin
      SwapVariants(data[base+i],data[base+j]);
      SwapVariants(data[base-i],data[base-j]);
    end
    else if (i<0) then
      SwapVariants(data[base+i],data[base+j]);
  end;
end;

procedure VariantTernary1D.GeneralFFT(isInverse: Boolean);
var sgn,N1,M1,T1,k,j,incr,big_incr,i: Integer;
  Ph,TwoPi: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay : Real;
  xp,xm,x0: Variant;
  w,wn: Variant;  //поворот на pi/3 и -pi/3
  incW, Tw,Twm: Variant;  //twiddle factor (фаз. множитель) и его приращ
  //p1,0,m1 - +1,0,-1 соотв
begin
  if isInverse then sgn:=1 else sgn:=-1;
  w:=VarComplexCreate(-0.5,-sqrt(3)/2*sgn);
  wn:=VarComplexConjugate(w);
  TwoPi:=2*pi*sgn;

  inversion;
  T1:=fT;
  N1:=fN;
  incr:=1;

  while N1>0 do begin
    T1:=T1 div 3;
    N1:=(T1-1) div 2;
    big_incr:=incr*3; //для внутреннего цикла
    M1:=(incr-1) div 2; //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
       j:=base+big_incr*k;
       //отдельно обраб. нулевое значение - там не нужно фаз. множителей
        x0:=data[j];
        xp:=data[j+incr];
        xm:=data[j-incr];
        data[j]:=x0+xp+xm;
        data[j+incr]:=x0+xp*w+xm*wn;
        data[j-incr]:=x0+xp*wn+xm*w;
       end;
    //шаг фазового множителя: 2pi/incr;
    //на первой итерации просто 2pi, но там цикл и не запустится
    //на второй итер:
    Ph:=TwoPi/big_incr;
    incW:=VarComplexCreate(cos(Ph),-sin(Ph));
    Tw:=VarComplexCreate(1);
    for i:=1 to M1 do begin
      //пересчитываем фазовый множитель, потом делаем циклы для i и -i
      Tw:=Tw*incW;
      Twm:=VarComplexConjugate(Tw);
      for k:=-N1 to N1 do begin
        //итерация для +i
        j:=base+i+big_incr*k;
       //x0,y0 - без изменений
        x0:=data[j];
        xp:=data[j+incr]*Tw;
        xm:=data[j-incr]*Twm;
        data[j]:=x0+xp+xm;
        data[j+incr]:=x0+xp*w+xm*wn;
        data[j-incr]:=x0+xp*wn+xm*w;
        //Теперь, то же самое для элемента -i
        j:=base-i+big_incr*k;
       //x0,y0 - без изменений
        x0:=data[j];
        xp:=data[j+incr]*Twm;
        xm:=data[j-incr]*Tw;
        data[j]:=x0+xp+xm;
        data[j+incr]:=x0+xp*w+xm*wn;
        data[j-incr]:=x0+xp*wn+xm*w;

       end;
    end;
    //конец одного слоя
    incr:=big_incr;
  end;
end;

procedure VariantTernary1D.FFT;
var i: Integer;
begin
  GeneralFFT(false);
  for i:=0 to fT-1 do
    data[i]:=data[i]/fT;
end;

procedure VariantTernary1D.inverseFFT;
begin
  GeneralFFT(true);
end;

procedure VariantTernary1D.Assign(aSource: TPersistent);
var s: VariantTernary1D absolute aSource;
begin
  if aSource is VariantTernary1D then begin
    data:=Copy(s.data);
    fN:=s.fN;
    fT:=s.fT;
    fq:=s.fq;
    fqmin1:=s.fqmin1;
    base:=s.base;
  end
  else inherited;
end;

procedure VariantTernary1D.Convolute(response: VariantTernary1D);
var cresp: VariantTernary1D;
    i: Integer;
begin
  cresp:=VariantTernary1D.Create;
  cresp.Assign(response);
  cresp.GeneralFFT(false);  //без деления на N
  FFT;
  for i:=0 to fT-1 do
    data[i]:=data[i]*cresp.data[i];
  cresp.Free;
  inverseFFT;  //обратное
end;

procedure VariantTernary1D.LinearConvolute(response: VariantTernary1D);
var i,j: Integer;
    temp: VariantTernary1D;
const zero: Real=0.0;
begin
  temp:=VariantTernary1D.Create;
  temp.Set_Length(fT);
  for i:=-fN to fN do begin
    temp[i]:=zero;
    for j:=-response.fN to response.fN do
      if (i-j>=-fN) and (i-j<=fN) then
        temp[i]:=temp[i]+Value[i-j]*response[j];
  end;
  Assign(temp);
  temp.Free;
end;

procedure VariantTernary1D.CyclicConvolute(response: VariantTernary1D);
var i,j,k: Integer;
    temp: VariantTernary1D;
const zero: Real=0.0;
begin
  temp:=VariantTernary1D.Create;
  temp.Set_Length(fT);
  for i:=-fN to fN do begin
    temp[i]:=zero;
    for j:=-response.fN to response.fN do begin
      k:=i-j;
      if k>fN then k:=k-fT
      else if k<-fN then k:=k+fT;
      temp[i]:=temp[i]+value[k]*response[j];
    end;
  end;
  Assign(temp);
  temp.Free;
end;

function VariantTernary1D.AsString: string;
var i: Integer;
begin
  for i:=0 to fT-1 do
    Result:=Result+VarToStr(data[i])+';';
end;

end.
