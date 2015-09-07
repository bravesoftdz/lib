unit Complex1D;

interface

type

  ComplexTernary1D=class
    private
      Re_data,Im_data: array of Real;
      _q,_qmin1: Integer; //число тритов
      _T,_N: Integer; //полное число элементов и мин/макс значение (-N, N)
      base: Integer; //смещение нулевого отсчета
      function Re_value(i: Integer): Real;
      function Im_value(i: Integer): Real;
      procedure set_Re(i: Integer; value: Real);
      procedure set_Im(i: Integer; value: Real);
    public
      procedure Set_Length(new_T: Integer);
      procedure inversion;
      property Re[i: integer]: Real read Re_value write set_Re;
      property Im[i: integer]: Real read Im_value write set_Im;
      property N: Integer read _N;
//      procedure generalFFT(inverse: boolean);
      procedure FFT;
      procedure inverseFFT;
      constructor Create;
   end;

   function VarReal(X: Real): Variant;

implementation

uses math,streaming_class_lib;

(*
    General
                *)
function VarReal(X: Real): Variant;
begin
  Result:=X;
end;  //очень тупая функция, но без нее громоздко выходит

(*
    ComplexTernary1D
                        *)
constructor ComplexTernary1D.Create;
begin
  inherited Create;
  Set_Length(0);
end;

procedure ComplexTernary1D.Set_Length(new_T: Integer);
begin
  assert(new_T>-1,'Set_Length: negative argument');
  if new_T<1 then begin
    _q:=-1;
    _qmin1:=-2;
    _T:=0;
    _N:=-1;
    SetLength(Re_data,0);
    SetLength(Im_data,0);
  end
  else begin
    _q:=math.Ceil(ln(new_T)/ln(3));
    _qmin1:=_q-1;
    _T:=Round(power(3,_q));
    _N:=(_T-1) div 2;
    SetLength(Re_data,_T);
    SetLength(Im_data,_T);
    base:=_N;
  end;
end;

function ComplexTernary1D.Re_value(i: Integer): Real;
begin
  assert((i<=_N) and (i>=-_N),'Re_value index out of range');
  Re_value:=Re_data[base+i];
end;

function ComplexTernary1D.Im_value(i: Integer): Real;
begin
  assert((i<=_N) and (i>=-_N),'Im_value index out of range');
  Im_value:=Im_data[base+i];
end;

procedure ComplexTernary1D.set_Re(i: Integer; value: Real);
begin
  assert((i<=_N) and (i>=-_N),'set_Re index out of range');
  Re_data[base+i]:=value;
end;

procedure ComplexTernary1D.set_Im(i: Integer; value: Real);
begin
  assert((i<=_N) and (i>=-_N),'set_Im index out of range');
  Im_data[base+i]:=value;
end;

procedure ComplexTernary1D.inversion;
var i,k,j,ma,ik,ipos,jpos,lim: Integer;
    tmp: Real;
begin
  i:=0;
  ma:=_N-2;
  ik:=_T div 3;
  for j:=1 to ma do begin
    k:=ik;
    i:=i+k;
    lim:=_N;
    while i>_N do begin
      i:=i-3*k;
      lim:=lim-2*k;
      k:=k div 3;
      i:=i+k;
    end;
    if (j<i)  then begin
      SwapFloats(Re_data[base+i],Re_data[base+j]);
      SwapFloats(Im_data[base+i],Im_data[base+j]);
      SwapFloats(Re_data[base-i],Re_data[base-j]);
      SwapFloats(Im_data[base-i],Im_data[base-j]);
    end
    else if (i<0) then begin
      SwapFloats(Re_data[base+i],Re_data[base+j]);
      SwapFloats(Im_data[base+i],Im_data[base+j]);
    end;
  end;

end;

procedure ComplexTernary1D.FFT;
var N1,M1,T1,k,j,incr,big_incr,i: Integer;
  sqrt3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - суммы
  //dif - разности
  //p1,0,m1 - +1,0,-1 соотв
begin
  sqrt3:=-sqrt(3)/2;
  TwoPi:=2*pi;
  inversion;
  T1:=_T;
  N1:=_N;
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
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        xp1:=Re_data[j];
        yp1:=Im_data[j];
        j:=j-2*incr;
        xm1:=Re_data[j];
        ym1:=Im_data[j];

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //итого, 12 сложений и 4 умножения
       end;
    //шаг фазового множителя: 2pi/incr;
    //на первой итерации просто 2pi, но там цикл и не запустится
    //на второй итер:
    Ph:=TwoPi/big_incr;
    incWr:=cos(Ph);
    incWi:=-sin(Ph);
    Wr:=1;
    Wi:=0;
    for i:=1 to M1 do begin
      //пересчитываем фазовый множитель, потом делаем циклы для i и -i
      tmpWr:=Wr;
      Wr:=tmpWr*incWr-Wi*incWi;
      Wi:=Wi*incWr+tmpWr*incWi;
      for k:=-N1 to N1 do begin
        //итерация для +i
        j:=base+i+big_incr*k;
       //x0,y0 - без изменений
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        j:=j-2*incr;
        //элем. -1 умножаем на W* (сопряж)
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr+ym1*Wi;
        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //Теперь, то же самое для элемента -i

        j:=base-i+big_incr*k;
       //x0,y0 - без изменений
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W* (т.к -i)
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr+yp1*Wi;
        yp1:=yp1*Wr-tmpWr*Wi;

        j:=j-2*incr;
        //элем. -1 умножаем на W
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr-ym1*Wi;
        ym1:=ym1*Wr+tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;


       end;
    end;
    //конец одного слоя
    incr:=big_incr;
  end;

end;

procedure ComplexTernary1D.inverseFFT;
var N1,M1,T1,k,j,incr,big_incr,i: Integer;
  sqrt3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - суммы
  //dif - разности
  //p1,0,m1 - +1,0,-1 соотв
begin
  sqrt3:=sqrt(3)/2;
  TwoPi:=2*pi;
  inversion;
  T1:=_T;
  N1:=_N;
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
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        xp1:=Re_data[j];
        yp1:=Im_data[j];
        j:=j-2*incr;
        xm1:=Re_data[j];
        ym1:=Im_data[j];

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //итого, 12 сложений и 4 умножения
       end;
    //шаг фазового множителя: 2pi/incr;
    //на первой итерации просто 2pi, но там цикл и не запустится
    //на второй итер:
    Ph:=TwoPi/big_incr;
    incWr:=cos(Ph);
    incWi:=sin(Ph);
    Wr:=1;
    Wi:=0;
    for i:=1 to M1 do begin
      //пересчитываем фазовый множитель, потом делаем циклы для i и -i
      tmpWr:=Wr;
      Wr:=tmpWr*incWr-Wi*incWi;
      Wi:=Wi*incWr+tmpWr*incWi;
      for k:=-N1 to N1 do begin
        //итерация для +i
        j:=base+i+big_incr*k;
       //x0,y0 - без изменений
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        j:=j-2*incr;
        //элем. -1 умножаем на W* (сопряж)
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr+ym1*Wi;
        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //Теперь, то же самое для элемента -i

        j:=base-i+big_incr*k;
       //x0,y0 - без изменений
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W* (т.к -i)
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr+yp1*Wi;
        yp1:=yp1*Wr-tmpWr*Wi;

        j:=j-2*incr;
        //элем. -1 умножаем на W
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr-ym1*Wi;
        ym1:=ym1*Wr+tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-й элемент
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-й элемент
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;


       end;
    end;
    //конец одного слоя
    incr:=big_incr;
  end;

end;

end.
