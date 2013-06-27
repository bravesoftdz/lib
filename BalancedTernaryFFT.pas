unit BalancedTernaryFFT;

interface
uses math,graphics,gamma_function,classes;
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
      procedure old_inversion;
      procedure inversion;
      property Re[i: integer]: Real read Re_value write set_Re;
      property Im[i: integer]: Real read Im_value write set_Im;
      property N: Integer read _N;
//      procedure generalFFT(inverse: boolean);
      procedure FFT;
      procedure inverseFFT;
      constructor Create;
   end;

  RealTernary1D=class
    private
      data: array of Real;
      fq,fqmin1: Integer; //число тритов
      fT,fN: Integer; //полное число элементов и мин/макс значение (-N;N)
      base: Integer; //смещение нулевого отсчета
      function value(i: Integer): Real;
      procedure set_value(i: Integer;value: Real);
    public
      procedure Set_Length(aT: Integer);
      procedure inversion;
      procedure old_inversion;
      property Re[i: integer]: Real read value write set_value; default;
      property N: Integer read fN;
      property T: Integer read fT;
      procedure FFT;
      procedure inverseFFT;
      procedure inversenormFFT;
      constructor Create;
    end;

  ComplexTernary2D=class(TPersistent)
    private
      _qx,_qy: Integer; //число тритов по осям x и y
      _Tx,_Ty,_Nx,_Ny: Integer; //полное число элементов и мин/макс значение (-N, N)
      _T: Integer; //всего элементов в массиве
      base: Integer; //смещение отсчета (0,0)
      function Re_value(x,y: Integer): Real;
      function Im_value(x,y: Integer): Real;
      procedure set_Re(x,y: Integer; value: Real);
      procedure set_Im(x,y: Integer; value: Real);
      procedure Inversion(fbase,mult,_T1: Integer);
      procedure general_FFT(fbase,mult,_T1: Integer; inverse: boolean);
    public
      Re_data, Im_data: array of Real; //адресацию сделаем свою
      procedure Set_Length(X,Y: Integer);
      procedure FFT;
      procedure InverseFFT;
      procedure power_spectrum;
      procedure Clear;
      procedure LoadFromBitmap(btmp: TBitmap; offset_x: Integer=0; offset_y: Integer=0);
      procedure SaveToBitmap(btmp: TBitmap);

      property Nx: Integer read _Nx;
      property Ny: Integer read _Ny;
      property Tx: Integer read _Tx;
      property Ty: Integer read _Ty;
      property T: Integer read _T;
      property Re[x,y: Integer]: Real read Re_value write Set_Re;
      property Im[x,y: Integer]: Real read Im_value write Set_Im;

      procedure Assign(source:TPersistent); override;
      procedure AssignWithShift(source:ComplexTernary2D;x_offset,y_offset: Integer);

      procedure apply_Hann_window;

      constructor Create;
  end;

  RealTernary2D=class (TPersistent)
  private
    _qx,_qy: Integer;
    _Tx,_Ty,_Nx,_Ny: Integer; //полное число элементов и мин/макс значение (-N, N)
    _T: Integer; //всего элементов в массиве
    base: Integer; //смещение отсчета (0,0)
    function get_value(x,y: Integer): Real;
    procedure set_value(x,y: Integer; value: Real);
    procedure Inversion(fbase,mult,_T1: Integer);
    procedure general_FFT(fbase,mult,_T1: Integer);
    procedure general_inverseFFT(fbase,mult,_T1: Integer);
  public
    data: array of Real; //адресацию сделаем свою
    procedure Set_Length(X,Y: Integer);
    procedure FFT;
    procedure InverseFFT;
    procedure InverseNormFFT;
    procedure power_spectrum;
    procedure Clear;
    procedure LoadFromBitmap(btmp: TBitmap; offset_x: Integer=0; offset_y: Integer=0);
    procedure SaveToBitmap(btmp: TBitmap);

    property Nx: Integer read _Nx;
    property Ny: Integer read _Ny;
    property Tx: Integer read _Tx;
    property Ty: Integer read _Ty;
    property T: Integer read _T;
    property value[x,y: Integer]: Real read get_value write Set_value; default;

    procedure Assign(source:TPersistent); override;
    procedure AssignWithShift(source:RealTernary2D;x_offset,y_offset: Integer);

    procedure apply_Hann_window;

    constructor Create;
  end;



implementation

constructor ComplexTernary1D.Create;
begin
  inherited Create;
  Set_Length(0);
end;

constructor RealTernary1D.Create;
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

procedure RealTernary1D.Set_Length(aT: Integer);
begin
  assert(aT>-1,'Set_Length: negative argument');
  if aT<1 then begin
    fQ:=-1;
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
    SetLength(data,fT);
    base:=fN;
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

function RealTernary1D.value(i: Integer): Real;
begin
  assert((i<=fN) and (i>=-fN), 'RealTernary1D.value: index out of range');
  Result:=data[base+i];
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

procedure RealTernary1D.set_value(i: Integer; value: Real);
begin
  assert((i<=fN) and (i>=-fN),'RealTernary1D.set_value: index out of range');
  data[base+i]:=value;
end;

procedure ComplexTernary1D.old_inversion;
var i,j,k,b: Integer;
    trits: array of Integer;
    tmp: Real;
begin
  if _q<0 then Exit;
  SetLength(trits,_q);
  for j:=0 to _qmin1 do trits[j]:=-1; //самый отрицательный элемент
  for i:=-_N to _N do begin
    k:=0;
    b:=1;
    for j:=_qmin1 downto 0 do begin
      k:=k+trits[j]*b;
      b:=b*3;
    end;

    //k указывает инверсию от i.
    if k>i then begin
    //поменяем местами
      tmp:=Re[k];
      Re[k]:=Re[i];
      Re[i]:=tmp;
      tmp:=Im[k];
      Im[k]:=Im[i];
      Im[i]:=tmp;
    end;
    //прибавим единичку
    j:=0;
    while j<_q do begin
      inc(trits[j]);
      if trits[j]<2 then break;
      trits[j]:=-1;
      inc(j);
    end;

  end;
end;

procedure ComplexTernary1D.inversion;
var i,j,aT,aN,bT,bN,mi,ma,ipos,jpos: Integer;
    tmp: Real;
begin
  bT:=_T div 3;
  bN:=(bT-1) shr 1;
  mi:=-_N+1;
  ma:=_N-3;
  j:=-bN;
  for i:=mi to ma do begin
    aN:=bN;
    aT:=bT;
    if i<j then begin
      ipos:=base+i;
      jpos:=base+j;
      tmp:=Re_data[ipos];
      Re_data[ipos]:=Re_data[jpos];
      Re_data[jpos]:=tmp;
      tmp:=Im_data[ipos];
      Im_data[ipos]:=Im_data[jpos];
      Im_data[jpos]:=tmp;
    end;
    //прибавляем единицу в инверсной форме
    while j>aN do begin
      j:=j-(aT shl 1);
      aT:=aT div 3;
      aN:=aN-(aT shl 2);
    end;
    j:=j+aT;
  end;



end;

procedure RealTernary1D.inversion;
var i,j,aT,aN,bT,bN,mi,ma,ipos,jpos: Integer;
    tmp: Real;
begin
  bT:=fT div 3;
  bN:=(bT-1) shr 1;
  mi:=-fN+1;
  ma:=fN-3;
  j:=-bN;
  for i:=mi to ma do begin
    aN:=bN;
    aT:=bT;
    if i<j then begin
      ipos:=base+i;
      jpos:=base+j;
      tmp:=data[ipos];
      data[ipos]:=data[jpos];
      data[jpos]:=tmp;
    end;
    //прибавляем единицу в инверсной форме
    while j>aN do begin
      j:=j-(aT shl 1);
      aT:=aT div 3;
      aN:=aN-(aT shl 2);
    end;
    j:=j+aT;
  end;
end;
procedure RealTernary1D.old_inversion;
var i,j,k,b: Integer;
    trits: array of Integer;
    tmp: Real;
begin
  if fq<0 then Exit;
  SetLength(trits,fq);
  for j:=0 to fqmin1 do trits[j]:=-1; //самый отрицательный элемент
  for i:=-fN to fN do begin
    k:=0;
    b:=1;
    for j:=fqmin1 downto 0 do begin
      k:=k+trits[j]*b;
      b:=b*3;
    end;
    //k указывает инверсию от i.
    b:=k+base;
    j:=i+base;
    //b - это реальный индекс от k,
    //j - реальный индекс от i.
    if b>j then begin
    //поменяем местами
      tmp:=data[b];
      data[b]:=data[j];
      data[j]:=tmp;
    end;
    //прибавим единичку
    j:=0;
    while j<fq do begin
      inc(trits[j]);
      if trits[j]<2 then break;
      trits[j]:=-1;
      inc(j);
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

procedure RealTernary1D.FFT;
var N1,M1,T1,k,j,incr,big_incr,i,i0re,i0im,ipre,ipim,inre,inim: Integer;
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

  T1:=fT;
  N1:=fN;
  incr:=1;

  while N1>0 do begin
    T1:=T1 div 3;
    N1:=(T1-1) shr 1;
    big_incr:=incr*3; //для внутреннего цикла
    M1:=(incr-1) shr 1; //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
       j:=base+big_incr*k;
       ipre:=j+incr;
       inre:=j-incr;
       //отдельно обраб. нулевое значение - там не нужно фаз. множителей
        x0:=data[j];
        xp1:=data[ipre];
        xm1:=data[inre];

        xsum:=xp1+xm1;

        //в нем храним мним. значение от +1-го
        data[inre]:=sqrt3*(xp1-xm1);;

        //+1-й элемент
        //в нем храним действ. значение +1-го
        data[ipre]:=x0-0.5*xsum;

        //0-й элемент
        data[j]:=x0+xsum;

        //итого, 4 сложения и 2 умножения
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
        j:=base+big_incr*k;

        i0re:=j+i;
        i0im:=j-i;
        ipre:=i0re+incr;
        ipim:=i0im+incr;
        inre:=i0re-incr;
        inim:=i0im-incr;
        //нда, прорва индексов
        //на risc-архитектуре должно получаться быстро
        //но это уже совсем другая история

       //x0,y0 - без изменений
        x0:=data[i0re];
        y0:=data[i0im];
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        tmpWr:=data[ipre];
        yp1:=data[ipim];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        //элем. -1 умножаем на W* (сопряж)
        tmpWr:=data[inre];
        ym1:=data[inim];

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
        data[ipim]:=Ax-xdif;
        data[inre]:=Ay-ydif;

        //+1-й элемент
        data[ipre]:=Ax+xdif;
        data[inim]:=Ay+ydif;

        //0-й элемент
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

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

procedure RealTernary1D.inversenormFFT;
var i: Integer;
begin
  inverseFFT;
  for i:=0 to fT do data[i]:=data[i]/fT;
end;

procedure RealTernary1D.inverseFFT;
var N1,M1,T1,k,j,incr,big_incr,i: Integer;
  i0re,i0im,ipre,ipim,inre,inim: Integer; //6 индексов для одной "бабочки"
  sqrt3,sqr3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr,tmpWi: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - суммы
  //dif - разности
  //p1,0,m1 - +1,0,-1 соотв
begin
  //не будем делить на N, это правда можно к какому-нибудь другому перемнож. добавить
  sqrt3:=sqrt(3)/2;
  TwoPi:=2*pi;

  sqr3:=sqrt(3); //без деления на 2

  T1:=1;
  N1:=0;
  big_incr:=fT;

  repeat
    incr:=big_incr div 3; //для внутреннего цикла
    M1:=(incr-1) div 2; //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
      j:=base+big_incr*k;
      ipre:=j+incr;
      inre:=j-incr;
      //отдельно обраб. нулевое значение - там не нужно фаз. множителей
      x0:=data[j]; //центральное действительное значение
      xp1:=data[ipre]; //действ. часть от +1 и -1
      xm1:=sqr3*data[inre]; //мним. часть от +1 и -1 (они компл. сопряжены)

      xsum:=x0-xp1;

      //в нем храним мним. значение от +1-го
      data[inre]:=xsum+xm1;

      //+1-й элемент
      //в нем храним действ. значение +1-го
      data[ipre]:=xsum-xm1;

      //0-й элемент
      data[j]:=x0+2*xp1;

      //итого, 4 сложения и 2 умножения
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
        j:=base+big_incr*k;

        i0re:=j+i;
        i0im:=j-i;
        ipre:=i0re+incr;
        inre:=i0im+incr;
        inim:=i0re-incr;
        ipim:=i0im-incr;
        //нда, прорва индексов
        //на risc-архитектуре должно получаться быстро
        //но это уже совсем другая история

       //x0,y0 - без изменений
        x0:=data[i0re];
        y0:=data[i0im];
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        xp1:=data[ipre];
        yp1:=data[ipim];

//        xp1:=tmpWr*Wr-yp1*Wi;
//        yp1:=yp1*Wr+tmpWr*Wi;

        //элем. -1 умножаем на W* (сопряж)
//        tmpWr:=data[inre];
        xm1:=data[inre];
        ym1:=data[inim];

//        xm1:=tmpWr*Wr+ym1*Wi;
//        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        tmpWr:=Ax-xdif;
        tmpWi:=Ay-ydif;
        data[inim]:=tmpWr*Wr-tmpWi*Wi;  //Re
        data[ipim]:=tmpWi*Wr+tmpWr*Wi;  //Im

        //+1-й элемент
        tmpWr:=Ax+xdif;
        tmpWi:=Ay+ydif;
        data[ipre]:=tmpWr*Wr+tmpWi*Wi;
        data[inre]:=tmpWi*Wr-tmpWr*Wi;

        //0-й элемент
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

       end;

    end;
    //конец одного слоя
    T1:=T1*3;
    N1:=(T1-1) div 2;
    big_incr:=incr;
  until incr=1;
  inversion;
end;









function ComplexTernary2D.Re_value(x,y: Integer): Real;
begin
  Assert(x<=_Nx,'Re_value: x too big');
  Assert(x>=-_Nx,'Re_value: x too small');
  Assert(y<=_Ny,'Re_value: y too big');
  Assert(y>=-_Ny,'Re_value: y too small');
  Re_value:=Re_data[base+y*_Tx+x];
end;

function ComplexTernary2D.Im_value(x,y: Integer): Real;
begin
  Assert(x<=_Nx,'Im_value: x too big');
  Assert(x>=-_Nx,'Im_value: x too small');
  Assert(y<=_Ny,'Im_value: y too big');
  Assert(y>=-_Ny,'Im_value: y too small');
  Im_value:=Im_data[base+y*_Tx+x];
end;

function RealTernary2D.get_value(x,y: Integer): Real;
begin
  Assert(x<=_Nx,'Im_value: x too big');
  Assert(x>=-_Nx,'Im_value: x too small');
  Assert(y<=_Ny,'Im_value: y too big');
  Assert(y>=-_Ny,'Im_value: y too small');
  Result:=data[base+y*_Tx+x];
end;

procedure ComplexTernary2D.set_Re(x,y: Integer; value: Real);
begin
  Assert(x<=_Nx,'set_Re: x too big');
  Assert(x>=-_Nx,'set_Re: x too small');
  Assert(y<=_Ny,'set_Re: y too big');
  Assert(y>=-_Ny,'set_Re: y too small');
  Re_data[base+y*_Tx+x]:=value;
end;

procedure ComplexTernary2D.set_Im(x,y: Integer; value: Real);
begin
  Assert(x<=_Nx,'set_Im: x too big');
  Assert(x>=-_Nx,'set_Im: x too small');
  Assert(y<=_Ny,'set_Im: y too big');
  Assert(y>=-_Ny,'set_Im: y too small');
  Im_data[base+y*_Tx+x]:=value;
end;

procedure RealTernary2D.set_value(x,y: Integer; value: Real);
begin
  Assert(x<=_Nx,'set_Im: x too big');
  Assert(x>=-_Nx,'set_Im: x too small');
  Assert(y<=_Ny,'set_Im: y too big');
  Assert(y>=-_Ny,'set_Im: y too small');
  data[base+y*_Tx+x]:=value;
end;

procedure ComplexTernary2D.Set_Length(x,y: Integer);
begin
  assert(x>-1,'Set_Length: negative argument x');
  assert(y>-1,'Set_Length: negative argument y');
  if x<1 then begin
    _qx:=-1;
    _Tx:=0;
    _Nx:=-1;
  end
  else begin
    _qx:=math.Ceil(ln(x)/ln(3));
    _Tx:=Round(power(3,_qx));
    _Nx:=(_Tx-1) div 2;
  end;

  if y<1 then begin
    _qy:=-1;
    _Ty:=0;
    _Ny:=-1;
  end
  else begin
    _qy:=math.Ceil(ln(y)/ln(3));
    _Ty:=Round(power(3,_qy));
    _Ny:=(_Ty-1) div 2;
  end;
  _T:=_Tx*_Ty;
  SetLength(Re_data,_T);
  SetLength(Im_data,_T);
  base:=(_T-1) div 2;
end;

procedure RealTernary2D.Set_Length(x,y: Integer);
begin
  assert(x>-1,'Set_Length: negative argument x');
  assert(y>-1,'Set_Length: negative argument y');
  if x<1 then begin
    _qx:=-1;
    _Tx:=0;
    _Nx:=-1;
  end
  else begin
    _qx:=math.Ceil(ln(x)/ln(3));
    _Tx:=Round(power(3,_qx));
    _Nx:=(_Tx-1) div 2;
  end;

  if y<1 then begin
    _qy:=-1;
    _Ty:=0;
    _Ny:=-1;
  end
  else begin
    _qy:=math.Ceil(ln(y)/ln(3));
    _Ty:=Round(power(3,_qy));
    _Ny:=(_Ty-1) div 2;
  end;
  _T:=_Tx*_Ty;
  SetLength(data,_T);
  base:=(_T-1) div 2;
end;

constructor ComplexTernary2D.Create;
begin
  inherited Create;
  Set_Length(0,0);
end;

constructor RealTernary2D.Create;
begin
  inherited Create;
  Set_Length(0,0);
end;

procedure ComplexTernary2D.general_FFT(fbase,mult,_T1: Integer; Inverse: boolean);
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
  if Inverse then begin
    sqrt3:=-sqrt3;
    TwoPi:=-TwoPi;
  end;
  inversion(fbase,mult,_T1);
  T1:=_T1;
  N1:=(T1-1) shr 1;
  incr:=mult;

  while N1>0 do begin
    T1:=T1 div 3;
    N1:=(T1-1) shr 1;
    big_incr:=incr*3; //для внутреннего цикла
    M1:=(incr-1) div (2*mult); //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
       j:=fbase+big_incr*k;
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
    Ph:=TwoPi/big_incr*mult;
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
        j:=fbase+i*mult+big_incr*k;
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

        j:=fbase-i*mult+big_incr*k;
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

procedure RealTernary2D.general_FFT(fbase,mult,_T1: Integer);
var N1,M1,T1,k,j,incr,big_incr,i,i0re,i0im,ipre,ipim,inre,inim: Integer;
  sqrt3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - суммы
  //dif - разности
  //p1,0,m1 - +1,0,-1 соотв
begin
  sqrt3:=-sqrt(3)/2;
  TwoPi:=2*pi;
  inversion(fbase,mult,_T1);

  T1:=_T1;
  N1:=(T1-1) shr 1;
  incr:=mult;

  while N1>0 do begin
    T1:=T1 div 3;
    N1:=(T1-1) shr 1;
    big_incr:=incr*3; //для внутреннего цикла
    M1:=(incr-1) div (2*mult); //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
       j:=fbase+big_incr*k;
       ipre:=j+incr;
       inre:=j-incr;
       //отдельно обраб. нулевое значение - там не нужно фаз. множителей
        x0:=data[j];
        xp1:=data[ipre];
        xm1:=data[inre];

        xsum:=xp1+xm1;

        //в нем храним мним. значение от +1-го
        data[inre]:=sqrt3*(xp1-xm1);

        //+1-й элемент
        //в нем храним действ. значение +1-го
        data[ipre]:=x0-0.5*xsum;

        //0-й элемент
        data[j]:=x0+xsum;

        //итого, 4 сложения и 2 умножения
       end;
    //шаг фазового множителя: 2pi/incr;
    //на первой итерации просто 2pi, но там цикл и не запустится
    //на второй итер:
    Ph:=TwoPi/big_incr*mult;
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
        j:=fbase+big_incr*k;

        i0re:=j+i*mult;
        i0im:=j-i*mult;
        ipre:=i0re+incr;
        ipim:=i0im+incr;
        inre:=i0re-incr;
        inim:=i0im-incr;
        //нда, прорва индексов
        //на risc-архитектуре должно получаться быстро
        //но это уже совсем другая история

       //x0,y0 - без изменений
        x0:=data[i0re];
        y0:=data[i0im];
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        tmpWr:=data[ipre];
        yp1:=data[ipim];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        //элем. -1 умножаем на W* (сопряж)
        tmpWr:=data[inre];
        ym1:=data[inim];

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
        data[ipim]:=Ax-xdif;
        data[inre]:=Ay-ydif;

        //+1-й элемент
        data[ipre]:=Ax+xdif;
        data[inim]:=Ay+ydif;

        //0-й элемент
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

       end;
    end;
    //конец одного слоя
    incr:=big_incr;
  end;
end;

procedure RealTernary2D.general_inverseFFT(fbase,mult,_T1: Integer);
var N1,M1,T1,k,j,incr,big_incr,i: Integer;
  i0re,i0im,ipre,ipim,inre,inim: Integer; //6 индексов для одной "бабочки"
  sqrt3,sqr3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr,tmpWi: Real;
  //W - фазовый множитель, r,i - действ. и мнимое знач.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - суммы
  //dif - разности
  //p1,0,m1 - +1,0,-1 соотв
begin
  //не будем делить на N, это правда можно к какому-нибудь другому перемнож. добавить
  sqrt3:=sqrt(3)/2;
  TwoPi:=2*pi;

  sqr3:=sqrt(3); //без деления на 2

  T1:=1;
  N1:=0;
  big_incr:=_T1*mult;

  repeat
    incr:=big_incr div 3; //для внутреннего цикла
    M1:=(incr-1) div (2*mult); //для внешнего
    //отдельно обработаем i=0, там фазовый множ. не нужен
    for k:=-N1 to N1 do begin
      j:=fbase+big_incr*k;
      ipre:=j+incr;
      inre:=j-incr;
      //отдельно обраб. нулевое значение - там не нужно фаз. множителей
      x0:=data[j]; //центральное действительное значение
      xp1:=data[ipre]; //действ. часть от +1 и -1
      xm1:=sqr3*data[inre]; //мним. часть от +1 и -1 (они компл. сопряжены)

      xsum:=x0-xp1;

      //в нем храним мним. значение от +1-го
      data[inre]:=xsum+xm1;

      //+1-й элемент
      //в нем храним действ. значение +1-го
      data[ipre]:=xsum-xm1;

      //0-й элемент
      data[j]:=x0+2*xp1;

      //итого, 4 сложения и 2 умножения
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
        j:=fbase+big_incr*k;

        i0re:=j+i*mult;
        i0im:=j-i*mult;
        ipre:=i0re+incr;
        inre:=i0im+incr;
        inim:=i0re-incr;
        ipim:=i0im-incr;
        //нда, прорва индексов
        //на risc-архитектуре должно получаться быстро
        //но это уже совсем другая история

       //x0,y0 - без изменений
        x0:=data[i0re];
        y0:=data[i0im];
        //а здесь надо умножить на фаз. множ.
        //элем. +1 - на W
        xp1:=data[ipre];
        yp1:=data[ipim];

//        xp1:=tmpWr*Wr-yp1*Wi;
//        yp1:=yp1*Wr+tmpWr*Wi;

        //элем. -1 умножаем на W* (сопряж)
//        tmpWr:=data[inre];
        xm1:=data[inre];
        ym1:=data[inim];

//        xm1:=tmpWr*Wr+ym1*Wi;
//        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 сложения и 2 умножения (с плав. точкой)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 сложений и 4 умножения
        //сейчас j указывает на -1-й элемент
        tmpWr:=Ax-xdif;
        tmpWi:=Ay-ydif;
        data[inim]:=tmpWr*Wr-tmpWi*Wi;  //Re
        data[ipim]:=tmpWi*Wr+tmpWr*Wi;  //Im

        //+1-й элемент
        tmpWr:=Ax+xdif;
        tmpWi:=Ay+ydif;
        data[ipre]:=tmpWr*Wr+tmpWi*Wi;
        data[inre]:=tmpWi*Wr-tmpWr*Wi;

        //0-й элемент
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

       end;

    end;
    //конец одного слоя
    T1:=T1*3;
    N1:=(T1-1) div 2;
    big_incr:=incr;
  until incr=mult;
  inversion(fbase,mult,_T1);
end;



procedure ComplexTernary2D.FFT;
var x,y: Integer;
begin
//сначала одномерный FFT по каждой строке
  for y:=-_Ny to _Ny do begin
    general_FFT(base+y*_Tx,1,_Tx,false);
  end;
//теперь по каждому столбцу
  for x:=-_Nx to _Nx do begin
    general_FFT(base+x,_Tx,_Ty,false);
  end;

end;

procedure RealTernary2D.FFT;
var x,y: Integer;
begin
//сначала одномерный FFT по каждой строке
  for y:=-_Ny to _Ny do begin
    general_FFT(base+y*_Tx,1,_Tx);
  end;
//теперь по каждому столбцу
  for x:=-_Nx to _Nx do begin
    general_FFT(base+x,_Tx,_Ty);
  end;
end;

procedure ComplexTernary2D.InverseFFT;
var x,y: Integer;
begin

//по каждому столбцу, предварительно поделив на _T
  for x:=-_Nx to _Nx do begin
    for y:=-_Ny to _Ny do begin
      set_Re(x,y,Re_value(x,y)/_T);
      set_Im(x,y,Im_value(x,y)/_T);
    end;
    general_FFT(base+x,_Tx,_Ty,true);
  end;

  for y:=-_Ny to _Ny do begin
    general_FFT(base+y*_Tx,1,_Tx,true);
  end;

end;

procedure RealTernary2D.InverseFFT;
var x,y: Integer;
begin

//по каждому столбцу, предварительно поделив на _T
  for x:=-_Nx to _Nx do begin
    general_inverseFFT(base+x,_Tx,_Ty);
  end;
  
  for y:=-_Ny to _Ny do begin
    general_inverseFFT(base+y*_Tx,1,_Tx);
  end;

end;

procedure RealTernary2D.InverseNormFFT;
var x: Integer;
begin
  InverseFFT;
  for x:=0 to _T-1 do data[x]:=data[x]/_T;
end;

procedure ComplexTernary2D.inversion(fbase,mult,_T1: integer);
var i,j,k,b,q,_qmin1,_N: Integer;
    trits: array of Integer;
    tmp: Real;
begin
  if _T1<4 then Exit;
  q:=ceil(ln(_T1)/ln(3));
  SetLength(trits,q);
  _qmin1:=q-1;
  _N:=(_T1-1) div 2;
  for j:=0 to _qmin1 do trits[j]:=-1; //самый отрицательный элемент
  for i:=-_N to _N do begin
    k:=0;
    b:=1;
    for j:=_qmin1 downto 0 do begin
      k:=k+trits[j]*b;
      b:=b*3;
    end;
    //k указывает инверсию от i.
    if k>i then begin
    //поменяем местами
      tmp:=Re_data[fbase+k*mult];
      Re_data[fbase+k*mult]:=Re_data[fbase+i*mult];
      Re_data[fbase+i*mult]:=tmp;
      tmp:=Im_data[fbase+k*mult];
      Im_data[fbase+k*mult]:=Im_data[fbase+i*mult];
      Im_data[fbase+i*mult]:=tmp;
    end;
    //прибавим единичку
    j:=0;
    while j<q do begin
      inc(trits[j]);
      if trits[j]<2 then break;
      trits[j]:=-1;
      inc(j);
    end;

  end;
end;

procedure RealTernary2D.Inversion(fbase,mult,_T1: Integer);
var i,j,aT,aN,bT,bN,_N,mi,ma,ipos,jpos: Integer;
    tmp: Real;
begin
  bT:=_T1 div 3;
  bN:=(bT-1) shr 1;
  _N:=(_T1-1) shr 1;
  mi:=-_N+1;
  ma:=_N-3;
  j:=-bN;
  for i:=mi to ma do begin
    aN:=bN;
    aT:=bT;
    if i<j then begin
      ipos:=fbase+mult*i;
      jpos:=fbase+mult*j;
      tmp:=data[ipos];
      data[ipos]:=data[jpos];
      data[jpos]:=tmp;
    end;
    //прибавляем единицу в инверсной форме
    while j>aN do begin
      j:=j-(aT shl 1);
      aT:=aT div 3;
      aN:=aN-(aT shl 2);
    end;
    j:=j+aT;
  end;

end;

procedure ComplexTernary2D.power_spectrum;
var x,y: Integer;
begin
  for y:=-_Ny to _Ny do
    for x:=-_Nx to _Nx do
      Set_Re(x,y,Sqr(Re_value(x,y))+Sqr(Im_value(x,y)));
end;

procedure RealTernary2D.power_spectrum;
begin

end;

procedure ComplexTernary2D.Clear;
var x: Integer;
begin
  for x:=0 to _T-1 do begin
    Re_data[x]:=0;
    Im_data[x]:=0;
  end;
end;

procedure RealTernary2D.Clear;
var x: Integer;
begin
  for x:=0 to _T-1 do data[x]:=0;
end;

procedure ComplexTernary2D.LoadFromBitmap(btmp: TBitmap; offset_x, offset_y: Integer);
var i,j,w,h,wh: Integer;
    x_offset,y_offset: Integer;
    i_init,j_init: Integer;
begin
  w:=btmp.Width;
  h:=btmp.Height;
  wh:=max(w,h); //на первое время (т.е на пару лет :( )
  Set_Length(wh,wh);
  Clear;
  //теперь отцентрируем
  x_offset:=-_Nx+((_Tx-w) div 2)+offset_x;
  y_offset:=-_Ny+((_Ty-h) div 2)+offset_y;
  if w-1+x_offset>_Nx then w:=_Nx-x_offset+1;
  if h-1+y_offset>_Ny then h:=_Ny-y_offset+1;
  if x_offset<-_Nx then i_init:=-_Nx-x_offset else i_init:=0;
  if y_offset<-_Ny then j_init:=-_Ny-y_offset else j_init:=0;
  for j:=j_init to h-1 do begin
    for i:=i_init to w-1 do begin
      Re[i+x_offset,j+y_offset]:=Real_from_monochrome(btmp.Canvas.Pixels[i,j]);
    end;
  end;
end;

procedure RealTernary2D.LoadFromBitmap(btmp: TBitmap; offset_x,offset_y: Integer);
begin

end;

procedure ComplexTernary2D.SaveToBitmap(btmp: TBitmap);
var i,j: Integer;
begin
  btmp.Width:=_Tx;
  btmp.Height:=_Ty;
  for i:=-_Nx to _Nx do
    for j:=-_Ny to _Ny do
      btmp.Canvas.Pixels[i+_Nx,j+_Ny]:=monochrome_from_Real(Re[i,j]);
end;

procedure RealTernary2D.SaveToBitmap(btmp: TBitmap);
begin

end;

procedure ComplexTernary2D.Assign(source: TPersistent);
var _source: ComplexTernary2D;
    i: Integer;
begin
  if source is ComplexTernary2D then begin
    _source:=source as ComplexTernary2D;
    _qx:=_source._qx;
    _qy:=_source._qy;
    _Nx:=_source._Nx;
    _Ny:=_source._Ny;
    _tx:=_source._Tx;
    _ty:=_source._Ty;
    _t:=_source._T;
    base:=_source.base;
    SetLength(Re_data,_t);
    SetLength(Im_data,_t);
    for i:=0 to _T-1 do begin
      Re_data[i]:=_source.Re_data[i];
      Im_data[i]:=_source.Im_data[i];
    end;
  end
  else inherited Assign(source);
end;

procedure RealTernary2D.Assign(source: TPersistent);
var _source: RealTernary2D;
    i: Integer;
begin
  if source is RealTernary2D then begin
    _source:=source as RealTernary2D;
    _qx:=_source._qx;
    _qy:=_source._qy;
    _Nx:=_source._Nx;
    _Ny:=_source._Ny;
    _tx:=_source._Tx;
    _ty:=_source._Ty;
    _t:=_source._T;
    base:=_source.base;
    SetLength(data,_t);
    for i:=0 to _T-1 do data[i]:=_source.data[i];
  end
  else inherited Assign(source);
end;

procedure ComplexTernary2D.AssignWithShift(source: ComplexTernary2D;x_offset,y_offset: Integer);
var i,j,imin,imax,jmin,jmax: Integer;
begin
    _qx:=source._qx;
    _qy:=source._qy;
    _Nx:=source._Nx;
    _Ny:=source._Ny;
    _tx:=source._Tx;
    _ty:=source._Ty;
    _t:=source._T;
    base:=source.base;
    SetLength(Re_data,_t);
    SetLength(Im_data,_t);
    clear;
    jmin:=max(-_Ny,-Ny-y_offset);
    jmax:=min(_Ny,_Ny-y_offset);
    imin:=max(-_Nx,-_Nx-x_offset);
    imax:=min(_Nx,_Nx-x_offset);
    for j:=jmin to jmax do begin
      for i:=imin to imax do begin
         Re[i+x_offset,j+y_offset]:=source.Re[i,j];
         Im[i+x_offset,j+y_offset]:=source.Im[i,j];
      end;
    end;
end;

procedure RealTernary2D.AssignWithShift(source: RealTernary2D; x_offset,y_offset: Integer);
var i,j,imin,imax,jmin,jmax: Integer;
begin
    _qx:=source._qx;
    _qy:=source._qy;
    _Nx:=source._Nx;
    _Ny:=source._Ny;
    _tx:=source._Tx;
    _ty:=source._Ty;
    _t:=source._T;
    base:=source.base;
    SetLength(data,_t);
    clear;
    jmin:=max(-_Ny,-Ny-y_offset);
    jmax:=min(_Ny,_Ny-y_offset);
    imin:=max(-_Nx,-_Nx-x_offset);
    imax:=min(_Nx,_Nx-x_offset);
    for j:=jmin to jmax do begin
      for i:=imin to imax do begin
         value[i+x_offset,j+y_offset]:=source[i,j];
      end;
    end;
end;

procedure ComplexTernary2D.apply_Hann_window;
var i,j: Integer;
    wfx,wfy: Real;
begin
  for i:=-_Nx to _Nx do begin
    wfx:=0.5*(1+cos(i*pi/_Nx));
    for j:=-_Ny to _Ny do begin
      wfy:=wfx*0.5*(1+cos(j*pi/_Ny));
      Re[i,j]:=wfy*Re[i,j];
      Im[i,j]:=wfy*Im[i,j];
    end;
  end;
end;

procedure RealTernary2D.apply_Hann_window;
var i,j: Integer;
    wfx,wfy: Real;
begin
  for i:=-_Nx to _Nx do begin
    wfx:=0.5*(1+cos(i*pi/_Nx));
    for j:=-_Ny to _Ny do begin
      wfy:=wfx*0.5*(1+cos(j*pi/_Ny));
      value[i,j]:=wfy*value[i,j];
    end;
  end;
end;


end.
