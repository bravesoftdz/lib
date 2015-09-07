unit Real2D;

interface
uses classes;
type

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

uses math;

function RealTernary2D.get_value(x,y: Integer): Real;
begin
  Assert(x<=_Nx,'Im_value: x too big');
  Assert(x>=-_Nx,'Im_value: x too small');
  Assert(y<=_Ny,'Im_value: y too big');
  Assert(y>=-_Ny,'Im_value: y too small');
  Result:=data[base+y*_Tx+x];
end;

procedure RealTernary2D.set_value(x,y: Integer; value: Real);
begin
  Assert(x<=_Nx,'set_Im: x too big');
  Assert(x>=-_Nx,'set_Im: x too small');
  Assert(y<=_Ny,'set_Im: y too big');
  Assert(y>=-_Ny,'set_Im: y too small');
  data[base+y*_Tx+x]:=value;
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

constructor RealTernary2D.Create;
begin
  inherited Create;
  Set_Length(0,0);
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


procedure RealTernary2D.power_spectrum;
begin

end;

procedure RealTernary2D.Clear;
var x: Integer;
begin
  for x:=0 to _T-1 do data[x]:=0;
end;


procedure RealTernary2D.LoadFromBitmap(btmp: TBitmap; offset_x,offset_y: Integer);
begin

end;

procedure RealTernary2D.SaveToBitmap(btmp: TBitmap);
begin

end;

procedure RealTernary2D.Assign(source: TPersistent);
var _source: RealTernary2D absolute source;
    i: Integer;
begin
  if source is RealTernary2D then begin
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
