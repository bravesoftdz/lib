unit Complex2D;

interface

uses classes,graphics;

type
  ComplexTernary2D=class(TPersistent)
    private
      _qx,_qy: Integer; //����� ������ �� ���� x � y
      _Tx,_Ty,_Nx,_Ny: Integer; //������ ����� ��������� � ���/���� �������� (-N, N)
      _T: Integer; //����� ��������� � �������
      base: Integer; //�������� ������� (0,0)
      function Re_value(x,y: Integer): Real;
      function Im_value(x,y: Integer): Real;
      procedure set_Re(x,y: Integer; value: Real);
      procedure set_Im(x,y: Integer; value: Real);
      procedure Inversion(fbase,mult,_T1: Integer);
      procedure general_FFT(fbase,mult,_T1: Integer; inverse: boolean);
    public
      Re_data, Im_data: array of Real; //��������� ������� ����
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

implementation

uses math,gamma_function;

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

constructor ComplexTernary2D.Create;
begin
  inherited Create;
  Set_Length(0,0);
end;

procedure ComplexTernary2D.general_FFT(fbase,mult,_T1: Integer; Inverse: boolean);
var N1,M1,T1,k,j,incr,big_incr,i: Integer;
  sqrt3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr: Real;
  //W - ������� ���������, r,i - ������. � ������ ����.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - �����
  //dif - ��������
  //p1,0,m1 - +1,0,-1 �����
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
    big_incr:=incr*3; //��� ����������� �����
    M1:=(incr-1) div (2*mult); //��� ��������
    //�������� ���������� i=0, ��� ������� ����. �� �����
    for k:=-N1 to N1 do begin
       j:=fbase+big_incr*k;
       //�������� �����. ������� �������� - ��� �� ����� ���. ����������
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
        // 4 �������� � 2 ��������� (� ����. ������)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 �������� � 4 ���������
        //������ j ��������� �� -1-� �������
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-� �������
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-� �������
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //�����, 12 �������� � 4 ���������
       end;
    //��� �������� ���������: 2pi/incr;
    //�� ������ �������� ������ 2pi, �� ��� ���� � �� ����������
    //�� ������ ����:
    Ph:=TwoPi/big_incr*mult;
    incWr:=cos(Ph);
    incWi:=-sin(Ph);
    Wr:=1;
    Wi:=0;
    for i:=1 to M1 do begin
      //������������� ������� ���������, ����� ������ ����� ��� i � -i
      tmpWr:=Wr;
      Wr:=tmpWr*incWr-Wi*incWi;
      Wi:=Wi*incWr+tmpWr*incWi;
      for k:=-N1 to N1 do begin
        //�������� ��� +i
        j:=fbase+i*mult+big_incr*k;
       //x0,y0 - ��� ���������
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //� ����� ���� �������� �� ���. ����.
        //����. +1 - �� W
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        j:=j-2*incr;
        //����. -1 �������� �� W* (������)
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr+ym1*Wi;
        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 �������� � 2 ��������� (� ����. ������)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 �������� � 4 ���������
        //������ j ��������� �� -1-� �������
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-� �������
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-� �������
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;

        //������, �� �� ����� ��� �������� -i

        j:=fbase-i*mult+big_incr*k;
       //x0,y0 - ��� ���������
        x0:=Re_data[j];
        y0:=Im_data[j];
        j:=j+incr;
        //� ����� ���� �������� �� ���. ����.
        //����. +1 - �� W* (�.� -i)
        tmpWr:=Re_data[j];
        yp1:=Im_data[j];

        xp1:=tmpWr*Wr+yp1*Wi;
        yp1:=yp1*Wr-tmpWr*Wi;

        j:=j-2*incr;
        //����. -1 �������� �� W
        tmpWr:=Re_data[j];
        ym1:=Im_data[j];

        xm1:=tmpWr*Wr-ym1*Wi;
        ym1:=ym1*Wr+tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 �������� � 2 ��������� (� ����. ������)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 �������� � 4 ���������
        //������ j ��������� �� -1-� �������
        Re_data[j]:=Ax-xdif;
        Im_data[j]:=Ay-ydif;

        j:=j+2*incr;
        //+1-� �������
        Re_data[j]:=Ax+xdif;
        Im_data[j]:=Ay+ydif;

        j:=j-incr;
        //0-� �������
        Re_data[j]:=x0+xsum;
        Im_data[j]:=y0+ysum;


       end;
    end;
    //����� ������ ����
    incr:=big_incr;
  end;

end;

procedure ComplexTernary2D.FFT;
var x,y: Integer;
begin
//������� ���������� FFT �� ������ ������
  for y:=-_Ny to _Ny do begin
    general_FFT(base+y*_Tx,1,_Tx,false);
  end;
//������ �� ������� �������
  for x:=-_Nx to _Nx do begin
    general_FFT(base+x,_Tx,_Ty,false);
  end;

end;

procedure ComplexTernary2D.InverseFFT;
var x,y: Integer;
begin

//�� ������� �������, �������������� ������� �� _T
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
  for j:=0 to _qmin1 do trits[j]:=-1; //����� ������������� �������
  for i:=-_N to _N do begin
    k:=0;
    b:=1;
    for j:=_qmin1 downto 0 do begin
      k:=k+trits[j]*b;
      b:=b*3;
    end;
    //k ��������� �������� �� i.
    if k>i then begin
    //�������� �������
      tmp:=Re_data[fbase+k*mult];
      Re_data[fbase+k*mult]:=Re_data[fbase+i*mult];
      Re_data[fbase+i*mult]:=tmp;
      tmp:=Im_data[fbase+k*mult];
      Im_data[fbase+k*mult]:=Im_data[fbase+i*mult];
      Im_data[fbase+i*mult]:=tmp;
    end;
    //�������� ��������
    j:=0;
    while j<q do begin
      inc(trits[j]);
      if trits[j]<2 then break;
      trits[j]:=-1;
      inc(j);
    end;

  end;
end;

procedure ComplexTernary2D.power_spectrum;
var x,y: Integer;
begin
  for y:=-_Ny to _Ny do
    for x:=-_Nx to _Nx do
      Set_Re(x,y,Sqr(Re_value(x,y))+Sqr(Im_value(x,y)));
end;

procedure ComplexTernary2D.Clear;
var x: Integer;
begin
  for x:=0 to _T-1 do begin
    Re_data[x]:=0;
    Im_data[x]:=0;
  end;
end;

procedure ComplexTernary2D.LoadFromBitmap(btmp: TBitmap; offset_x, offset_y: Integer);
var i,j,w,h,wh: Integer;
    x_offset,y_offset: Integer;
    i_init,j_init: Integer;
begin
  w:=btmp.Width;
  h:=btmp.Height;
  wh:=max(w,h); //�� ������ ����� (�.� �� ���� ��� :( )
  Set_Length(wh,wh);
  Clear;
  //������ ������������
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

procedure ComplexTernary2D.SaveToBitmap(btmp: TBitmap);
var i,j: Integer;
begin
  btmp.Width:=_Tx;
  btmp.Height:=_Ty;
  for i:=-_Nx to _Nx do
    for j:=-_Ny to _Ny do
      btmp.Canvas.Pixels[i+_Nx,j+_Ny]:=monochrome_from_Real(Re[i,j]);
end;

procedure ComplexTernary2D.Assign(source: TPersistent);
var _source: ComplexTernary2D absolute source;
    i: Integer;
begin
  if source is ComplexTernary2D then begin
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


end.
