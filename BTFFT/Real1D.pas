unit Real1D;

interface

type

  RealTernary1D=class
    private
      data: array of Real;
      fq,fqmin1: Integer; //����� ������
      fT,fN: Integer; //������ ����� ��������� � ���/���� �������� (-N;N)
      base: Integer; //�������� �������� �������
      increments: array of Integer; //��� ��������� ���� - ��������� �������
      function value(i: Integer): Real;
      procedure set_value(i: Integer;value: Real);
    public
      procedure Set_BitCount(aQ: Integer);
      procedure Set_Length(aT: Integer);
      procedure inversion;
      procedure inversion_by_Elena;
      procedure inversion_combined;
      procedure old_inversion;
      property Re[i: integer]: Real read value write set_value; default;
      property N: Integer read fN;
      property T: Integer read fT;
      procedure FFT;
      procedure inverseFFT;
      procedure inversenormFFT;
      constructor Create;
    end;

implementation

uses math, streaming_class_lib;

(*
    RealTernary1D
                      *)
constructor RealTernary1D.Create;
begin
  inherited Create;
  Set_Length(0);
end;

procedure RealTernary1D.Set_BitCount(aQ: Integer);
var i,plus,minus: Integer;
begin
  if aQ<0 then begin
    fQ:=-1;
    fqmin1:=-2;
    fT:=0;
    fN:=-1;
    SetLength(data,0);
  end
  else begin
    fq:=aQ;
    fqmin1:=fq-1;
    fT:=Round(power(3,fq));
    fN:=(fT-1) div 2;
    SetLength(data,fT);
    base:=fN;
    SetLength(increments,fq);
    if fq>0 then begin
      increments[0]:=fT div 3;
      plus:=fT div 9;
      minus:=fT;
      for i:=1 to fq-1 do begin
        increments[i]:=increments[i-1]+plus-minus;
        plus:=plus div 3;
        minus:=minus div 3;
      end;
    end;
  end;
end;

procedure RealTernary1D.Set_Length(aT: Integer);
begin
  assert(aT>-1,'Set_Length: negative argument');
  if aT<1 then Set_BitCount(-1)
  else begin
    fq:=math.Ceil(ln(aT)/ln(3));
    Set_BitCount(fq);
  end;
end;


function RealTernary1D.value(i: Integer): Real;
begin
  assert((i<=fN) and (i>=-fN), 'RealTernary1D.value: index out of range');
  Result:=data[base+i];
end;

procedure RealTernary1D.set_value(i: Integer; value: Real);
begin
  assert((i<=fN) and (i>=-fN),'RealTernary1D.set_value: index out of range');
  data[base+i]:=value;
end;

procedure RealTernary1D.inversion;
var i,k,j,ma,ik,lim: Integer;
begin
  i:=0;
  ma:=fN-2;
  ik:=fT div 3;
  for j:=1 to ma do begin
    k:=ik;
    i:=i+k;
    lim:=fN;
    while i>lim do begin
      i:=i-3*k;
      lim:=lim-2*k;
      k:=k div 3;
      i:=i+k;
    end;
    if (j<i)  then begin
      SwapFloats(data[base+i],data[base+j]);
      SwapFloats(data[base-i],data[base-j]);
    end
    else if (i<0) then begin
      SwapFloats(data[base+i],data[base+j]);
    end;
  end;
end;

procedure RealTernary1D.inversion_by_Elena;
var k: Integer; //i, ��������� �� N+1, �� ���� ��������� ��������
    j: Integer; //��������������� ������
    a: Integer; //������� ������
    b: Integer; //��������� ������ k
    Tmin1: Integer;
begin
  j:=fN;
  Tmin1:=fT-1;
  for k:=fN+1 to fT-3 do begin //fN-2 ����������� ���� ���� ��� ����� ������� �����
    b:=0;
    a:=3;
    while k mod a=0 do begin
      inc(b);
      a:=a*3;
    end;
    j:=j+increments[b];
    //j �� ������ ������ - ��������� �������� i
    //k==i+fN
    if (k<j) then begin
      SwapFloats(data[k],data[j]);
      SwapFloats(data[Tmin1-k],data[Tmin1-j]);
    end
    else if (j<fN) then  //j ���������� �� fN
      SwapFloats(data[k],data[j]);
  end;
end;

procedure RealTernary1D.inversion_combined;
var i,j,ik,a,b,ma,mirrj,mirri: Integer;
    t: Real;
begin
  ma:=2*fN-2;
  ik:=fT div 3;
  i:=fN+ik;
  mirri:=fT-1-i;
  j:=fN+1;
  mirrj:=fT-1-j;
  while j<=ma do begin
    //�����, ��� ����� i>0
    if (j<i)  then begin
      t:=data[i];
      data[i]:=data[j];
      data[j]:=t;
      t:=data[mirri];
      data[mirri]:=data[mirrj];
      data[mirrj]:=t;
    end;
    //����� ��������� �������, ����� ������, �� ������� ��������
    inc(j);
    dec(mirrj);
    a:=9;
    b:=1;
    while j mod a=0 do begin
      inc(b);
      a:=a*3;
    end;
    inc(i,increments[b]);
    dec(mirri,increments[b]);
    //i �������� �������������, �.�. ������ fN
    t:=data[i];
    data[i]:=data[j];
    data[j]:=t;
    //�������, � -1 �� 0 ��� ��������
    //����� ������ � �������������� ������!
    inc(i,ik);
    inc(j);
    dec(mirri,ik);
    dec(mirrj);
    if (j<i)  then begin
      t:=data[i];
      data[i]:=data[j];
      data[j]:=t;
      t:=data[mirri];
      data[mirri]:=data[mirrj];
      data[mirrj]:=t;
    end
    else if i<fN then begin
      t:=data[i];
      data[i]:=data[j];
      data[j]:=t;
    end;
    //��������� � ��������� �������� - � 0 �� 1 ��� ��������
    inc(i,ik);
    inc(j);
    dec(mirri,ik);
    dec(mirrj);
  end;
end;

procedure RealTernary1D.old_inversion;
var i,j,k,b: Integer;
    trits: array of Integer;
    tmp: Real;
begin
  if fq<0 then Exit;
  SetLength(trits,fq);
  for j:=0 to fqmin1 do trits[j]:=-1; //����� ������������� �������
  for i:=-fN to fN do begin
    k:=0;
    b:=1;
    for j:=fqmin1 downto 0 do begin
      k:=k+trits[j]*b;
      b:=b*3;
    end;
    //k ��������� �������� �� i.
    b:=k+base;
    j:=i+base;
    //b - ��� �������� ������ �� k,
    //j - �������� ������ �� i.
    if b>j then begin
    //�������� �������
      tmp:=data[b];
      data[b]:=data[j];
      data[j]:=tmp;
    end;
    //�������� ��������
    j:=0;
    while j<fq do begin
      inc(trits[j]);
      if trits[j]<2 then break;
      trits[j]:=-1;
      inc(j);
    end;

  end;
end;

procedure RealTernary1D.FFT;
var N1,M1,T1,k,j,incr,big_incr,i,i0re,i0im,ipre,ipim,inre,inim: Integer;
  sqrt3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr: Real;
  //W - ������� ���������, r,i - ������. � ������ ����.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - �����
  //dif - ��������
  //p1,0,m1 - +1,0,-1 �����
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
    big_incr:=incr*3; //��� ����������� �����
    M1:=(incr-1) shr 1; //��� ��������
    //�������� ���������� i=0, ��� ������� ����. �� �����
    for k:=-N1 to N1 do begin
       j:=base+big_incr*k;
       ipre:=j+incr;
       inre:=j-incr;
       //�������� �����. ������� �������� - ��� �� ����� ���. ����������
        x0:=data[j];
        xp1:=data[ipre];
        xm1:=data[inre];

        xsum:=xp1+xm1;

        //� ��� ������ ����. �������� �� +1-��
        data[inre]:=sqrt3*(xp1-xm1);;

        //+1-� �������
        //� ��� ������ ������. �������� +1-��
        data[ipre]:=x0-0.5*xsum;

        //0-� �������
        data[j]:=x0+xsum;

        //�����, 4 �������� � 2 ���������
       end;
    //��� �������� ���������: 2pi/incr;
    //�� ������ �������� ������ 2pi, �� ��� ���� � �� ����������
    //�� ������ ����:
    Ph:=TwoPi/big_incr;
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
        j:=base+big_incr*k;

        i0re:=j+i;
        i0im:=j-i;
        ipre:=i0re+incr;
        ipim:=i0im+incr;
        inre:=i0re-incr;
        inim:=i0im-incr;
        //���, ������ ��������
        //�� risc-����������� ������ ���������� ������
        //�� ��� ��� ������ ������ �������

       //x0,y0 - ��� ���������
        x0:=data[i0re];
        y0:=data[i0im];
        //� ����� ���� �������� �� ���. ����.
        //����. +1 - �� W
        tmpWr:=data[ipre];
        yp1:=data[ipim];

        xp1:=tmpWr*Wr-yp1*Wi;
        yp1:=yp1*Wr+tmpWr*Wi;

        //����. -1 �������� �� W* (������)
        tmpWr:=data[inre];
        ym1:=data[inim];

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
        data[ipim]:=Ax-xdif;
        data[inre]:=Ay-ydif;

        //+1-� �������
        data[ipre]:=Ax+xdif;
        data[inim]:=Ay+ydif;

        //0-� �������
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

       end;
    end;
    //����� ������ ����
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
  i0re,i0im,ipre,ipim,inre,inim: Integer; //6 �������� ��� ����� "�������"
  sqrt3,sqr3,Wr,Wi,Ph,incWr,incWi,TwoPi,tmpWr,tmpWi: Real;
  //W - ������� ���������, r,i - ������. � ������ ����.
  xsum,ysum,xdif,ydif,ax,ay,xp1,xm1,yp1,ym1,x0,y0: Real;
  //sum - �����
  //dif - ��������
  //p1,0,m1 - +1,0,-1 �����
begin
  //�� ����� ������ �� N, ��� ������ ����� � ������-������ ������� ��������. ��������
  sqrt3:=sqrt(3)/2;
  TwoPi:=2*pi;

  sqr3:=sqrt(3); //��� ������� �� 2

  T1:=1;
  N1:=0;
  big_incr:=fT;

  repeat
    incr:=big_incr div 3; //��� ����������� �����
    M1:=(incr-1) div 2; //��� ��������
    //�������� ���������� i=0, ��� ������� ����. �� �����
    for k:=-N1 to N1 do begin
      j:=base+big_incr*k;
      ipre:=j+incr;
      inre:=j-incr;
      //�������� �����. ������� �������� - ��� �� ����� ���. ����������
      x0:=data[j]; //����������� �������������� ��������
      xp1:=data[ipre]; //������. ����� �� +1 � -1
      xm1:=sqr3*data[inre]; //����. ����� �� +1 � -1 (��� �����. ���������)

      xsum:=x0-xp1;

      //� ��� ������ ����. �������� �� +1-��
      data[inre]:=xsum+xm1;

      //+1-� �������
      //� ��� ������ ������. �������� +1-��
      data[ipre]:=xsum-xm1;

      //0-� �������
      data[j]:=x0+2*xp1;

      //�����, 4 �������� � 2 ���������
      end;
    //��� �������� ���������: 2pi/incr;
    //�� ������ �������� ������ 2pi, �� ��� ���� � �� ����������
    //�� ������ ����:
    Ph:=TwoPi/big_incr;
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
        j:=base+big_incr*k;

        i0re:=j+i;
        i0im:=j-i;
        ipre:=i0re+incr;
        inre:=i0im+incr;
        inim:=i0re-incr;
        ipim:=i0im-incr;
        //���, ������ ��������
        //�� risc-����������� ������ ���������� ������
        //�� ��� ��� ������ ������ �������

       //x0,y0 - ��� ���������
        x0:=data[i0re];
        y0:=data[i0im];
        //� ����� ���� �������� �� ���. ����.
        //����. +1 - �� W
        xp1:=data[ipre];
        yp1:=data[ipim];

//        xp1:=tmpWr*Wr-yp1*Wi;
//        yp1:=yp1*Wr+tmpWr*Wi;

        //����. -1 �������� �� W* (������)
//        tmpWr:=data[inre];
        xm1:=data[inre];
        ym1:=data[inim];

//        xm1:=tmpWr*Wr+ym1*Wi;
//        ym1:=ym1*Wr-tmpWr*Wi;

        xsum:=xp1+xm1;
        ysum:=yp1+ym1;
        ydif:=sqrt3*(xp1-xm1);
        xdif:=sqrt3*(ym1-yp1);
        // 4 �������� � 2 ��������� (� ����. ������)
        Ax:=x0-0.5*xsum;
        Ay:=y0-0.5*ysum;
        // 6 �������� � 4 ���������
        //������ j ��������� �� -1-� �������
        tmpWr:=Ax-xdif;
        tmpWi:=Ay-ydif;
        data[inim]:=tmpWr*Wr-tmpWi*Wi;  //Re
        data[ipim]:=tmpWi*Wr+tmpWr*Wi;  //Im

        //+1-� �������
        tmpWr:=Ax+xdif;
        tmpWi:=Ay+ydif;
        data[ipre]:=tmpWr*Wr+tmpWi*Wi;
        data[inre]:=tmpWi*Wr-tmpWr*Wi;

        //0-� �������
        data[i0re]:=x0+xsum;
        data[i0im]:=y0+ysum;

       end;

    end;
    //����� ������ ����
    T1:=T1*3;
    N1:=(T1-1) div 2;
    big_incr:=incr;
  until incr=1;
  inversion;
end;

end.
