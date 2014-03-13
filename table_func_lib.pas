unit table_func_lib;
(*
version 0.9
��������� � 0.9
- description ������ TStringList - ��� ������� �������� � �������� ��������� � �����
- ��������� ����������� �����, ��� desription �� ����� �����

��������� � 0.83
- �������-��, ����������� ���������!

��������� � 0.82
- ������� ����� ������� ���������� � ������� ������ ��������, ���������� �� DefineProperties
- ������ LoadFromFile � SaveToFile ������������� � LoadFromTextFile � SaveToTextFile - ����� �� ������ �� streaming ��������
- ���������� ������, ��������� ��� �������� �� �������������� ������� ����� ��������� ������

��������� � 0.81
- ��������� ������ ������ � ������ [data]
- �������� ���������� � ���� � � ������, ��������� ������������ � ���������� ��������
- �������� ���������� ��������� AddPoint - ����� �����, ���� ���� �������� ����� - ��������.
- ��������� ������ ���������� � ������� ������ ��������, ��� ������� ����������
- ������ �� ������������� ���������� step - �������!

��������� � 0.8
- ������ ��� ����������� �� ������ TStreamingClass
- ����� ������� � ������� ������ ��������
- ����� �������� zero_out_of_bounds. ���� true, ��� ������� ����������� ��������� ����. ���� false - �� �������� �� ��������� �����.

version 0.72
��������� � 0.72
- ��������� �� ����� ����������, ���������� �� ������������ ��������, �������� ��� �
������, ��� � � ������� � ���� ����������� ����� � ������� �����

��������� � 0.71
- � ����� ���� ���������� ����, ���� �������� x ��� ������� ����������� �������

��������� � 0.70
- ��������� Draw ������� ����������� � ������������ �������� �� �������, ���� ��� ������
- � ��� �� ������ ��������� �� ������������ ���-�� �������� �� ������, ������� ����������� �������� ��� ����� ������� - 1280 ����.
- ����� ������ ������ � ���������, ����� ����� ���� ������� ������� ������������, ������� � ����, ��� �������,
��������, ����������� � ��. �� ���� �������� ������������� � ������� ��������.
������ [general]
title - �������� �������
Xname - ������� ��� X
Yname - ������� ��� Y
Xunit - ����������� �� X
Yunit - ����������� �� Y
order - ������� ������������
������ [description]
� ��������� ���� - ����������� � �������
������ [data]
���� x-y: ���������� ������.

����������� - ��� ;, ��� //, ��� #
����� ���������� ������

- ��������� ���� � update_spline, ��� ������ ��� ������� ������������ 3 �� ��������� ���� changed,
��� ����� ������� ��������� � ������������ ������ normalize. 


��������� � 0.63
���������� ��������� ������:
- ������� integrate ��������� ���������� changed, ���� true, �������� update_spline
- ������� assign �� ��������� ������ changed=true, �� ������, ���� �������� ����. ������� ��� �� �� ����� ���������
- � normalize, ���� �������� - ���� ��� ������, ������������ �� ����������
- �������-�� �������� ��������� morepoints, �� �� � 16, � � 2 ���� ����. ���������� �����

��������� � 0.62

������� ������ ���������� changed (private), ��� �������� �� true, ���� ����������
�������, �� ��� �� ���� ������� updatespline. ����� updatespline � add_point �����.
��� ���������� �� splinevalue, ���� ������� ����������. ����� �������, ��� ����������
�������� ����� ����� �� ����� ����������� ��������� �� ������ �� ��� - ������ ����� �����������
����. �������. �������� minx,maxx,miny,maxy ��� ��������� ������ �������� �������. ���� ����
��������� - ���������� updatespline

��������� � 0.61
��������� ��������� Clear - ��� ������� �������, �� ��������� �������� � �������

��������� � 0.6
������ ������ ���������� �� addpoint

���� �������� ����� �������� ������ �������, ������ �������� NAN

������ ����� table_func ��������� �� TPersistent,
���������� ��������� assign, ����� ������ ���������� ������
��������� �������� �������

���������� ������� AddPoint, � ��� �� ��� ������������ �������, ���
����� � ����� �� ����������� X ��� ����������. ������ � ����� ������
������ ����� ���������� �����.

��������� ������� ������������ ���� table_func: multiply(by)
���������:
dest.multiply(source)
��� ������������ ������������� ������
dest=dest*source

��������� ������� ��������� �� ��������� (������������� ������ multiply
dest.multiply(100); - �������� �� 100

��������� ������� ������ ������������� ���������
�� ���� ������� ����������� �������
value:=func.integrate;






��������� � 0.53:
��������� ������������� ������ ������������, ����� ���������
������� �� ����� ����� �������

�������� �������� chart_series - ������ �� ������� TLineSeries,
��������� draw - ����� ������������� ���������� ������.

��������� � 0.52:
��������� ������� addpoint(X,Y) - ����� �������� � ������� ����� �����
� �������������� ���������� ��������
� �������� enabled - ��� ���������� true
������ ���� ���� ���� �� ���� �����

��������� � 0.51:
- �������� ���� � access violation: � ������ update_spline
�������������� ����������-������� ����� ����� for, �� ��������
���� �����������

- �����������, �� ��������� ������� ������������ 3
�� ��������� (������ ���������� ������� 0, ��� ����� ������
�����)

��������� � 0.5:
������������ 0-�� ������� (���������)
1-�� (����. �������)
� 3-�� (����������� ���������)
������� ����� ������ �� ���� ����
�������� ��������: ���. ���, ��� ������ ������������ ���� �������������� � ��.
�������������� ��������
��������� ������� ������������ �� ���������

*)

interface
uses SysUtils,math,TeEngine, Series, ExtCtrls, TeeProcs, Chart,classes,streaming_class_lib,streamio,simple_parser_lib,Graphics;

type
  table_func=class(TstreamingClass)
    private
    _title: string;
    _Xname: string;
    _Yname: string;
    _Xunit: string;
    _Yunit: string;
    _description: Tstrings;
    iOrder: Integer;
    fTolerance: Real;
    fCyclic: Boolean;

    _length: Integer;
    allocated_length: Integer;

    ixmin,ixmax,iymin,iymax: Real;
    b,c,d :array of Real;  {a==Y}

    _oub: Boolean;
    changed: boolean;
    fchart_series: TLineSeries;
//    fLineColor: TColor; //���� ����� �� ������� ��� ���������� Draw

    procedure plus_one; //����������� ����� ��� ��� ������ �����
    function splinevalue(xi:Real): Real;
    procedure update_spline();
    procedure update_order(new_value: Integer);
    function isen: Boolean;
    function get_xmin: Real;
    function get_xmax: Real;
    function get_ymin: Real;
    function get_ymax: Real;

    procedure WriteData(Writer: TWriter);
    procedure ReadData(Reader: TReader);

    procedure write_to_stream(var F: Textfile);
    procedure read_from_stream(var F: TextFile);

    procedure SetDescription(Strings: TStrings);

      protected

    procedure DefineProperties(Filer: TFiler); override;

      public
    X,Y: array of Real;

    property xmin: Real read get_xmin;
    property xmax: Real read get_xmax;
    property ymin: Real read get_ymin;
    property ymax: Real read get_ymax;
    property value[xi: Real]: Real read splinevalue; default;

    function LoadFromTextFile(filename: string): Boolean;
    procedure LoadConstant(new_Y:Real;new_xmin:Real;new_xmax:Real);
    procedure Clear;
    procedure ClearPoints;
    function SaveToTextFile(filename: string): Boolean;
    function AsTabbedText: string;
    procedure normalize();
    function addpoint(Xn:Real;Yn:Real): Boolean;
    function deletepoint(Xn:Real): Boolean;
    function FindPoint(val: Real;var pX,pY: Real): Boolean;
    property enabled: Boolean read isen;
    constructor Create(owner: TComponent); overload; override;
    constructor Create; reintroduce; overload;
    constructor Create(filename: string); reintroduce; overload;
    destructor Destroy; override;
    procedure draw;
    procedure Add(c: Real); overload;
    procedure Add(term: table_func); overload;
    procedure Sub(c: Real); overload;
    procedure Sub(term: table_func); overload;
    procedure Shift(amount: Real);
    procedure multiply(by: table_func); overload;
    procedure multiply(by: Real); overload;
    procedure multiply_argument(by: Real);
    procedure assign(Source:TPersistent); override;
    function integrate: Real;
    procedure morepoints;
    procedure derivative; overload;
    function derivative(xi: Real): Real; overload;
    function FindInterval(xi: Real): Integer; //����� ������ ����. ���������� ����� ������ ���
    procedure integral;
    property chart_series: TLineSeries read fchart_series write fchart_series;
      published
    property title: string read _title write _title;
    property Xname: string read _Xname write _XName;
    property Yname: string read _Yname write _Yname;
    property Xunit: string read _Xunit write _Xunit;
    property Yunit: string read _Yunit write _Yunit;
    property Description: Tstrings read _description write SetDescription;
    property order: Integer read iorder write update_order default 3;
    property zero_out_of_bounds: boolean read _oub write _oub default true; //��������� �� ��������� ���. �����
    property Cyclic: boolean read fCyclic write fCyclic default false;
    property count: Integer read _length stored false;
    property Tolerance: Real read fTolerance write fTolerance;
//    property LineColor: TColor read fLineColor write fLineColor default clBlack;
    end;
implementation
const eol: string=#13+#10;

procedure table_func.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',ReadData,WriteData,isen);
end;

procedure table_func.WriteData(Writer: TWriter);
var i: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString('('+FloatToStr(X[i])+';'+FloatToStr(Y[i])+')');
  Writer.WriteListEnd;
end;

procedure table_func.ReadData(Reader: TReader);
var p: TSimpleParser;
    i: Integer;
begin
  p:=TSimpleParser.Create;
  _length:=0;
  i:=0;
  Reader.ReadListBegin;
  while not Reader.EndOfList do begin
    plus_one;
    p.AssignString(Reader.readstring);
    p.getChar;
    X[i]:=p.getFloat;
    Y[i]:=p.getFloat;
    inc(i);
  end;
  Reader.ReadListEnd;
  p.Free;
  changed:=true;
end;

procedure table_func.plus_one;
begin
  inc(_length);
  if _length>allocated_length then begin
     allocated_length:=(allocated_length shl 1)+1;
     SetLength(X,allocated_length);
     SetLength(Y,allocated_length);
  end;
end;


constructor table_func.Create;
begin
  Create(nil);
end;

constructor table_func.Create(owner: TComponent);
begin
  inherited Create(owner);
  _description:=TStringList.Create;
  iorder:=3;
  tolerance:=1e-27;
  _oub:=true;
//  LineColor:=clBlack;
  Clear;
  chart_series:=nil;
  SetSubComponent(true);
end;

constructor table_func.Create(filename: string);
begin
  Create(owner);
  LoadFromTextFile(filename);
end;

destructor table_func.Destroy;
begin
  _description.Free;
  inherited Destroy;
end;

function table_func.isen: Boolean;
begin
  isen:=(count>0);
end;

function table_func.FindInterval(xi: Real): Integer;
var i,j,k: Integer;
label found;
begin
  if changed then update_spline;
    {������� �������� ����� ������� ������� �������}
  i:=0;
  j:=count-1;
  //���� ����� �� �����������, ���� ������ ������ (High(X)=-1)
  if j<i then begin
    Result:=-2;
    Exit;
  end;

  if xi>xmax then begin
    Result:=count;
    exit;
  end
  else if xi<xmin then begin
    Result:=-1;
    exit;
  end;

  k:=0;
  while j>=i do begin
  k:=(i+j) shr 1;
  if xi<X[k] then j:=k-1
  else if xi>X[k] then i:=k+1 else goto found;
  end;
  if xi<X[k] then dec(k);
  found:
  Result:=k;
end;


function table_func.splinevalue(xi: Real): Real;
var k: Integer;
r: Real;
begin
  k:=FindInterval(xi);
  if k=-2 then splinevalue:=NAN
  else if (k=-1) then
    if _oub then Result:=0 else Result:=Y[0]
    else if k>=count then
      if _oub then Result:=0 else Result:=Y[count-1]
      else begin
        r:=xi-X[k];
        Result:=Y[k]+r*(b[k]+r*(c[k]+r*d[k]));
      end;
end;

procedure table_func.draw;
var i,w: Integer;
    t,st,t_xmin,t_xmax: Real;
begin
  if (chart_series<>nil) and isen then begin
//    w:=chart_series.ParentChart.ClientWidth;
    w:=1280;

    if chart_series.GetHorizAxis.AutomaticMinimum then
      t_xmin:=xmin else t_xmin:=max(chart_series.GetHorizAxis.Minimum,xmin);
    if chart_series.GetHorizAxis.AutomaticMaximum then
      t_xmax:=xmax else t_xmax:=min(chart_series.GetHorizAxis.Maximum,xmax);
    st:=(t_xmax-t_xmin)/w;
    (*
    if chart_series.ParentChart.BottomAxis.Automatic then begin
      t_xmin:=xmin;
      st:=(xmax-t_xmin)/w;
    end
    else begin
      t_xmin:=chart_series.ParentChart.BottomAxis.Minimum;
      st:=(chart_series.ParentChart.BottomAxis.Maximum-t_xmin)/w;
    end;
    *)
    chart_series.Clear;
    for i:=0 to w do begin
      t:=t_xmin+st*i;
      chart_series.AddXY(t,splinevalue(t));
    end;

  end;
end;


procedure table_func.update_spline;
var i,j: Integer;
    h,alpha,l,mu,z: array of Real; {��� ���������� ��������}
begin
    changed:=false;
    j:=count-1;
    allocated_length:=count;
    SetLength(X,count); //����� ����� ��������� ������ ������
    SetLength(Y,count);
    Setlength(b,count);
    Setlength(c,count);
    Setlength(d,count);
    if not enabled then Exit;
//���� � ��� ��������
    ixmin:=X[0];
    ixmax:=ixmin;
    iymin:=Y[0];
    iymax:=iymin;
    for i:=1 to j do begin
      if X[i]<ixmin then ixmin:=X[i];
      if X[i]>ixmax then ixmax:=X[i];
      if Y[i]<iymin then iymin:=Y[i];
      if Y[i]>iymax then iymax:=Y[i];
    end;
//���������� �������
    for i:=0 to j do begin
      b[i]:=0;
      c[i]:=0;
      d[i]:=0;
    end;
    if iorder=0 then exit
    else begin
      SetLength(h,count);
      for i:=0 to j-1 do h[i]:=X[i+1]-X[i];
      if iorder=1 then begin
        for i:=0 to j-1 do b[i]:=(Y[i+1]-Y[i])/h[i];
        exit;
      end;
      if iorder=2 then begin
        if j=0 then Exit;
//        c[0]:=0; ��� � ��� �����������
          b[0]:=(Y[1]-Y[0])/h[0];
          for i:=1 to j-1 do begin
            b[i]:=b[i-1]+2*c[i-1]*h[i-1];
            c[i]:=(y[i+1]-y[i]-b[i]*h[i])/h[i]/h[i];
          end;
        Exit;
      end;
      Setlength(alpha,count);
      SetLength(l,count);
      SetLength(mu,count);
      SetLength(z,count);


      for i:=1 to j-1 do alpha[i]:=3/h[i]*(Y[i+1]-Y[i])-3/h[i-1]*(Y[i]-Y[i-1]);
      l[0]:=1;
      mu[0]:=0;
      z[0]:=0;
        for i:=1 to j-1 do begin
        l[i]:=2*(X[i+1]-X[i-1])-h[i-1]*mu[i-1];
        mu[i]:=h[i]/l[i];
        z[i]:=(alpha[i]-h[i-1]*z[i-1])/l[i];
      end;
      l[j]:=1;
      z[j]:=0;
      c[j]:=0;
      for i:=j-1 downto 0 do begin
        c[i]:=z[i]-mu[i]*c[i+1];
        b[i]:=(Y[i+1]-Y[i])/h[i]-h[i]*(c[i+1]+2*c[i])/3;
        d[i]:=(c[i+1]-c[i])/3/h[i];
      end;
    end;
end;

procedure table_func.update_order(new_value: Integer);
begin
iOrder:=new_value;
changed:=true;
end;

function table_func.addpoint(Xn:Real;Yn:Real): boolean; //��� ��� ���������� ������ undo
var i,k: Integer;
begin
  i:=count-1;   //������ ��������� �� ��������� ������� � �������
  plus_one;
  while (i>=0) and (Xn<X[i]) do dec(i);
  //i ����� ��������� �� ������������ �������, ������� Xn
  //��������, � ����� ���������
  if (i>=0) and (abs(Xn-X[i])<=Tolerance) then begin
    dec(_length);
    if abs(Yn-Y[i])<=Tolerance then begin
      result:=false; //������ �� ����������
      exit;
    end
    else begin
      Y[i]:=Yn;
      changed:=true;
      result:=true;
      Exit;
    end;
  end;
  //i ��������� �� ������������ �������, ������� ������������
  //������ ��������� � ������ �����
  inc(i);
  k:=count-1;
  while k>i do begin
  X[k]:=X[k-1];
  Y[k]:=Y[k-1];
  dec(k);
  end;
  X[i]:=Xn;
  Y[i]:=Yn;
  changed:=true;
  result:=true;
end;

function table_func.FindPoint(val: Real;var pX,pY: Real): boolean; //���������� �� ��� ����� �����?
var i,l: Integer;
begin
  if not enabled then begin
    result:=false;
    exit;
  end;
  l:=count-1;
  i:=l;
  while (abs(val-X[i])>Tolerance) do begin
    dec(i);
    if i=-1 then begin
      result:=false;
      exit;
    end;
  end;
  pX:=X[i];
  pY:=Y[i];
  Result:=true;
end;

function table_func.deletepoint(Xn: Real): boolean;
var i,l: Integer;
begin
  if not enabled then begin
    result:=false;
    exit;
  end;
  l:=count-1;
  i:=l;
  while (abs(Xn-X[i])>Tolerance) do begin
    dec(i);
    if i=-1 then begin
      result:=false;
      exit;
    end;
  end;
  //�� ���� ����� i ��������� �� �������, ������� ���� �������.
  while i<l do begin
  X[i]:=X[i+1];
  Y[i]:=Y[i+1];
  inc(i);
  end;
  dec(_length);
  changed:=true;
  result:=true;
end;

procedure table_func.read_from_stream(var F: TextFile);
var s0,s,t: string;
    i,j,k: Integer;
    section: (general,descr,data);
    separator: char;
begin
  separator:=DecimalSeparator;
  Reset(F);
  _length:=0;
  section:=data;
  i:=0;

  repeat
    ReadLn(F,s);
    //��������� ������� � ���������
    j:=1;
      while (j<=Length(s)) and ((s[j]=' ') or (s[j]=#9)) do inc(j);
    //� � ����� ������ ����
    k:=Length(s);
      while (k>=j) and ((s[k]=' ') and (s[k]=#9)) do dec(k);
    t:=copy(s,j,k+1-j);
    s0:=uppercase(t);
    if (s0='[GENERAL]') then begin section:=general; continue; end;
    if (s0='[DESCRIPTION]') then begin section:=descr; continue; end;
    if (s0='[DATA]') then begin section:=data; continue; end;
    if section=general then begin
      k:=Length(t);
      s0:=copy(s0,1,6);
      if (s0='TITLE=') then begin title:=copy(t,7,k); continue; end;
      if (s0='XNAME=') then begin Xname:=copy(t,7,k); continue; end;
      if (s0='YNAME=') then begin Yname:=copy(t,7,k); continue; end;
      if (s0='XUNIT=') then begin Xunit:=copy(t,7,k); continue; end;
      if (s0='YUNIT=') then begin Yunit:=copy(t,7,k); continue; end;
      if (s0='ORDER=') then begin order:=StrToInt(copy(t,7,k)); continue; end;
      if (s0='BOUND=') then begin zero_out_of_bounds:=(StrToInt(copy(t,7,k))=1); continue; end;
    end;
    if section=descr then begin
      description.Add(s);
    end;
    if section=data then begin
      if Length(s)=0 then begin
        if eof(F) then break else continue;
      end;
      if ((s[1]<>'/') or (s[2]<>'/')) and (s[1]<>'[') and (length(s)>2) then begin
      {skip spaces}
      j:=1;
      while (j<=Length(s)) and ((s[j]=' ') or (s[j]=#9)) do inc(j);
      {find end of number}
      k:=j;
      while (k<=Length(s)) and ((s[k]<>' ') and (s[k]<>#9)) do begin
        if (s[k]='.') or (s[k]=',') then s[k]:=separator;
        inc(k);
      end;
      {manage dynamic array in asimptotically fast way}
      plus_one;
      {assigning values}
      X[i]:=StrToFloat(copy(s,j,k-j));
      {skip spaces}
      j:=k;
      while (j<=Length(s)) and ((s[j]=' ') or (s[j]=#9)) do inc(j);
      {find end of number}
      k:=j;
      while (k<=Length(s)) and ((s[k]<>' ') and (s[k]<>#9)) do begin
          if (s[k]='.') or (s[k]=',') then s[k]:=separator;
          inc(k);
      end;
      Y[i]:=StrToFloat(copy(s,j,k-j));
      inc(i);
    end;
  end;

  until eof(F);
    if i>0 then begin
    changed:=true;
    {������ ���������� ����/��� ��������, ��������� ����. ��������}
    end;
end;


function table_func.LoadFromTextFile(filename: string): Boolean;
var F: TextFile;
begin
  try
    Result:=false;
    AssignFile(F,filename);
    read_from_stream(F);
    Result:=true;
  finally
    CloseFile(F);
  end;
end;

procedure table_func.write_to_stream(var F: Textfile);
var old_format: boolean;
    i: Integer;
begin
  Rewrite(F);
  old_format:=true;
  if (title<>'') or (Xname<>'') or (Yname<>'') or (Xunit<>'') or (Yunit<>'') or (iorder<>3) then
    begin
      Writeln(F,'[general]');
      if (title<>'') then Writeln(F,'title='+title);
      if (Xname<>'') then Writeln(F,'Xname='+Xname);
      if (Yname<>'') then Writeln(F,'Yname='+Yname);
      if (Xunit<>'') then Writeln(F,'Xunit='+Xunit);
      if (Yunit<>'') then WriteLn(F,'Yunit='+Yunit);
      if Zero_out_of_bounds then WriteLn(F,'bound=1');
      WriteLn(F,'order='+IntToStr(order));
      old_format:=false;
    end;
  if description.Count>0 then
    begin
      WriteLn(F,'[description]');
      for i:=0 to description.Count-1 do
        WriteLn(F,description[i]);
      old_format:=false;
    end;
  if (old_format=false) then WriteLn(F,'[data]');
  for i:=0 to count-1 do begin
      WriteLn(F,FloatToStr(X[i])+#9+FloatToStr(Y[i]));
  end;
end;

function table_func.SaveToTextFile(filename: string): Boolean;
var F: TextFile;
begin
  try
  Result:=false;
  assignFile(F,filename);
  write_to_stream(F);
  Result:=true;
  finally
  Closefile(F);
  end;
end;

procedure table_func.LoadConstant(new_Y:Real;new_xmin:Real;new_xmax:Real);
begin
    Setlength(X,1);
    SetLength(Y,1);
    Setlength(b,1);
    Setlength(c,1);
    Setlength(d,1);
    _length:=1;
    ixmin:=new_xmin;
    ixmax:=new_xmax;
    iymin:=new_Y;
    iymax:=iymin;
    X[0]:=ixmin;
    Y[0]:=iymin;
    b[0]:=0;
    c[0]:=0;
    d[0]:=0;
    changed:=false;
end;

procedure table_func.normalize;
var i: Integer;
begin
  if ymax>0 then begin
    for i:=0 to count-1 do
      begin
    Y[i]:=Y[i]/ymax;
      end;
    changed:=true;
  end;
end;

procedure table_func.multiply(by: table_func);
var i,ls,ld: Integer;
    Yt: array of Real;
begin
  if not by.enabled then begin
    Clear;
    Exit;
  end;
  if not enabled then Exit;
  //xmin, xmax � ������ ���������� update_spline
  //��� ���� ���� ����������� ��������� �����
  ls:=by.count-1;
  SetLength(Yt,ls+1);
  for i:=0 to ls do Yt[i]:=by.Y[i]*splinevalue(by.X[i]);

  ld:=count-1;
  for i:=0 to ld do Y[i]:=Y[i]*by[X[i]];

  for i:=0 to ls do addpoint(by.X[i],Yt[i]);
  changed:=true;

end;

procedure table_func.Add(term: table_func);
var i,ls,ld: Integer;
  Yt: array of Real;
begin
  if not (term.enabled and enabled) then Exit;
  ls:=term.count-1;
  SetLength(Yt,ls+1);
  for i:=0 to ls do Yt[i]:=term.Y[i]+splinevalue(term.X[i]);

  ld:=count-1;
  for i:=0 to ld do Y[i]:=Y[i]+term[X[i]];

  for i:=0 to ls do addpoint(term.X[i],Yt[i]);
  changed:=true;
end;

procedure table_func.Sub(term: table_func);
var i,ls,ld: Integer;
  Yt: array of Real;
begin
  if not (term.enabled and enabled) then Exit;
  ls:=term.count-1;
  SetLength(Yt,ls+1);
  for i:=0 to ls do Yt[i]:=-term.Y[i]+splinevalue(term.X[i]);

  ld:=count-1;
  for i:=0 to ld do Y[i]:=Y[i]-term[X[i]];

  for i:=0 to ls do addpoint(term.X[i],Yt[i]);
  changed:=true;
end;


procedure table_func.multiply(by: Real);
var i,ld: Integer;
begin
  if changed then update_spline;
  ld:=count-1;
  for i:=0 to ld do begin
     Y[i]:=Y[i]*by;
     b[i]:=b[i]*by;
     c[i]:=c[i]*by;
     d[i]:=d[i]*by;
  end;
  iymin:=iymin*by;
  iymax:=iymax*by;
end;


procedure table_func.assign(source: TPersistent);
var s: table_func;
begin
  if Source is table_func then
  begin
    s:=table_func(source);
    ixmin:=S.ixmin;
    ixmax:=s.ixmax;
    iymin:=s.iymin;
    iymax:=s.iymax;
    iOrder:=s.iOrder;
    _oub:=s._oub;
    _length:=s.count;
    allocated_length:=_length;

    title:=s.title;
    XName:=s.Xname;
    YName:=s.Yname;
    XUnit:=s.Xunit;
    YUnit:=s.Yunit;
    description.Assign(s.Description);
    //chart_series �� �������! �������� �������, �� �� ���������
    X:=copy(s.X,0,count);
    Y:=copy(s.Y,0,count);
    changed:=true;
  end else
    inherited Assign(Source);
end;

function table_func.integrate: Real;
var i,l: Integer;
    tmp,dif: Real;
begin
    if changed then update_spline;
    tmp:=0;
    l:=count-2;
    for i:=0 to l do begin
      dif:=X[i+1]-X[i];
      tmp:=tmp+dif*(Y[i]+dif*(b[i]/2+dif*(c[i]/3+dif*d[i]/4)));
    end;
    integrate:=tmp;
end;

procedure table_func.Clear;
begin
//������� �������, �� �� ���������!
  title:='';
  Xname:='';
  Yname:='';
  Xunit:='';
  Yunit:='';
  description.Clear;
  changed:=false;
  ClearPoints;
end;

procedure table_func.ClearPoints;
begin
  //������� ������ �����, �������� ���������
  SetLength(X,0);
  SetLength(Y,0);
  SetLength(b,0);
  SetLength(c,0);
  SetLength(d,0);
  _length:=0;
  allocated_length:=0;
  ixmin:=0;
  ixmax:=0;
  iymin:=0;
  iymax:=0;
end;

procedure table_func.morepoints;
var i,j,k: Integer;
  Yt: array of Real;
begin
//��������� � 2 ���� ���������� �����
update_spline;
j:=count-1;
if j<=0 then exit;
i:=2*j; //����� �������
SetLength(Yt,j);
for k:=0 to j-1 do begin
  Yt[k]:=splinevalue((X[k]+X[k+1])/2);
end;

_length:=i+1;
allocated_length:=_length;
SetLength(X,_length);
SetLength(Y,_length);


for k:=j downto 1 do begin
  X[i]:=X[k];
  Y[i]:=Y[k];
  dec(i);
  X[i]:=(X[k]+X[k-1])/2;
  Y[i]:=Yt[k-1];
  dec(i);
end;
changed:=true;
end;

function table_func.get_xmin :Real;
begin
  if changed then update_spline;
  get_xmin:=ixmin;
end;

function table_func.get_xmax :Real;
begin
  if changed then update_spline;
  get_xmax:=ixmax;
end;

function table_func.get_ymin :Real;
begin
  if changed then update_spline;
  get_ymin:=iymin;
end;

function table_func.get_ymax :Real;
begin
  if changed then update_spline;
  get_ymax:=iymax;
end;

procedure table_func.derivative;
var i,j :Integer;
begin
  if changed then update_spline;
  j:=count-1;
  dec(iOrder);
  if iOrder=-1 then begin
    iOrder:=0;
  end;
  for i:=0 to j do begin
    Y[i]:=b[i];
    b[i]:=2*c[i];
    c[i]:=3*d[i];
    d[i]:=0;
  end;
end;

function table_func.derivative(xi: Real): Real;
var k: Integer;
    r: Real;
begin
  k:=FindInterval(xi);
  if (k<0) or (k>=count) then Result:=0
  else begin
    r:=xi-X[k];
    Result:=b[k]+r*(2*c[k]+3*d[k]*r);
  end;
end;

procedure table_func.integral;
var i,j :Integer;
    acc,prev_acc,r: Real;
begin
  if not enabled then Exit;
    if changed then update_spline;
    j:=count-1;
    if iOrder<3 then begin
    //����������� ������, ����. �������, ����� �������� ���������
    //������ �������� "�������", ����� �� ������� if ������ �����
    d[0]:=c[0]/3;
    c[0]:=b[0]/2;
    b[0]:=Y[0];
    Y[0]:=0;
    for i:=1 to j do begin
      r:=X[i]-X[i-1];
      d[i]:=c[i]/3;
      c[i]:=b[i]/2;
      b[i]:=Y[i];
      Y[i]:=Y[i-1]+r*(b[i-1]+r*(c[i-1]+r*d[i-1]));
    end;
    inc(iOrder);
  end
  else begin
    //�� ������� �������������, ������, ������� ������� �����, � ��������� �������������
    prev_acc:=d[0]/4;
    d[0]:=c[0]/3;
    c[0]:=b[0]/2;
    b[0]:=Y[0];
    Y[0]:=0;
    for i:=1 to j do begin
      r:=X[i]-X[i-1];
      acc:=d[i]/4;
      d[i]:=c[i]/3;
      c[i]:=b[i]/2;
      b[i]:=Y[i];
      Y[i]:=Y[i-1]+r*(b[i-1]+r*(c[i-1]+r*(d[i-1]+r*prev_acc)));
      prev_acc:=acc;
    end;
    changed:=true; //������, ����� ������� ������������ ����������� ���������
  end;


end;

procedure Table_func.Add(c: Real);
var i: Integer;
begin
  for i:=0 to Count-1 do begin
    Y[i]:=Y[i]+c;
  end;
end;

procedure Table_func.Sub(c: Real);
begin
  Add(-c);
end;

procedure table_func.Shift(amount: Real);
var i: Integer;
begin
  for i:=0 to count-1 do X[i]:=X[i]+amount;
end;

procedure table_func.SetDescription(strings: TStrings);
begin
  _description.Assign(strings);
end;

procedure table_func.multiply_argument(by: Real);
var i: Integer;
begin
  for i:=0 to Length(X)-1 do begin
    X[i]:=X[i]*by;
  end;
  changed:=true;
end;

function table_func.AsTabbedText: string;
var i: Integer;
    m: TStringList;
begin
  m:=TStringList.Create;
  for i:=0 to Length(X)-1 do begin
    m.Add(FloatToStr(X[i])+#9+FloatToStr(Y[i]));
  end;
  Result:=m.Text;
  m.Free;
end;


initialization
RegisterClass(Table_func);

end.
