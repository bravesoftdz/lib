unit table_func_lib;

interface
uses SysUtils,math,TeEngine, Series, ExtCtrls, TeeProcs, Chart,classes,streaming_class_lib,streamio,simple_parser_lib,Graphics;

type

  TAbstractMathFunc=class(TStreamingClass)
    private
      ftitle,fXname,fYname: string;
      fDescription: TStrings;

      procedure SetDescription(value: TStrings);

      function get_xmin: Real; virtual; abstract;
      function get_xmax: Real; virtual; abstract;
      function get_ymin: Real; virtual; abstract;
      function get_ymax: Real; virtual; abstract;
    protected
      fXunit,fYunit: string;
      function GetValue(xi: Real): Real; virtual; abstract;
    public
      constructor Create(owner: TComponent); override;
      destructor Destroy; override;
      
      property xmin: Real read get_xmin;
      property xmax: Real read get_xmax;
      property ymin: Real read get_ymin;
      property ymax: Real read get_ymax;
      property value[xi: Real]: Real read GetValue; default;
    published
      property title: string read ftitle write ftitle;
      property Xname: string read fXname write fXName;
      property Yname: string read fYname write fYname;
      property Xunit: string read fXunit write fXunit;
      property Yunit: string read fYunit write fYunit;
      property Description: Tstrings read fdescription write SetDescription;
    end;

  FourierSeriesRec = record
    dc: Real; //постоянная составляющая
    c,s: array of Real; //0 соотв. первой гармонике
  end;

  TFourierSeriesWindowType=(wtFullPeriodsRect,wtRect);
  //wtRect - для нахождения ряда Фурье используем все доступные данные, при этом последний период может оказаться с нулями
  //wtFullPeriodsRect - обрезаем справа до целого числа периодов, только по ним ищем разложение

  table_func=class(TAbstractMathFunc)
    private
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

    procedure plus_one; //приготовить место для еще одного числа
    procedure update_spline();
    procedure update_order(new_value: Integer);
    function isen: Boolean;
    function get_xmin: Real; override;
    function get_xmax: Real; override;
    function get_ymin: Real; override;
    function get_ymax: Real; override;



    procedure WriteData(Writer: TWriter);
    procedure ReadData(Reader: TReader);

    procedure write_to_stream(var F: Textfile);
    procedure read_from_stream(var F: TextFile);

      protected
    function Getvalue(xi:Real): Real; override;
    procedure DefineProperties(Filer: TFiler); override;
    function IsNonZeroTolerance: boolean;

      public
    X,Y: array of Real;

    function LoadFromTextFile(filename: string): Boolean;
    procedure LoadFromTextTable(filename: string; x_column,y_column: Integer);
    procedure LoadConstant(new_Y:Real;new_xmin:Real;new_xmax:Real);
    procedure Clear; override;
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

    function GetInverse: table_func;  //на свой страх и риск!
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
    function CalculateArea: Real;
    procedure morepoints;
    procedure derivative; overload;
    function derivative(xi: Real): Real; overload;
    function FindInterval(xi: Real): Integer; //между какими табл. значениями лежит нужное нам
    function FindNearestPoint(ax,ay: Real;out dist: Real): Integer;  //точка, ближ. к данным коорд.
    function Average: Real;
    function RMSValue: Real;
    //dist - квадрат расстояния до этой точки
    procedure integrate;
    function IsEqual(t: table_func): Boolean; reintroduce;

    function GetB(index: Integer): Real;
    function GetC(index: Integer): Real;
    function GetD(index: Integer): Real;

    procedure FourierSeries(var storage: FourierSeriesRec;Period: Real=0; WindowType: TFourierSeriesWindowType=wtFullPeriodsRect);

    property chart_series: TLineSeries read fchart_series write fchart_series;
      published
    property order: Integer read iorder write update_order default 3;
    property zero_out_of_bounds: boolean read _oub write _oub default true; //поведение за границами обл. опред
    property Cyclic: boolean read fCyclic write fCyclic default false;
    property count: Integer read _length stored false;
    property Tolerance: Real read fTolerance write fTolerance stored IsNonZeroTolerance;
//    property LineColor: TColor read fLineColor write fLineColor default clBlack;

    end;

  procedure SwapReals(var X,Y: Real);

implementation
const eol: string=#13+#10;

procedure SwapReals(var X,Y: Real);
var tmp: Real;
begin
  tmp:=X; X:=Y; Y:=tmp;
end;

(*
      TAbstractMathFunc
                            *)
constructor TAbstractMathFunc.Create(owner: TComponent);
begin
  inherited Create(owner);
  fdescription:=TStringList.Create;
  SetSubComponent(true);
end;

destructor TAbstractMathFunc.Destroy;
begin
  fdescription.Free;
  inherited Destroy;
end;

procedure TAbstractMathFunc.SetDescription(value: TStrings);
begin
  fdescription.Assign(value);
end;

(*
      table_func
                        *)

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

function table_func.IsNonZeroTolerance: Boolean;
begin
  Result:=(fTolerance<>0);
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
  iorder:=3;
  tolerance:=1e-27;
  _oub:=true;
  Clear;
  chart_series:=nil;
end;

constructor table_func.Create(filename: string);
begin
  Create(owner);
  LoadFromTextFile(filename);
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
    {сначала двоичный поиск нужного отрезка сплайна}
  i:=0;
  j:=count-1;
  //цикл может не выполниться, если массив пустой (High(X)=-1)
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


function table_func.GetValue(xi: Real): Real;
var k: Integer;
r: Real;
begin
  k:=FindInterval(xi);
  if k=-2 then
    if _oub then Result:=0 else Result:=NAN
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
      chart_series.AddXY(t,GetValue(t));
    end;

  end;
end;


procedure table_func.update_spline;
var i,j: Integer;
    h,alpha,l,mu,z: array of Real; {для вычисления сплайнов}
begin
    changed:=false;
    j:=count-1;
    allocated_length:=count;
    SetLength(X,count); //самое время сократить расход памяти
    SetLength(Y,count);
    Setlength(b,count);
    Setlength(c,count);
    Setlength(d,count);
    if not enabled then Exit;
//макс и мин значения
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
//собственно сплайны
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
//        c[0]:=0; это и так выполняется
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

function table_func.addpoint(Xn:Real;Yn:Real): boolean; //это для правильной работы undo
var i,k: Integer;
begin
  i:=count-1;   //сейчас указывает на последний элемент в массиве
  plus_one;
  while (i>=0) and (Xn<X[i]) do dec(i);
  //i будет указывать на максимальный элемент, меньший Xn
  //проверим, а вдруг совпадает
  if (i>=0) and (abs(Xn-X[i])<=Tolerance) then begin
    dec(_length);
    if abs(Yn-Y[i])<=Tolerance then begin
      result:=false; //ничего не изменилось
      exit;
    end
    else begin
      Y[i]:=Yn;
      changed:=true;
      result:=true;
      Exit;
    end;
  end;
  //i указывает на максимальный элемент, меньший вставляемого
  //теперь вставляем в нужное место
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

function table_func.FindPoint(val: Real;var pX,pY: Real): boolean; //существует ли уже такая точка?
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
  //на этом месте i указывает на элемент, который надо удалить.
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
    //пропустим пробелы и табуляцию
    j:=1;
      while (j<=Length(s)) and ((s[j]=' ') or (s[j]=#9)) do inc(j);
    //и в конце строки тоже
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
    {Теперь определить макс/мин значения, посчитать коэф. сплайнов}
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

(*$WARNINGS OFF*)
procedure table_func.LoadFromTextTable(filename: string; x_column,y_column: Integer);
var p: TSimpleParser;
    F: TextFile;
    s: string;
    i: Integer;
    cX,cY: Real;
begin
  Clear;
  p:=TSimpleParser.Create;
  p.delimiter:=' '+#9; //возможно, придется новый парам. ввести в парсере-игнорировать пустые знач
  p.spaces:='';
  AssignFile(F,filename);
  Reset(F);
  while not eof(F) do begin
    ReadLn(F,s);
    p.AssignString(s);
    i:=1; //первый столбец именно первый, а не нулевой, будем так считать
    while i<=max(x_column,y_column) do begin
      if i=x_column then
        cX:=p.getFloat
      else if i=y_column then
        cY:=p.getFloat
      else p.getFloat;
      inc(i);
    end;

    AddPoint(cX,cY);
    //мы точно знаем, что cX,cY инициализированы, жаль компилятор этого не понимает
  end;
  p.Free;
  CloseFile(F);
end;
(*$WARNINGS ON*)

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

function table_func.Average: Real;
begin
  Result:=CalculateArea/(xmax-xmin);
end;

function table_func.RMSValue: Real;
var tmp: table_func;
begin
  tmp:=table_func.Create;
  tmp.assign(self);
  tmp.multiply(Self);
  Result:=sqrt(tmp.Average);
  tmp.Free;
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
  //xmin, xmax и прочее переделает update_spline
  //нам надо лишь перемножить отдельные точки
  ls:=by.count-1;
  SetLength(Yt,ls+1);
  for i:=0 to ls do Yt[i]:=by.Y[i]*GetValue(by.X[i]);

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
  for i:=0 to ls do Yt[i]:=term.Y[i]+GetValue(term.X[i]);

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
  for i:=0 to ls do Yt[i]:=-term.Y[i]+GetValue(term.X[i]);

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
var s: table_func absolute source;
begin
  if Source is table_func then
  begin
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
    //chart_series НЕ ТРОГАЕМ! Копируем функцию, но не интерфейс
    X:=copy(s.X,0,count);
    Y:=copy(s.Y,0,count);
    changed:=true;
  end else
    inherited Assign(Source);
end;

function table_func.CalculateArea: Real;
var i,l: Integer;
    dif: Real;
begin
    if changed then update_spline;
    Result:=0;
    l:=count-2;
    for i:=0 to l do begin
      dif:=X[i+1]-X[i];
      Result:=Result+dif*(Y[i]+dif*(b[i]/2+dif*(c[i]/3+dif*d[i]/4)));
    end;
end;

procedure table_func.Clear;
begin
//очищаем функцию, но не интерфейс!
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
  //очищаем только точки, названия оставляем
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
//увеличить в 2 раза количество точек
update_spline;
j:=count-1;
if j<=0 then exit;
i:=2*j; //конец массива
SetLength(Yt,j);
for k:=0 to j-1 do begin
  Yt[k]:=GetValue((X[k]+X[k+1])/2);
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
  changed:=true;
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

procedure table_func.integrate;
var i,j :Integer;
    acc,prev_acc,r: Real;
begin
  if not enabled then Exit;
    if changed then update_spline;
    j:=count-1;
    if iOrder<3 then begin
    //интегрируем честно, коэф. хватает, чтобы записать результат
    //первую итерацию "вручную", чтобы не вводить if внутрь цикла
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
    //не хватает коэффициентов, значит, считаем узловые точки, а остальное интерполируем
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
    changed:=true; //значит, потом пройдет интерполяция кубическими сплайнами
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

procedure table_func.multiply_argument(by: Real);
var i: Integer;
begin
  for i:=0 to count-1 do begin
    X[i]:=X[i]*by;
  end;
  if by<0 then begin
    for i:=0 to (count div 2)-1 do begin
      SwapReals(X[i],X[count-1-i]);
      SwapReals(Y[i],Y[count-1-i]);
    end;
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

function table_func.IsEqual(t: table_func): boolean;
var tol: Real;
    i: Integer;
begin
  Result:=false;
  if (t.count=count) and (order=t.order) and (cyclic=t.Cyclic) and (zero_out_of_bounds=t.zero_out_of_bounds) then begin
    tol:=tolerance+t.Tolerance;
    for i:=0 to count-1 do
      if (abs(X[i]-t.X[i])>tol) or (abs(Y[i]-t.Y[i])>tol) then Exit;
    Result:=true;
  end;
end;

function table_func.FindNearestPoint(ax,ay: Real; out dist: Real): Integer;
var i,j: Integer;
begin
  j:=FindInterval(ax);  //нашли ближ. слева
  if j<0 then begin
    Result:=-1;
    Exit;
  end;
  Result:=j;  //потом мы оставим j неизменным чтобы не потерять, откуда начали, но Result будем менять
  dist:=Sqr(ax-X[j])+Sqr(ay-Y[j]);
  //теперь наши подозреваемые - чуть влево и чуть вправо, пока заведомо не выйдем за пределы окр.
  i:=j-1;
  while (i>=0) and (Sqr(ax-X[i])<dist) do begin
    if Sqr(ax-X[i])+Sqr(ay-Y[i])<dist then begin
      Result:=i;
      dist:=Sqr(ax-X[i])+Sqr(ay-Y[i]);
    end;
    dec(i);
  end;
  //теперь пора направо
  i:=j+1;
  while (i<count) and (Sqr(ax-X[i])<dist) do begin
    if Sqr(ax-X[i])+Sqr(ay-Y[i])<dist then begin
      Result:=i;
      dist:=Sqr(ax-X[i])+Sqr(ay-Y[i]);
    end;
    inc(i);
  end;
end;

function table_func.GetB(index: Integer): Real;
begin
  if changed then update_spline;
  Result:=b[index];
end;

function table_func.GetC(index: Integer): Real;
begin
  if changed then update_spline;
  Result:=c[index];
end;

function table_func.GetD(index: Integer): Real;
begin
  if changed then update_spline;
  Result:=d[index];
end;

procedure table_func.FourierSeries(var storage: FourierSeriesRec; Period: real=0;  WindowType: TFourierSeriesWindowType=wtFullPeriodsRect);
var i,clen,slen: Integer;
    start_x,end_x,cur_x,increment,cur_val: Real;
    alpha,alpha_incr: Real;
    scale: Real;
begin
  //находит ряд Фурье до тех гармоник, под которые выделено место в хранилище
  //если ничего другого не указано, за один период считает всю область определения функции
  if Period=0 then Period:=xmax-xmin;
  case WindowType of
    wtFullPeriodsRect: begin
      start_x:=xmin;
      scale:=Floor((xmax-xmin)/Period)*Period;
      end_x:=start_x+scale;
      end;
    wtRect: begin
      start_x:=xmin;
      end_x:=xmax;
      scale:=Ceil((xmax-xmin)/Period)*Period;
//      end_x:=start_x+scale;
    end;
    else
      Raise Exception.Create('Table_func.FourierSeries: unsupported window type');
  end;
  cur_x:=start_x;
  increment:=Period/100;

  with storage do begin
    dc:=Average;
    clen:=Length(c)-1;
    slen:=Length(s)-1;
    //метод трапеций - первый и последний член половинные, остальные целые
//    dc:=value[cur_x]/2;
    for i:=0 to clen do c[i]:=(value[cur_x]-dc)/2;  //*cos(0)
    for i:=0 to slen do s[i]:=0;   //*sin(0)
    alpha:=0;
    alpha_incr:=2*pi/100;
    cur_val:=0;
    while cur_x<end_X do begin
      cur_x:=cur_x+increment;
      alpha:=alpha+alpha_incr;
      cur_val:=value[cur_x]-dc;
      for i:=0 to clen do c[i]:=c[i]+cur_val*cos(alpha*(i+1));
      for i:=0 to slen do s[i]:=s[i]+cur_val*sin(alpha*(i+1));
    end;
    //посчитаны все, а последний вместо половинного знач. полное получилось
    //плюс, надо смасштабировать
//    dc:=(dc-cur_val/2)*increment/(end_x-start_x); //просто среднее значение
    scale:=increment*2/scale;
    for i:=0 to clen do c[i]:=(c[i]-cur_val*cos(alpha*(i+1))/2)*scale;
    for i:=0 to slen do s[i]:=(s[i]-cur_val*sin(alpha*(i+1))/2)*scale;
  end;
end;

function table_func.GetInverse: table_func;
var i: Integer;
begin
  Result:=table_func.Create;
  Result.zero_out_of_bounds:=zero_out_of_bounds;
  Result.order:=order;
  for i:=0 to count-1 do
    Result.addpoint(Y[i],X[i]);
end;


initialization
RegisterClass(Table_func);

end.
