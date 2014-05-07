unit family_of_curves_lib;

interface

uses table_func_lib,streaming_class_lib,classes;

type

TFamilyMember=class(table_func)
private
  fparam: Real;
published
  property param: Real read fparam write fparam;
end;

TFamilyOfCurves=class(TStreamingClass)
  private
    fCount: Integer;
    fTolerance: Real;
    fZeroBounds: Boolean;
    list: TList;
    procedure readCount(reader: TReader);
    procedure writeCount(writer: TWriter);
    procedure update_list;

    function Interpolate(t1,t2: TFamilyMember;x,param: Real): Real;
    function getValue(x,param: Real): Real;

  protected
    procedure DefineProperties(filer: TFiler); override;
  public
    difficult_interp: Boolean;
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

//    procedure AddPoint(x,param,value: Real);
    procedure AddCurve(t: table_func;param: Real);

    property Value[x,param: Real]: Real read getValue; default;
    property Count: Integer read fCount;
  published
    property Tolerance: Real read fTolerance write fTolerance;
    property ZeroBounds: Boolean read fZeroBounds write fZeroBounds;
end;


implementation

uses Math,SysUtils;

function Compare(p1,p2: Pointer): Integer;
begin
  Result:=CompareValue(TFamilyMember(p1).param,TFamilyMember(p2).param);
end;

procedure TFamilyOfCurves.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('count',readCount,writeCount,count<>0);
end;

procedure TFamilyOfCurves.readCount(reader: TReader);
begin
  fCount:=reader.ReadInteger;
end;

procedure TFamilyOfCurves.writeCount(writer: TWriter);
begin
  writer.writeInteger(fCount);
end;

constructor TFamilyOfCurves.Create(owner: TComponent);
begin
  inherited Create(owner);
  list:=TList.Create;
end;

destructor TFamilyOfCurves.Destroy;
begin
  list.Free;
  inherited Destroy;
end;

procedure TFamilyOfCurves.update_list;
var i: Integer;
begin
  if list.Count<>Count then begin
    list.Clear;
    for i:=0 to ComponentCount-1 do list.Add(Components[i]);
    list.Sort(compare);
  end;
end;


function TFamilyofCurves.Interpolate(t1,t2: TFamilyMember;x,param: Real): Real;
var r,xl,xr,f0: Real;
  rx,ry,rz: Real;
  ny,k,distl,distr: Real;
  i: Integer;
begin
  r:=(param-t1.param)/(t2.param-t1.param);
  f0:=t1[x]*(1-r)+t2[x]*r;
  if difficult_interp=false then Result:=f0
  else begin
    //ищем перпендикул€ры на оба графика
    xl:=x;
    xr:=x;
    for i:=0 to 10 do begin
    rx:=x-xl;
    ry:=f0-t1[x];
    rz:=r;
    //nx=1, nz=0
    ny:=t1.derivative(xl);
    k:=(rx+ry*ny)/(1+ny*ny);
    distl:=sqrt(sqr(rx-k)+sqr(ry-k*ny)+sqr(rz));
    xl:=xl+k;

    rx:=x-xr;
    ry:=f0-t2[x];
    rz:=1-r;

    ny:=t2.derivative(xr);
    k:=(rx+ry*ny)/(1+ny*ny);
    distr:=sqrt(sqr(rx-k)+sqr(ry-k*ny)+sqr(rz));
    xr:=xr+k;

    f0:=(t1[xl]*distr+t2[xr]*distl)/(distl+distr);
    end;

    Result:=f0;
  end;
end;






function TFamilyOfCurves.getValue(x,param: Real): Real;
var i,j,k: Integer;
//    r: Real;
label Found;
begin
  i:=0;
  j:=count-1;
  //цикл может не выполнитьс€, если массив пустой (High(X)=-1)
  if j<i then begin
    Result:=NAN;
    Exit;
  end;
  update_list;

  if param>TFamilyMember(list[j]).param then begin
    if ZeroBounds then Result:=0 else Result:=TFamilyMember(list[count-1])[x];
    exit;
  end
  else if param<TFamilyMember(list[0]).param then begin
    if ZeroBounds then Result:=0 else Result:=TFamilyMember(list[0])[x];
    exit;
  end;
  dec(j); //последний элемент не нужен!
  k:=0;
  while j>=i do begin
    k:=(i+j) shr 1;
    if param<TFamilyMember(list[k]).param then j:=k-1
    else if param>TFamilyMember(list[k]).param then i:=k+1 else goto found;
  end;
  if param<TFamilyMember(list[k]).param then dec(k);
  if k<0 then begin Result:=TFamilyMember(list[0])[x]; exit; end;
  found:
  Result:=Interpolate(TFamilyMember(list[k]),TFamilyMember(list[k+1]),x,param);

//  r:=(param-TFamilyMember(list[k]).param)/(TFamilyMember(list[k+1]).param-TFamilyMember(list[k]).param);
//  Result:=TFamilyMember(list[k])[x]*(1-r)+TFamilyMember(list[k+1])[x]*r;
end;

procedure TFamilyOfCurves.AddCurve(t: table_func;param: Real);
var f: TFamilyMember;
begin
  f:=TFamilyMember.Create(self);
  f.SetSubComponent(false);
  f.assign(t);
  f.param:=param;
  List.Add(f);
  List.Sort(Compare);
  inc(fCount);
end;


initialization
RegisterClasses([TFamilyMember,TFamilyOfCurves]);

end.
