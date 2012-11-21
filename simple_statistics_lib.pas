unit simple_statistics_lib;

interface

type

T1Dstats=class
  private
    _max,_min,_sum,_sum_squares :Real;
    _count: Integer;
    function get_std_dev: Real;
    function get_ave: Real;
  public
    constructor Create;

    procedure Clear;
    procedure Add(value: Real);

    property sum: Real read _sum;
    property sum_squares: Real read _sum_squares;
    property count: Integer read _count;
    property std_dev: Real read get_std_dev;
    property ave: Real read get_ave;
    property max: Real read _max;
    property min: Real read _min;
  end;

implementation

constructor T1Dstats.Create;
begin
  inherited Create;
  Clear;
end;

procedure T1Dstats.Clear;
begin
  _sum:=0;
  _sum_squares:=0;
  _count:=0;
  _max:=-1.0/0.0;
  _min:=1.0/0.0;
end;

procedure T1Dstats.Add(value: Real);
begin
  _sum:=_sum+value;
  _sum_squares:=_sum_squares+value*value;
  if value>_max then _max:=value;
  if value<_min then _min:=value;
  inc(_count);
end;

function T1Dstats.get_ave: Real;
begin
  Result:=_sum/_count;
end;


function T1Dstats.get_std_dev: Real;
begin
  result:=sqrt(_sum_squares/_count-get_ave*get_ave);
end;



end.
