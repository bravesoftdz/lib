unit stopwatch_lib;

interface

type

TStopWatch=class
  private
    ftms: Integer;
    frun: Boolean;
    st_time: TDateTime;
    function get_current_value: Integer;
  public
    procedure reset;
    procedure start;
    procedure stop;
    property tms: Integer read get_current_value;
    property running: Boolean read frun;
end;
//  Tend:=now;


//  msec:=MilliSecondsBetween(Tst,Tend);
//  lblTime.Caption:=intToStr(msec);

implementation

uses sysutils,DateUtils;

procedure TStopWatch.reset;
begin
  ftms:=0;
  st_time:=Now;
end;

procedure TStopWatch.start;
begin
  if not frun then begin
    frun:=true;
    st_time:=Now;
  end;
end;

procedure TStopWatch.stop;
begin
  if frun then begin
    ftms:=ftms+MilliSecondsBetween(st_time,Now);
    frun:=false;
  end;
end;

function TStopWatch.get_current_value: Integer;
begin
  if frun then result:=ftms+MilliSecondsBetween(st_time,Now)
  else result:=ftms;
end;


end.