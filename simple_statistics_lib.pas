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

TDiscretePick=class
  protected
    function GetCount: Integer; virtual; abstract;
    procedure SetCount(value: Integer); virtual; abstract;
    function GetProb(index: Integer): Real; virtual; abstract;
    procedure SetProb(index: Integer;value: Real); virtual; abstract;
  public
    function Pick: Integer; virtual; abstract;
    property Count: Integer read GetCount write SetCount;
    property Prob[index: Integer]: Real read GetProb write SetProb;
    procedure Normalize;
  end;

TSimplestPick=class(TDiscretePick)
  private
    fchanged: boolean;
    fCount: Integer;
    fSumOfProbs: array of Real;
    fProb: array of Real;
    procedure Prepare;
  protected
    function GetCount: Integer; override;
    procedure SetCount(value: Integer); override;
    function GetProb(index: Integer): Real; override;
    procedure SetProb(index: Integer;value: Real); override;
  public
    function Pick: Integer; override;
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

(*
        TDiscretePick
                            *)
procedure TDiscretePick.Normalize;
var i: Integer;
    s: Real;
begin
  s:=0;
  for i:=0 to count-1 do
    s:=s+prob[i];
  Assert(s>0,'TDiscretePick.Normalize: zero total probability');
  for i:=0 to count-1 do
    prob[i]:=prob[i]/s;
end;
(*
        TSimplestPick
                            *)
procedure TSimplestPick.SetCount(value: Integer);
begin
  fCount:=value;
  SetLength(fProb,value);
  fchanged:=true;
end;

function TSimplestPick.GetCount: Integer;
begin
  Result:=fCount;
end;

procedure TSimplestPick.SetProb(index: Integer; value: Real);
begin
  fProb[index]:=value;
  fchanged:=true;
end;

function TSimplestPick.GetProb(index: Integer): Real;
begin
  Result:=fprob[index];
end;

procedure TSimplestPick.Prepare;
var s: Real;
    i: Integer;
begin
  SetLength(fSumOfProbs,fCount);
  s:=0;
  for i:=0 to fCount-1 do begin
    s:=s+prob[i];
    fSumOfProbs[i]:=s;
  end;
  fchanged:=false;
end;

function TSimplestPick.Pick: Integer;
var i: Integer;
    r: Real;
begin
  if fChanged then prepare;
  r:=Random;
  i:=0;
  while r>fSumOfProbs[i] do inc(i);
  Result:=i;
end;





end.
