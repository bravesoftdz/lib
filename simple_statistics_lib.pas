unit simple_statistics_lib;

interface
uses classes;

type

T1Dstats=class(TPersistent)
  private
    _max,_min,_sum,_sum_squares :Extended;
    _count: Integer;
    function get_std_dev: Extended;
    function get_ave: Extended;
    function getRMS: Extended;
  public
    constructor Create;

    procedure Clear;
    procedure Add(value: Extended);

    procedure Append(values: T1Dstats);
  published
    property sum: Extended read _sum;
    property sum_squares: Extended read _sum_squares;
    property rms: Extended read getRMS;
    property count: Integer read _count;
    property std_dev: Extended read get_std_dev;
    property ave: Extended read get_ave;
    property max: Extended read _max;
    property min: Extended read _min;
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

TPickingProc=function:Integer of object;

TSmallCountPick=class(TSimplestPick)
  private
    fNonZeroCount: Integer;
    fPickingProc: TPickingProc;
    fIndexes: array of Integer;
    procedure NewPrepare;
    function NoChoice: Integer;
    function OneOfTwo: Integer;
    function OneofMany: Integer;
  protected
    procedure SetCount(value: Integer); override;
  public
    function Pick: Integer; override;
  end;

//простейшие классы для конкретного числа объектов, из которых выбирать
TNoChoicePick=class(TDiscretePick)
  protected
    function GetCount: Integer; override;
    procedure SetCount(value: Integer); override;
    function GetProb(index: Integer): Real; override;
    procedure SetProb(index: Integer;value: Real); override;
  public
    function Pick: Integer; override;
  end;

TOneOfTwoChoicePick=class(TDiscretePick)
  private
    fprob: array [0..1] of Real;
  protected
    function GetCount: Integer; override;
    procedure SetCount(value: Integer); override;
    function GetProb(index: Integer): Real; override;
    procedure SetProb(index: Integer;value: Real); override;
  public
    function Pick: Integer; override;
  end;

TPermanentOneOfTwoChoicePick=class(TDiscretePick)
  private
    fprob: array [0..1] of Real;
    fthreshold: Real;
  protected
    function GetCount: Integer; override;
    procedure SetCount(value: Integer); override;
    function GetProb(index: Integer): Real; override;
    procedure SetProb(index: Integer; value: Real); override;
  public
    function Pick: Integer; override;
  end;

function CreateAppropriatePick(count: Integer;PickToChangeRatio: Real=1000): TDiscretePick;




implementation

uses SysUtils;

(*
      T1Dstats
                      *)

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

procedure T1Dstats.Add(value: Extended);
begin
  _sum:=_sum+value;
  _sum_squares:=_sum_squares+value*value;
  if value>_max then _max:=value;
  if value<_min then _min:=value;
  inc(_count);
end;

procedure T1Dstats.Append(values: T1Dstats);
begin
  _count:=_count+values._count;
  _sum:=_sum+values._sum;
  _sum_squares:=_sum_squares+values._sum_squares;
  if values._min<_min then _min:=values._min;
  if values._max>_max then _max:=values._max;
end;

function T1Dstats.get_ave: Extended;
begin
  if _count=0 then Raise Exception.Create('T1Dstas.ave: empty list');
  Result:=_sum/_count;
end;

function T1Dstats.get_std_dev: Extended;
begin
  result:=sqrt(_sum_squares/_count-get_ave*get_ave);
end;

function T1Dstats.getRMS: Extended;
begin
  result:=sqrt(_sum_squares/_count);
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
  SetLength(fSumOfProbs,value);
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
  normalize;
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

(*
        TSmallCountPick
                              *)
procedure TSmallCountPick.SetCount(value: Integer);
begin
  fCount:=value;
  SetLength(fProb,value);
  SetLength(fSumOfProbs,value);
  SetLength(fIndexes,value);
  fchanged:=true;
end;

procedure TSmallCountPick.NewPrepare;
var s: Real;
    i: Integer;
begin
  normalize;
  s:=0;
  fNonZeroCount:=0;
  for i:=0 to fCount-1 do
    if prob[i]>0 then begin
      s:=s+prob[i];
      fSumOfProbs[fNonZeroCount]:=s;
      fIndexes[fNonZeroCount]:=i;
      inc(fNonZeroCount);
    end;
  if fNonZeroCount=1 then
    fPickingProc:=NoChoice
  else if fNonZeroCount=2 then
    fPickingProc:=OneOfTwo
  else fPickingProc:=OneOfMany;
  fchanged:=false;
end;

function TSmallCountPick.NoChoice: Integer;
begin
  Result:=fIndexes[0];
end;

function TSmallCountPick.OneOfTwo: Integer;
begin
  if Random<fSumOfProbs[0] then Result:=fIndexes[0] else Result:=fIndexes[1];
end;

function TSmallCOuntPick.OneofMany: Integer;
var i: Integer;
    r: Real;
begin
  r:=Random;
  i:=0;
  while r>fSumOfProbs[i] do inc(i);
  Result:=fIndexes[i];
end;

function TSmallCountPick.Pick: Integer;
begin
  if fChanged then NewPrepare;
  Result:=fPickingProc;
end;

(*
      TNoChoicePick
                        *)
procedure TNoChoicePick.SetCount(value: Integer);
begin
  assert(value=1,'unable to change count');
end;

function TNoChoicePick.GetCount: Integer;
begin
  Result:=1;
end;

procedure TNoChoicePick.SetProb(index: Integer; value: Real);
begin

end;

function TNoChoicePick.GetProb(index: Integer): Real;
begin
  Result:=1;
end;

function TNoChoicePick.Pick: Integer;
begin
  Result:=0;
end;

(*
    TOneOfTwoChoicePick
                            *)
procedure TOneOfTwoChoicePick.SetCount(value: Integer);
begin
  assert(value=2,'unable to change count');
end;

function TOneOfTwoChoicePick.GetCount: Integer;
begin
  Result:=2;
end;

procedure TOneOfTwoChoicePick.SetProb(index: Integer; value: Real);
begin
  fprob[index]:=value;
end;

function TOneOfTwoChoicePick.GetProb(index: Integer): Real;
begin
  Result:=fprob[index];
end;

function TOneOfTwoChoicePick.Pick: Integer;
begin
  if Random*(fprob[0]+fprob[1])<fprob[0] then Result:=0
  else Result:=1;
end;

(*
    TPermanentOneOfTwoChoicePick
                                    *)
procedure TPermanentOneOfTwoChoicePick.SetCount(value: Integer);
begin
  assert(value=2,'PermanentOneOfTwoChoicePick: unable to change count');
end;

function TPermanentOneOfTwoChoicePick.GetCount: Integer;
begin
  Result:=2;
end;

procedure TPermanentOneOfTwoChoicePick.SetProb(index: Integer; value: Real);
begin
  fprob[index]:=value;
  fthreshold:=fprob[0]/(fprob[0]+fprob[1]);
end;

function TPermanentOneOfTwoChoicePick.GetProb(index: Integer): Real;
begin
  Result:=fprob[index];
end;

function TPermanentOneOfTwoChoicePick.Pick: Integer;
begin
  if Random<fthreshold then Result:=0
  else Result:=1;
end;

//фабрика
function CreateAppropriatePick(count: Integer;PickToChangeRatio: Real=1000): TDiscretePick;
begin
  if count=0 then Raise Exception.Create('CreateAppropriatePick: zero count');
  if count=1 then Result:=TNoChoicePick.Create
  else if count=2 then begin
    if PickToChangeRatio>10 then Result:=TPermanentOneOfTwoChoicePick.Create
    else
    Result:=TOneOfTwoChoicePick.Create;
    end
  else begin
    Result:=TSimplestPick.create;
    Result.Count:=count;
  end;
end;

end.
