unit simple_statistics_lib;

interface
uses classes,series;

type

TAbstract1DStats=class(TPersistent)
  protected
    fmin,fmax,fsum,fsumSquares: Real;
    fcount: Integer;
    function getRMS: Real; virtual; abstract;
    function getStdDev: Real; virtual; abstract;
    function getSampleStdDev: Real; virtual; abstract;
    function GetAve: Real; virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure Clear; virtual;
    procedure Add(value: Real); virtual; abstract;
    procedure Append(values: TAbstract1DStats); virtual; abstract;
  published
    property sum: Real read fsum;
    property sum_squares: Real read fsumSquares;
    property count: Integer read fcount;
    property rms: Real read getRMS;
    property std_dev: Real read getStdDev;
    property sample_std_dev: Real read getSampleStdDev;
    property ave: Real read GetAve;
    property max: Real read fmax;
    property min: Real read fmin;
  end;

T1DstatsOld=class(TAbstract1DStats)
  protected
    function getStdDev: Real; override;
    function getSampleStdDev: Real; override;
    function getAve: Real; override;
    function getRMS: Real; override;
  public
    procedure Add(value: Real); override;
    procedure Append(values: TAbstract1DStats); override;
  end;

T1Dstats=class(TAbstract1DStats) //better then 1DstatsOld because it computes stdDev with
//less error, situation of square root of negative val. is impossible
//but append procedure still missing.
  protected
    fSumDiff: Real;
    function getAve: Real; override;
    function getRMS: Real; override;
    function getStdDev: Real; override;
    function getSampleStdDev: Real; override;
  public
    procedure Clear; override;
    procedure Add(value: Real); override;
    procedure Append(values: TAbstract1DStats); override;
  end;

T1DFullStats=class(T1DStats)  //not only computes min,max,ave,std_dev etc,
//but also can draw distribution function
//median is possible,too
  protected
    fvalues: TList;
  public
    constructor Create; virtual;
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Add(value: Real); override;
    procedure Draw(series: TLineSeries);
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
function RealCompareFunc(P1,P2: Pointer): Integer;



implementation

uses SysUtils;
(*
      TAbstract1DStats
                        *)
procedure TAbstract1Dstats.AfterConstruction;
begin
  inherited AfterConstruction;
  Clear;
end;

procedure TAbstract1DStats.Clear;
begin
  fsum:=0;
  fsumSquares:=0;
  fcount:=0;
  fmax:=-1.0/0.0;
  fmin:=1.0/0.0;
end;


(*
      T1DstatsOld
                      *)
procedure T1DstatsOld.Add(value: Real);
begin
  fSum:=fSum+value;
  fSumSquares:=fSumSquares+value*value;
  if value>fmax then fmax:=value;
  if value<fmin then fmin:=value;
  inc(fcount);
end;

procedure T1DstatsOld.Append(values: TAbstract1DStats);
begin
  fcount:=fcount+values.count;
  fsum:=fsum+values.sum;
  fSumSquares:=fSumSquares+values.Sum_Squares;
  if values.min<fmin then fmin:=values.min;
  if values.max>fmax then fmax:=values.max;
end;

function T1DstatsOld.GetAve: Real;
begin
  if fcount=0 then Raise Exception.Create('T1Dstats.ave: empty list');
  Result:=fsum/fcount;
end;

function T1DstatsOld.getStdDev: Real;
begin
  result:=sqrt(fSumSquares/fcount-getAve*getAve);
end;

function T1DStatsOld.getSampleStdDev: Real;
begin
  result:=getStdDev*sqrt(fCount/(fCount-1));
end;

function T1DstatsOld.getRMS: Real;
begin
  result:=sqrt(fsumSquares/fcount);
end;

(*
        T1DStats
                            *)
procedure T1DStats.Clear;
begin
  inherited Clear;
  fSumDiff:=0;
end;

procedure T1DStats.Add(value: Real);
begin
  //пока что get_ave возвращает среднее от занесенных чисел, т.е без value
  if fcount=0 then fSumDiff:=0
  else fSumDiff:=fSumDiff+fcount/(fcount+1)*Sqr(value-getAve);
  inc(fcount);
  fsum:=fsum+value;
  fSumSquares:=fSumSquares+value*value;
  if value>fmax then fmax:=value;
  if value<fmin then fmin:=value;
end;

procedure T1Dstats.Append(values: TAbstract1Dstats);
begin
(*
  fcount:=fcount+values.fcount;
  fsum:=fsum+values.fsum;
  fSumSquares:=fSumSquares+values.fSumSquares;
  if values.fmin<fmin then fmin:=values.fmin;
  if values.fmax>fmax then fmax:=values.fmax;
  *)
  Raise Exception.Create('T1Dstats.append: stdDev value under construction (have to do later)');
end;

function T1Dstats.getAve: Real;
begin
  if fcount=0 then Raise Exception.Create('T1Dstats.ave: empty list');
  Result:=fsum/fcount;
end;

function T1Dstats.getRMS: Real;
begin
  result:=sqrt(fSumSquares/fcount);
end;

function T1DStats.getStdDev: Real;
begin
  result:=sqrt(fSumDiff/fcount);
end;

function T1Dstats.getSampleStdDev: Real;
begin
  result:=sqrt(fSumDiff/(fcount-1));
end;

(*
        T1DFullStats
                            *)
constructor T1DFullStats.Create;
begin
  inherited Create;
  fvalues:=TList.Create;
end;

procedure T1DFullStats.Clear;
var i: Integer;
begin
  inherited Clear;
  for i:=0 to fvalues.Count-1 do
    Dispose(PDouble(fvalues[i]));
  fvalues.Clear;
end;

destructor T1DFullStats.Destroy;
begin
  Clear;
  fvalues.Free;
  inherited Destroy;
end;

procedure T1DFullStats.Add(value: Real);
var pval: PDouble;
begin
  inherited Add(value);
  New(pval);
  pval^:=value;
  fvalues.Add(pval);
end;

procedure T1DFullStats.Draw(series: TLineSeries);
var i: Integer;
begin
  fvalues.Sort(RealCompareFunc);
  series.Clear;
  for i:=0 to fvalues.Count-1 do begin
    series.AddXY(PDouble(fvalues[i])^,i/fvalues.Count);
    series.AddXY(PDouble(fvalues[i])^,(i+1)/fvalues.Count);
  end;
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

function RealCompareFunc(P1,P2: Pointer): Integer;
begin
  if PDouble(P1)^<PDouble(P2)^ then
    Result:=-1
  else if PDouble(P1)^>PDouble(P2)^ then
    Result:=1
  else Result:=0;
end;


end.
