unit analysis_lib;

interface

uses classes,linear_eq,streaming_class_lib,streamable_component_list;

type

TSimulationType = -$7FFFFFFF-1..$7FFFFFFF;

IObjectForAnalysis=interface
  ['{303E2364-6CBD-4AAB-BFE1-7C728F0BAF6A}']
  procedure RunSimulation(SimulationType: TSimulationType);
  function isSeparationEnabledBy(variable: IEquationNode): Boolean;
  //да, время и частота тоже должны быть IEquationNode, чтобы единица изм. своя была и пр.
  function Implementor: TStreamingClass;
end;

TSweep = class (TComponent)
  private
    fEnabled,fIsLog: Boolean;
    fVariable: IEquationNode;
    fMinVal,fMaxVal,fIncr: Real;
  public
    function NumberOfPoints: Integer;
    function GetPoint(index: Integer): Real;
  published
    property Enabled: Boolean read fEnabled write fEnabled default false;
    property Variable: IEquationNode read fVariable write fVariable;
    property MinVal: Real read fMinVal write fMinVal;
    property MaxVal: Real read fMaxVal write fMaxVal;
    property Incr: Real read fIncr write fIncr;
    //если лог., то Incr имеет смысл точек на декаду или точек на октаву (тогда еще и отрицат)
    property isLog: Boolean read fIsLog write fIsLog default false;
end;

TAnalysis = class (TStreamingClass)
  private
    fSimulationType: TSimulationType;
    fVarsOfInterest: TStreamableComponentList;
    fSweeps: array [0..1] of TSweep;
    fOrigin: TAnalysis; //он создал свои копии
    fThread: TThread; //поток, к нам привязанный
    fIndex: Integer; //номер клона
    fProgress: array of Integer;  //проценты выполнения в потоках
    fClones: array of TAnalysis;
    offsets: array of Integer;
    fsweepIndex: Integer;

    fData: array of array of array of Variant; //возможны комплексные числа, а также недоопред.
    fPrimaryCount,fSecondaryCount: Integer; //на чем мы остановились в одном и в другом
    procedure RunThread(Origin: TAnalysis; index: Integer);
    procedure AppendThreadResults(clone: TAnalysis; exMessage: string ='');
  protected
    procedure OnThreadTerminate(Sender: TObject); //достаточно иметь одну ссылочку на процесс
    procedure ShowProgress(index,value: Integer);
  public
    onReady: TNotifyEvent;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Run;  //создает клоны схемы, разбивает интервалы sweep для каждого и запускает в них потоки
    procedure SaveToTextFile(FileName: string);
    procedure ExportToExcel;
  published
    property SimulationType: TSimulationType read fSimulationType write fSimulationType;
    property VarsOfInterest: TStreamableComponentList read fVarsOfInterest write fVarsOfInterest;
    property PrimarySweep: TSweep read fSweeps[0] write fSweeps[0];
    property SecondarySweep: TSweep read fSweeps[1] write fSweeps[1];
end;

TAnalysisThread = class (TThread)
  private
    fAnalysis: TAnalysis;
    fObject: IObjectForAnalysis;
    fPercentDone: Integer;
    fExceptionMessage: string;
  protected
    procedure Execute; override;
    procedure Progress;
  public
    constructor Create(aAnalysis: TAnalysis);
end;

function RegisterSimulationType(description: string): TSimulationType;

var stTransient,stAC,stDC, stUndefined : TSimulationType;
    NumberOfAnalysisThreads: Integer;

implementation

uses SysUtils,Math,command_class_lib,variants, ComObj;

var AnalysisTypes: TStrings;

function RegisterSimulationType(description: string): TSimulationType;
begin
  Result:=AnalysisTypes.Add(description);
end;

function AnalysisTypeToName(Int: LongInt; var Ident: string): Boolean;
begin
  Result:=(Int>=0) and (Int<AnalysisTypes.Count);
  if Result then Ident:=AnalysisTypes[Int];
end;

function NameToAnalysisType(const Ident: string; var Int: LongInt): Boolean;
begin
  Int:=AnalysisTypes.IndexOf(ident);
  Result:=(Int>0);
end;

function GetObjectForAnalysis(obj: TComponent): IObjectForAnalysis;
begin
  while assigned(obj) do begin
    if obj.GetInterface(IObjectForAnalysis,Result) then Exit;
    obj:=obj.Owner;
  end;
  Raise Exception.Create('GetObjectForAnalysis: didn''t find IObjectForAnalysis');
end;

(*
      TSweep
                    *)
function TSweep.NumberOfPoints: Integer;
begin
  if enabled then
    if isLog then
      if Incr>0 then  //точек на декаду
        Result:=Ceil(log10(MaxVal/MinVal)*incr+1-1e-7)
      else
        Result:=Ceil(log2(MaxVal/MinVal)*(-incr)+1-1e-7)
    else Result:=Ceil((MaxVal-MinVal)/Incr+1)
  else Result:=1; //чтобы не убить внутренние циклы
end;

function TSweep.GetPoint(index: Integer): Real;
begin
  if enabled then
    if isLog then
      if Incr>0 then
        Result:=MinVal*power(10,index/incr)
      else
        Result:=MinVal*power(2,index/(-incr))
    else
      Result:=MinVal+index*Incr
  else Raise Exception.Create('TSweep.GetPoint: can''t return value when disabled');
end;

(*
      TAnalysis
                    *)
constructor TAnalysis.Create(Owner: TComponent);
var i: Integer;
begin
  inherited Create(Owner);
  fVarsOfInterest:=TStreamableComponentList.Create(self);
  fVarsOfInterest.SetSubComponent(true);
  fVarsOfInterest.Name:='VarsOfInterest';
  for i:=0 to 1 do begin
    fSweeps[i]:=TSweep.Create(self);
    fSweeps[i].SetSubComponent(true);
  end;
end;

destructor TAnalysis.Destroy;
var i: Integer;
begin
  FreeAndNil(fThread);
  for i:=0 to Length(fclones)-1 do
    if Assigned(fclones[i]) and Assigned(fclones[i].fThread) then
      fclones[i].fThread.Terminate;

  //но тут мы их должны дождаться!
  inherited Destroy;
end;

procedure TAnalysis.Run;
var clone: TStreamingClass;
    source: TStreamingClass;
    numOfPoints: Integer;
    i,count,pointsPerThread: Integer;
    our_copy: TAnalysis;
begin
  SetLength(fdata,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,VarsOfInterest.Count);
  fPrimaryCount:=0;
  fSecondaryCount:=0;
  //а удастся ли вообще их сколько надо склонировать?
  if SecondarySweep.Enabled then fsweepIndex:=1 else fsweepIndex:=0;
  //простейшее поведение - расщепляем на потоки по самому последнему sweep
  numOfPoints:=fSweeps[fsweepIndex].NumberOfPoints;
  count:=min(NumberOfAnalysisThreads,numOfPoints);
  SetLength(fProgress,count);
  SetLength(fClones,count);
  SetLength(offsets,count);
  pointsPerThread:=Round(NumOfPoints/NumberOfAnalysisThreads);
  source:=GetObjectForAnalysis(self).Implementor;

  for i:=0 to count-1 do begin
    clone:=TStreamingClass.CloneComponent(source) as TStreamingClass;
    //теперь ищем в нем себя и меняем интервалы
    our_copy:=clone.FindComponent(Name) as TAnalysis;
    our_copy.fSweeps[fsweepIndex].MinVal:=fSweeps[fsweepIndex].GetPoint(i*pointsPerThread);
    offsets[i]:=i*pointsPerThread;
    if i=count-1 then
      our_copy.fSweeps[fsweepIndex].MaxVal:=fSweeps[fsweepIndex].GetPoint(NumOfPoints-1)
    else
      our_copy.fSweeps[fsweepIndex].MaxVal:=fSweeps[fsweepIndex].GetPoint((i+1)*pointsPerThread-1);

    clone.saveFormat:=fCyr;
    clone.SaveToFile('clone'+IntToStr(i)+'.txt');

    fClones[i]:=our_copy;
    our_copy.RunThread(self,i); //запускается, а потом вызовет нас назад, когда завершится
  end;
end;


procedure TAnalysis.RunThread(origin: TAnalysis; index: Integer);
begin
  fOrigin:=origin;
  fIndex:=index;
  fThread:=TAnalysisThread.Create(self);
end;

procedure TAnalysis.OnThreadTerminate(Sender: TObject);
var ExMessage: string;
begin
  ExMessage:=(fThread as TAnalysisThread).fExceptionMessage;
  fThread:=nil;
  fOrigin.AppendThreadResults(self,ExMessage);
end;

procedure TAnalysis.ShowProgress(index,value: Integer);
var i: Integer;
    s: string;
begin
  fProgress[index]:=value;
  for i:=0 to Length(fProgress)-1 do begin
    if fProgress[i]<0 then
      s:=s+'Err'
    else
      s:=s+IntToStr(fProgress[i])+'%';
    if i<Length(fProgress)-1 then
      s:=s+'  ';
  end;

  if FindOwner is TAbstractDocument then
    TAbstractDocument(FindOwner).DoneStatusPanel.Text:=s;
end;

procedure TAnalysis.AppendThreadResults(clone: TAnalysis; exMessage: string = '');
var i,j,k: Integer;
    isReady: Boolean;
    obj: TComponent;
    iobj: IObjectForAnalysis;
begin
  fPrimaryCount:=offsets[clone.fIndex];
  fSecondaryCount:=0;
  if fSweepIndex=1 then SwapIntegers(fPrimaryCount,fSecondaryCount);
  if exMessage='' then begin
    //присоединение данных
    for i:=0 to clone.PrimarySweep.NumberOfPoints-1 do
      for j:=0 to clone.SecondarySweep.NumberOfPoints-1 do
        for k:=0 to clone.VarsOfInterest.Count-1 do
          if not VarIsEmpty(clone.fData[i,j,k]) then
            fdata[i+fPrimaryCount,j+fSecondaryCount,k]:=clone.fData[i,j,k];
  end;
  //GetObjectForAnalysis(clone).Implementor.Free; //ох, это ж замкнутый круг
//удаление гланд через задницу
  iobj:=GetObjectForAnalysis(clone); //чтобы явным образом знать время жизни этой ссылки
  obj:=iobj.Implementor;
  iobj:=nil; //происходит уменьшение счетчика ссылок на 1, отвязались...
  obj.Free; //можно и удалить
//точнее, удаление объекта через интерфейсную ссылку
  isReady:=true;
  for i:=0 to Length(fclones)-1 do begin
    if clone=fclones[i] then
      fclones[i]:=nil;
    if fclones[i]<>nil then isReady:=false;
  end;
  if isReady and Assigned(onReady) then OnReady(self);
(*
  if exMessage<>'' then
    Raise Exception.Create(ExMessage);
    *)
end;

procedure TAnalysis.SaveToTextFile(FileName: string);
var F: TextFile;
    s,v,strType: string;
    i,j,k: Integer;
begin
  AssignFile(F,FileName);
  try
    Rewrite(F);
    WriteLn(F,name);
    if AnalysisTypeToName(fSimulationType,strType) then
      WriteLn(F,'type: '+strType);
    if SecondarySweep.Enabled then begin
      WriteLn(F,'Secondary sweep: enabled');
      WriteLn(F,'Variable: '+SecondarySweep.variable.ShowNodeName);
      s:='Values: '+FloatToStr(SecondarySweep.fMinVal)+' .. '+FloatToStr(SecondarySweep.fMaxVal)+', step '+FloatToStr(SecondarySweep.fIncr);
      if SecondarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
    end;
      WriteLn(F,'Primary sweep:');
      WriteLn(F,'Variable: '+PrimarySweep.variable.ShowNodeName);
      s:='Values: '+FloatToStr(PrimarySweep.fMinVal)+' .. '+FloatToStr(PrimarySweep.fMaxVal)+', step '+FloatToStr(PrimarySweep.fIncr);
      if PrimarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
  //заголовок готов
  for i:=0 to SecondarySweep.NumberOfPoints-1 do begin
    if SecondarySweep.Enabled then
      WriteLn(F,SecondarySweep.variable.ShowValue(SecondarySweep.GetPoint(i)));
    for j:=0 to PrimarySweep.NumberOfPoints-1 do begin
      s:=FloatToStr(PrimarySweep.GetPoint(j))+#9;
      for k:=0 to VarsOfInterest.Count-1 do begin
        v:=fdata[j,i,k];
        s:=s+v+#9;
      end;
      WriteLn(F,s);
    end;
  end;

  finally
    CloseFile(F);
  end;
end;

procedure TAnalysis.ExportToExcel;
var ExcelApp: Variant; //само приложение excel
    ExcelDoc: Variant; //новый документ
    ExcelSht: Variant; //новый лист
    i,j: Integer;
    cellname: string;
    val: Extended;
begin
  try
    ExcelApp:=CreateOleObject('Excel.Application');
    ExcelApp.visible:=false;
    ExcelDoc:=ExcelApp.Workbooks.Add;
    ExcelSht:=ExcelDoc.Worksheets.Add;
    ExcelSht.name:=name;
(*
    for j:=0 to RowCount-1 do
      for i:=0 to ColCount-1 do begin
        cellname:=Chr(Integer('A')+i)+IntToStr(j+1);
//        ExcelSht.Range[cellname,cellname]:=Cells[i,j];
//очень странно он обращается с числами с плав. точкой - убивает в них запятую
          if Cells[i,j]<>'' then begin
            if TryStrToFloat(Cells[i,j],val) then
              ExcelSht.Cells.Item[j+1,i+1]:=val
            else
              ExcelSht.Cells.Item[j+1,i+1]:=Cells[i,j];
          end;

      end;
      *)
  finally
    if not VarIsEmpty(ExcelApp) then ExcelApp.visible:=true;
    ExcelApp:=UnAssigned;
  end;

end;


(*
        TAnalysisThread
                            *)
constructor TAnalysisThread.Create(aAnalysis: TAnalysis);
begin
  inherited Create(true);
  fAnalysis:=aAnalysis;
  fObject:=GetObjectForAnalysis(fAnalysis);
  FreeOnTerminate:=true;
  onTerminate:=fAnalysis.OnThreadTerminate;
//  Priority:=tpIdle;
  Resume;
end;

procedure TAnalysisThread.Execute;
var i,j,k: Integer;
    node: IEquationNode;
begin
  try
//внешний цикл - по secondarySweep, если он есть
  with fAnalysis do begin
    SetLength(fData,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,VarsOfInterest.Count);
    for j:=0 to SecondarySweep.NumberOfPoints-1 do begin
      if SecondarySweep.Enabled then
        SecondarySweep.Variable.SetValue(SecondarySweep.GetPoint(j));
      for i:=0 to fAnalysis.PrimarySweep.NumberOfPoints-1 do begin
        if PrimarySweep.Enabled then
          PrimarySweep.Variable.SetValue(PrimarySweep.GetPoint(i));
        fObject.RunSimulation(SimulationType);
        for k:=0 to VarsOfInterest.Count-1 do
          if VarsOfInterest[k].GetInterface(IEquationNode,node) then begin
            if VarIsEmpty(node.value) then Raise Exception.CreateFMT('TAnalysisThread.Execute: unassigned value in node %s', [node.ShowNodeName]);
            fData[i,j,k]:=node.value;
          end;
        fPercentDone:=Round((j+i/PrimarySweep.NumberOfPoints)/SecondarySweep.NumberOfPoints*100);
        Synchronize(Progress);
      end;
    end;
  end;
  fPercentDone:=100;
  Synchronize(Progress);
  fObject:=nil;
  except
    on Ex: Exception do
      fExceptionMessage:= Ex.Message;
  end;
end;

procedure TAnalysisThread.Progress;
begin
  if fExceptionMessage<>'' then fPercentDone:=-1;
  fAnalysis.fOrigin.ShowProgress(fAnalysis.fIndex,fPercentDone);
end;


initialization
  RegisterClasses([TAnalysis]);
  AnalysisTypes:=TStringList.Create;
  RegisterIntegerConsts(TypeInfo(TSimulationType),NameToAnalysisType,AnalysisTypeToName);
  stUndefined:=RegisterSimulationType('Undefined');
  stTransient:=RegisterSimulationType('Transient');
  stAC:=RegisterSimulationType('AC');
  stDC:=RegisterSimulationType('DC');
  NumberOfAnalysisThreads:=4;
finalization
  FreeAndNil(AnalysisTypes);
end.
