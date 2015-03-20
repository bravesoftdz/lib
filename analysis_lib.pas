unit analysis_lib;

interface

uses classes,linear_eq,streaming_class_lib,streamable_component_list,expression_lib;

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
    fVariable: TAbstractExpression;
    fMinVal,fMaxVal,fIncr: TVariantExpression;
  public
    constructor Create(Owner: TComponent); override;
    function NumberOfPoints: Integer;
    function GetPoint(index: Integer): Variant;
  published
    property Enabled: Boolean read fEnabled write fEnabled default false;
    property Variable: TAbstractExpression read fVariable write fVariable;
    property MinVal: TVariantExpression read fMinVal write fMinVal;
    property MaxVal: TVariantExpression read fMaxVal write fMaxVal;
    property Incr: TVariantExpression read fIncr write fIncr;
    //если лог., то Incr имеет смысл точек на декаду или точек на октаву (тогда еще и отрицат)
    property isLog: Boolean read fIsLog write fIsLog default false;
end;

TChartDetails = record
  LeftAxisGraphs: array of Integer; //номера в страшенной таблице fData
  RightAxisEnabled: boolean;
  RightAxisGraphs: array of Integer;
  caption: string;
end;

TComplexNumbersDisplay = (cndAbsArg,cndReIm,cndGodograph);

TAnalysisShowDetails = class (TStreamingClass)
  private
    fChartCount: Integer;
    fChartDetails: array of TChartDetails;
    fComplexDisplay: TComplexNumbersDisplay;
  public
    property ComplexDisplay: TComplexNumbersDisplay read fComplexDisplay write fComplexDisplay;
end;

TAnalysis = class (TStreamingClass)
  private
    fSimulationType: TSimulationType;
    fVarsOfInterest: TStrings;
    fTempVarsOfInterest: TStrings;
    fExpressions: array of TVariantExpression;
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
    fShowDetails: TAnalysisShowDetails;
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
    procedure ShowOnScreen;
    property ShowDetails: TAnalysisShowDetails read fShowDetails write fShowDetails;
    procedure HandleComplexNumbers;
  published
    property SimulationType: TSimulationType read fSimulationType write fSimulationType;
    property VarsOfInterest: TStrings read fVarsOfInterest write fVarsOfInterest;
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

uses SysUtils,Math,command_class_lib,variants, ComObj, formShowAnalysisResults,
phys_units_lib,VarCmplx,ComCtrls,StrUtils,ConvUtils, chart, controls,graphics,series;

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
constructor TSweep.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fMinVal:=TVariantExpression.CreateZero(self);
  fMaxVal:=TVariantExpression.CreateZero(self);
  fIncr:=TVariantExpression.CreateZero(self);
end;

function TSweep.NumberOfPoints: Integer;
begin
  if enabled then
    if isLog then
      if Incr.GetVariantValue>0 then  //точек на декаду
        Result:=Ceil(log10(MaxVal.GetVariantValue/MinVal.GetVariantValue)*incr.GetVariantValue+1-1e-7)
      else
        Result:=Ceil(log2(MaxVal.GetVariantValue/MinVal.GetVariantValue)*(-incr.GetVariantValue)+1-1e-7)
    else Result:=Ceil((MaxVal.GetVariantValue-MinVal.GetVariantValue)/Incr.GetVariantValue+1)
  else Result:=1; //чтобы не убить внутренние циклы
end;

function TSweep.GetPoint(index: Integer): Variant;
begin
  if enabled then
    if isLog then
      if Incr.GetVariantValue>0 then
        Result:=MinVal.GetVariantValue*power(10,index/incr.GetVariantValue)
      else
        Result:=MinVal.GetVariantValue*power(2,index/(-incr.GetVariantValue))
    else
      Result:=MinVal.GetVariantValue+index*Incr.GetVariantValue
  else Raise Exception.Create('TSweep.GetPoint: can''t return value when disabled');
end;

(*
      TAnalysis
                    *)
constructor TAnalysis.Create(Owner: TComponent);
var i: Integer;
begin
  inherited Create(Owner);
  fVarsOfInterest:=TStringList.Create;
  fTempVarsOfInterest:=TStringList.Create;
  for i:=0 to 1 do begin
    fSweeps[i]:=TSweep.Create(self);
    fSweeps[i].SetSubComponent(true);
  end;
  fShowDetails:=TAnalysisShowDetails.Create(self);
  fShowDetails.SetSubComponent(true);
end;

destructor TAnalysis.Destroy;
var i: Integer;
begin
  FreeAndNil(fThread);
  for i:=0 to Length(fclones)-1 do
    if Assigned(fclones[i]) and Assigned(fclones[i].fThread) then
      fclones[i].fThread.Terminate;

  //но тут мы их должны дождаться!
  fVarsOfInterest.Free;
  fTempVarsOfInterest.Free;
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
    our_copy.fSweeps[fsweepIndex].MinVal.SetString(fSweeps[fsweepIndex].GetPoint(i*pointsPerThread));
    offsets[i]:=i*pointsPerThread;
    if i=count-1 then
      our_copy.fSweeps[fsweepIndex].MaxVal.SetString(fSweeps[fsweepIndex].GetPoint(NumOfPoints-1))
    else
      our_copy.fSweeps[fsweepIndex].MaxVal.setString(fSweeps[fsweepIndex].GetPoint((i+1)*pointsPerThread-1));

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
  fTempVarsOfInterest.Assign(fVarsOfInterest);
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
      WriteLn(F,'Variable: '+SecondarySweep.variable.name);
      s:='Values: '+SecondarySweep.fMinVal.GetVariantValue+'..'+SecondarySweep.MaxVal.GetVariantValue+', step '+SecondarySweep.fIncr.GetVariantValue;
      if SecondarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
    end;
      WriteLn(F,'Primary sweep:');
      WriteLn(F,'Variable: '+PrimarySweep.variable.name);
      s:='Values: '+PrimarySweep.fMinVal.GetVariantValue+'..'+PrimarySweep.MaxVal.GetVariantValue+', step '+PrimarySweep.fIncr.GetVariantValue;
      if PrimarySweep.fIsLog then s:=s+', log. scale';
      WriteLn(F,s);
  //заголовок готов
  for i:=0 to SecondarySweep.NumberOfPoints-1 do begin
    if SecondarySweep.Enabled then
      WriteLn(F,SecondarySweep.GetPoint(i));
    //и напоминаем названия переменных
    s:=PrimarySweep.Variable.name+#9;
    for k:=0 to fTempVarsOfInterest.Count-1 do
      s:=s+fTempVarsOfInterest[k]+#9;
    WriteLn(F,s);
    for j:=0 to PrimarySweep.NumberOfPoints-1 do begin
      s:=PrimarySweep.GetPoint(j);
      s:=s+#9;
      for k:=0 to fTempVarsOfInterest.Count-1 do begin
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
    i,j,k: Integer;
    strType,s: string;
    rowCount: Integer;
begin
  try
    ExcelApp:=CreateOleObject('Excel.Application');
    ExcelApp.visible:=false;
    ExcelDoc:=ExcelApp.Workbooks.Add;
    ExcelSht:=ExcelDoc.Worksheets.Add;
    ExcelSht.name:=name;
    rowCount:=1;
    if AnalysisTypeToName(fSimulationType,strType) then begin
      ExcelSht.Cells.Item[rowCount,1]:=strType;
      inc(rowCount);
    end;
    if SecondarySweep.Enabled then begin
      ExcelSht.Cells.Item[rowCount,1]:='Secondary sweep: enabled';
      inc(rowCount);
      ExcelSht.Cells.Item[rowCount,1]:='Variable:';
      ExcelSht.Cells.Item[rowCount,2]:=SecondarySweep.Variable.name;
      inc(rowCount);
      ExcelSht.Cells.Item[rowCount,1]:='Values:';
      ExcelSht.Cells.Item[rowCount,2]:=FloatToStr(SecondarySweep.fMinVal.GetVariantValue);
      ExcelSht.Cells.Item[rowCount,3]:='..';
      ExcelSht.Cells.Item[rowCount,4]:=FloatToStr(SecondarySweep.fMaxVal.GetVariantValue);
      ExcelSht.Cells.Item[rowCount,5]:='Step:';
      ExcelSht.Cells.Item[rowCount,6]:=FloatToStr(SecondarySweep.Incr.GetVariantValue);
      if SecondarySweep.isLog then
        ExcelSht.Cells.Item[rowCount,7]:=', log.scale';
    end;
    ExcelSht.Cells.Item[rowCount,1]:='Primary sweep:';
    inc(rowCount);
    ExcelSht.Cells.Item[rowCount,1]:='Variable:';
    ExcelSht.Cells.Item[rowCount,2]:=PrimarySweep.Variable.name;
    inc(rowCount);
    ExcelSht.Cells.Item[rowCount,1]:='Values:';
    s:=PrimarySweep.fMinVal.GetVariantValue;
    ExcelSht.Cells.Item[rowCount,2]:=s;
    ExcelSht.Cells.Item[rowCount,3]:='..';
    s:=PrimarySweep.fMaxVal.GetVariantValue;
    ExcelSht.Cells.Item[rowCount,4]:=s;
    ExcelSht.Cells.Item[rowCount,5]:='Step:';
    s:=PrimarySweep.Incr.GetVariantValue;
    ExcelSht.Cells.Item[rowCount,6]:=s;
    if PrimarySweep.isLog then
      ExcelSht.Cells.Item[rowCount,7]:=', log.scale';
  //заголовок готов
    inc(rowCount);
    for i:=0 to SecondarySweep.NumberOfPoints-1 do begin
      if SecondarySweep.Enabled then begin
        ExcelSht.Cells.Item[rowCount,1]:=SecondarySweep.GetPoint(i);
        inc(rowCount);
      end;
      //и напоминаем названия переменных
      ExcelSht.Cells.Item[rowCount,1]:=PrimarySweep.Variable.name;
      for k:=0 to fTempVarsOfInterest.Count-1 do
        ExcelSht.Cells.Item[rowCount,k+2]:=fTempVarsOfInterest[k];
      inc(rowCount);
      for j:=0 to PrimarySweep.NumberOfPoints-1 do begin

        s:=PrimarySweep.GetPoint(j);
        ExcelSht.Cells.Item[rowCount,1]:=s;
        for k:=0 to fTempVarsOfInterest.Count-1 do begin
          s:=fdata[j,i,k];
          ExcelSht.Cells.Item[rowCount,k+2]:=s;
        end;
        inc(rowCount);
      end;
    end;

  finally
    if not VarIsEmpty(ExcelApp) then ExcelApp.visible:=true;
    ExcelApp:=UnAssigned;
  end;

end;

procedure TAnalysis.HandleComplexNumbers;
var i,j,k,L: Integer;
    incr: Real;
    fullturn: Variant;
begin
  fullturn:=VarWithUnitCreate('1 turn');
  //первым делом с комплексными числами разберемся, если таковые есть
  for i:=0 to fTempVarsOfInterest.Count-1 do
    if VarIsComplex(VarWithUnitGetNumber(fdata[0,0,i])) then
      if fShowDetails.fComplexDisplay=cndAbsArg then begin
        fTempVarsOfInterest.Add('arg('+fTempVarsOfInterest[i]+')');
        fTempVarsOfInterest[i]:='abs('+fTempVarsOfInterest[i]+')';
        L:=fTempVarsOfInterest.Count-1;
        SetLength(fdata,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,L+1);
        for j:=0 to PrimarySweep.NumberOfPoints-1 do
          for k:=0 to SecondarySweep.NumberOfPoints-1 do begin
            fdata[j,k,L]:=VarWithUnitArg(fdata[j,k,i]);

            if j>0 then begin
              incr:=VarWithUnitGetNumberIn(fdata[j,k,L]-fdata[j-1,k,L],auRadian);
              if abs(incr)>pi then begin
                if incr>0 then fdata[j,k,L]:=fdata[j,k,L]-fullturn
                else fdata[j,k,L]:=fdata[j,k,L]+fullturn;
              end;
            end;

            fdata[j,k,i]:=VarWithUnitAbs(fdata[j,k,i]);
          end;
      end
      else if fShowDetails.fComplexDisplay=cndReIm then begin
        fTempVarsOfInterest.Add('Im('+fTempVarsOfInterest[i]+')');
        fTempVarsOfInterest[i]:='Re('+fTempVarsOfInterest[i]+')';
        L:=fTempVarsOfInterest.Count-1;
        SetLength(fdata,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,L+1);
        for j:=0 to PrimarySweep.NumberOfPoints-1 do
          for k:=0 to SecondarySweep.NumberOfPoints-1 do begin
            fdata[j,k,L]:=VarWithUnitIm(fdata[j,k,i]);
            fdata[j,k,i]:=VarWithUnitRe(fdata[j,k,i]);
          end;
      end;
  //если выбран вариант cndGodograph, то оставляем числа в покое, потом построим хитрый график
end;

procedure TAnalysis.ShowOnScreen;
var i,j,k,col: Integer;
    VariousTypes: array of Variant;
    graphscount: array of Integer;
    chartnumbers: array of Integer;
    found: boolean;
    fTabSheet: TTabSheet;
    fChart: TChart;
    fSeries: TLineSeries;
    XConv,YConv: TConvType;
begin
  //пока что здесь придумаем, как лучше всего распихать кривые по графикам
  if SecondarySweep.Enabled then begin
    //каждый график на своем полотне, вдруг иначе не уместится
    fShowDetails.fChartCount:=fTempVarsOfInterest.Count;
    SetLength(fShowDetails.fChartDetails,fShowDetails.fChartCount);
    for i:=0 to fShowDetails.fChartCount-1 do begin
      SetLength(fShowDetails.fChartDetails[i].LeftAxisGraphs,1);
      fShowDetails.fChartDetails[i].LeftAxisGraphs[0]:=i;
      fShowDetails.fChartDetails[i].RightAxisEnabled:=false;
      fShowDetails.fChartDetails[i].caption:=fTempVarsOfInterest[i];
    end;
  end
  else begin
    //пока что утрамбуем все величины одной размерности на одном полотне
    k:=0;
    SetLength(chartnumbers,fTempVarsOfInterest.Count);
    for i:=0 to fTempVarsOfInterest.Count-1 do begin
      found:=false;
      for j:=0 to k-1 do
        if IsVarWithUnitSameFamily(fdata[0,0,i],VariousTypes[j]) then begin
          found:=true;
          chartnumbers[i]:=j;
          inc(graphsCount[j]);
          break;
        end;
      if not found then begin
        inc(k);
        SetLength(VariousTypes,k);
        SetLength(GraphsCount,k);
        VariousTypes[k-1]:=fdata[0,0,i];
        GraphsCount[k-1]:=1;
        chartnumbers[i]:=k-1;
      end;
    end;
    //нашли количество разных размерностей и "представителя" от каждой
    fShowDetails.fChartCount:=k;
    SetLength(fShowDetails.fChartDetails,k);
    for i:=0 to fShowDetails.fChartCount-1 do begin
      SetLength(fShowDetails.fChartDetails[i].LeftAxisGraphs,graphsCount[i]);
      fShowDetails.fChartDetails[i].RightAxisEnabled:=false;
      fShowDetails.fChartDetails[i].caption:=ConvFamilyToDescription(VarWithUnitGetConvFamily(VariousTypes[i]));
    end;
    //ага, распихиваем наши величины по разным местам
    for i:=0 to k-1 do
      graphsCount[i]:=0;
    for i:=0 to fTempVarsOfInterest.Count-1 do begin
      fShowDetails.fChartDetails[chartnumbers[i]].LeftAxisGraphs[graphsCount[chartnumbers[i]]]:=i;
      inc(graphsCount[chartnumbers[i]]);
    end;
  end;

  XConv:=VarWithUnitGetConvType(PrimarySweep.Variable.getVariantValue);
  //а теперь собственно построение графиков. Для начала создадим компоненты.
  for i:=0 to fShowDetails.fChartCount-1 do begin
    fTabSheet:=TTabSheet.Create(frmShowAnalysisResults);
    fTabSheet.PageControl:=frmShowAnalysisResults.PageControl1;
    fTabSheet.Caption:=fShowDetails.fChartDetails[i].caption;
    fChart:=TChart.Create(frmShowAnalysisResults);
    fChart.Parent:=fTabSheet;
    fChart.Align:=alClient;
    fChart.Title.Text.Add(fTabSheet.Caption);
    fChart.Title.Font.Color:=clBlack;
    fChart.Title.Font.Size:=-14;
    fChart.Color:=clWhite;
    fChart.View3D:=false;
    fChart.BottomAxis.Logarithmic:=PrimarySweep.isLog;
    if SecondarySweep.Enabled then begin

    end
    else
      for j:=0 to Length(fShowDetails.fChartDetails[i].LeftAxisGraphs)-1 do begin
        fSeries:=TLineSeries.Create(fChart);
        fSeries.ParentChart:=fChart;
        fSeries.LinePen.Width:=2;
        col:=fShowDetails.fChartDetails[i].LeftAxisGraphs[j];
        fSeries.Title:=fTempVarsOfInterest[col];
        YConv:=VarWithUnitGetConvType(fdata[0,0,col]);
        for k:=0 to PrimarySweep.NumberOfPoints-1 do
          fSeries.AddXY(VarWithUnitGetNumberIn(PrimarySweep.GetPoint(k),XConv),VarWithUnitGetNumberIn(fdata[k,0,col],YConv));


      end;
  end;

  frmShowAnalysisResults.ShowModal;

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
begin
  try
//внешний цикл - по secondarySweep, если он есть
  with fAnalysis do begin
    SetLength(fExpressions,VarsOfInterest.Count);
    for i:=0 to VarsOfInterest.Count-1 do begin
      fExpressions[i]:=TVariantExpression.Create(nil);
      fExpressions[i].SetString(VarsOfInterest[i]);
      fExpressions[i].SetRootComponent(fObject.Implementor);
    end;
    SetLength(fData,PrimarySweep.NumberOfPoints,SecondarySweep.NumberOfPoints,VarsOfInterest.Count);
    for j:=0 to SecondarySweep.NumberOfPoints-1 do begin
      if SecondarySweep.Enabled then
        SecondarySweep.Variable.SetString(SecondarySweep.GetPoint(j));
      for i:=0 to fAnalysis.PrimarySweep.NumberOfPoints-1 do begin
        if PrimarySweep.Enabled then
          PrimarySweep.Variable.SetString(PrimarySweep.GetPoint(i));
        fObject.RunSimulation(SimulationType);
        for k:=0 to VarsOfInterest.Count-1 do
          fData[i,j,k]:=fExpressions[k].GetVariantValue;
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
