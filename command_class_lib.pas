unit command_class_lib;

interface

uses streaming_class_lib,classes,TypInfo,IdHash,SyncObjs,actnlist,controls,
comctrls,messages,abstract_command_lib;

type
  TAbstractTreeCommand=class(TAbstractCommand)  //чтобы историю изменений можно было хранить вместе со всем остальным
    private
      fNext,fPrev,fBranch: TAbstractTreeCommand;
      fTurnLeft: Boolean;
    protected
      fActiveBranch: Boolean;
    public
      constructor Create(Aowner: TComponent); override;
      procedure Clear; override;
      procedure ResolveMemory; virtual;
      function NameToDateTime: TDateTime;
      function NameToDate(aName: TComponentName): Integer;
      function EqualsByAnyOtherName(what: TStreamingClass): boolean; override;
    published
      property Prev: TAbstractTreeCommand read fPrev write fPrev;
      property Next: TAbstractTreeCommand read fNext write fNext;
      property Branch: TAbstractTreeCommand read fBranch write fBranch;
      property ActiveBranch: Boolean read fActiveBranch write fActiveBranch default false;
      property TurnLeft: Boolean read fTurnLeft write fTurnLeft default false;
    end;

  TInfoCommand=class(TAbstractTreeCommand)
    public
      constructor Create(AOwner: TComponent); override;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function SmartDateTimeToStr: string;
    end;

  TBranchCommand=class(TInfoCommand)
    public
      function caption: string; override;
    end;

  TSavedAsInfoCommand=class(TInfoCommand)
    private
      fFileName: string;
    public
      constructor Create(aFileName: string); reintroduce; overload;
      function caption: string; override;
    published
      property FileName: string read fFileName write fFileName;
    end;

  THashedCommand=class(TAbstractTreeCommand)
    private
      fHash: T4x4LongWordRecord;
      procedure WriteHash(stream: TStream);
      procedure ReadHash(stream: TStream);
      function HashNotEmpty: boolean;
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      function HashIsEqual(value: T4x4LongWordRecord): Boolean;
      constructor Create(owner: TComponent); override;
      property Hash: T4x4LongWordRecord read fHash;
    end;

  THashingThread=class(TThread)
    private
      fcommand: THashedCommand;
      fIsUndo: boolean;
      fErrorString: string;
      procedure ShowError;
    protected
      procedure Execute; override;
    public
      constructor Create(command: THashedCommand;isUndo: Boolean);
  end;


  TChangePropertyCommand=class(TAbstractTreeCommand)
    protected
      fComponent: TStreamingClass;
      fPropName: string;
      fComponentNameStr: string;
      function NewGetPropInfo: PPropInfo;
    public
      Constructor Create(AOwner: TComponent); override;
      procedure ResolveMemory; override;
      function Execute: Boolean; override;
    published
      property Component: TStreamingClass read fComponent write fComponent;
      property PropName: string read fPropName write fPropName;
    end;


  TChangeIntegerCommand=class(TChangePropertyCommand)
    private
      fVal, fBackup: Integer;
    public
      constructor Create(acomponent: TStreamingClass; propName: string; value: Integer); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: Integer read fVal write fVal;
      property Backup: Integer read fBackup write fBackup;
    end;

  TChangeFloatCommand=class(TChangePropertyCommand)
    private
      fVal,fBackup: Real;
    public
      constructor Create(acomponent: TStreamingClass; propName: string; value: Real); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: Real read fVal write fVal;
      property Backup: Real read fBackup write fBackup;
    end;


  TChangeBoolCommand=class(TChangePropertyCommand)
    private
      fVal: boolean;
    public
      constructor Create(aComponent: TStreamingClass; propName: string; value: boolean); reintroduce;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: boolean read fVal write fVal;
    end;

  TChangeStringCommand=class(TChangePropertyCommand)
    private
      fstring: string;
      fbackup: string;
    public
      constructor Create(aComponent: TStreamingClass; propName: string; value: string); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property val: string read fstring write fstring;
      property backup: string read fbackup write fbackup;
    end;


  TCommandTree=class(TAbstractCommandContainer) //дерево для undo/redo с многими ветвями
    private
      fRoot,fCurrent,fIterator: TAbstractTreeCommand;
      procedure RecursiveCompare(t1,t2: TabstractTreeCommand;var same,plus,minus: Integer);
      procedure RecursiveMerge(t1,t2: TAbstractTreeCommand);
      procedure Assimilate(t: TAbstractTreeCommand);
    protected
      function FindExistingCommand(command: TAbstractTreeCommand;position: TAbstractTreeCommand): boolean;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Clear; override;

      function CheckForExistingCommand(command: TAbstractCommand): boolean; override;
      function UndoEnabled: Boolean; override;
      function RedoEnabled: Boolean; override;

      procedure Add(command: TAbstractCommand); override;
      procedure Undo; override;
      procedure Redo; override;
      procedure JumpToBranch(command: TAbstractCommand); override;

      function CurrentExecutedCommand: TAbstractCommand; override;
      function PrevCommand: TAbstractCommand; override;
      function NextCommand: TAbstractCommand; override; //для построения списков undo/redo

      procedure CompareWith(tree: TCommandTree;var same,plus,minus: Integer);
      procedure MergeWith(tree: TCommandTree);
    published
      property Root: TAbstractTreeCommand read fRoot write fRoot;
      property Current: TAbstractTreeCommand read fCurrent write fCurrent;
    end;
  TAbstractToolAction=class;
  TAbstractDocument=class(TStreamingClass) //документ вместе со списком undo/redo
    private
      initial_pos: TAbstractCommand;  //узнать, сместилось ли состояние после сохр.
      new_commands_added: Boolean;  //и добавлены ли новые команды
      fOnDocumentChange: TNotifyEvent;
      fOnLoad: TNotifyEvent;
      fCriticalSection: TCriticalSection;
      fActionList: TActionList;
      procedure SetOnDocumentChange(value: TNotifyEvent);
      procedure SetOnLoad(value: TNotifyEvent);
    protected
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
      procedure Notification(aComponent: TComponent; operation: TOperation); override;
    public
      SaveWithUndo: boolean;
      StatusPanel: TStatusPanel;
      DoneStatusPanel: TStatusPanel;
      FileName: string;
      constructor Create(Aowner: TComponent); override;
      constructor LoadFromFile(aFileName: string); override;
      constructor LoadFromTemporaryFile(aFileName: string);
      procedure AfterConstruction; override;
      destructor Destroy; override;
      procedure Release;

      function NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean; override;

      //для взаимодействия с ActionList
      procedure RegisterActionList(value: TActionList);

      procedure Undo;
      procedure Redo;
      procedure JumpToBranch(Branch: TAbstractCommand);

      function isEmpty: Boolean;
      function Changed: Boolean;
      function DispatchCommand(command: TAbstractCommand): boolean;
      procedure Save;
      procedure Change; virtual;
      procedure DoLoad; virtual;
      function Hash: T4x4LongWordRecord;
      function UndoTree: TCommandTree;
      property onDocumentChange: TNotifyEvent read fOnDocumentChange write SetOnDocumentChange;
      property onLoad: TNotifyEvent read fOnLoad write SetOnLoad;
      property CriticalSection: TCriticalSection read fCriticalSection;

    published
      UndoContainer: TAbstractCommandContainer;
      Tool: TAbstractToolAction;
    end;

  TSavingThread=class(TThread)
    private
      fdoc: TAbstractDocument;
    protected
      procedure Execute; override;
    public
      constructor Create(docToSave: TAbstractDocument);
  end;

  TAbstractDocumentAction=class(TCustomAction)
  protected
    function GetDoc: TAbstractDocument;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    function Update: Boolean; override;
  published
    property Caption;
    property Hint;
    property ImageIndex;
  end;

  TAbstractToolAction=class(TAbstractDocumentAction)
  public
    procedure SetStatusPanel(text: string);
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    function Select: boolean; virtual; abstract;
    procedure Unselect; virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var Msg: TWMKey; var Handled: Boolean); virtual;
  end;

  TAbstractToolActionClass=class of TAbstractToolAction;

implementation

uses SysUtils,StrUtils,IdHashMessageDigest,abstract_document_actions,forms;

var BeginHashEvent: TEvent;

procedure WaitForHashEvent;
begin
  case BeginHashEvent.WaitFor(3000) of
    wrTimeout: raise Exception.Create('DispatchCommand: wait for hash begin timeout');
    wrError: raise Exception.Create('DispatchCommand: wait for hash begin error');
    wrAbandoned: raise Exception.Create('DispatchCommand: Hashing Thread abandoned');
  end;
end;

(*
        TAbstractCommand
                                 *)
constructor TAbstractTreeCommand.Create(AOwner: TComponent);
var t: TTimeStamp;
begin
  inherited Create(AOwner);
  //у любой уважающей себя команды должно быть имя
  //закодируем в него время и дату создания компоненты
  t:=DateTimeToTimeStamp(Now);
  //дата и время до мс еще не гарантируют уникальность - много команд может возн.
  //одновременно
  ensureCorrectName('c'+IntToHex(t.Date,8)+IntToHex(t.Time,8),AOwner);
  Clear;
end;

procedure TAbstractTreeCommand.Clear;
begin
  Next:=nil;
  Prev:=nil;
  Branch:=nil;
  ActiveBranch:=false;
  fImageIndex:=-1;
end;


function TAbstractTreeCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var t: TAbstractTreeCommand;
    buName: string;
    buNext,buPrev,buBranch: TAbstractTreeCommand;
    buActiveBranch,buTurnLeft: boolean;
    buHash: T4x4LongWordRecord;
    bin1,bin2: TMemoryStream;
    tmpName: string;
    ComponentWithSameName: TComponent;
begin
  if ClassType=what.ClassType then begin
    t:=what as TAbstractTreeCommand;
    //нынче команды очень сложные пошли, завязанные на документ
    //выдернуть их из документа - не поймут, что происходит
    //придется действовать аккуратно...
    //сначала записываем наш родной компонент
    bin1:=TMemoryStream.Create;
    bin1.WriteComponent(self);
    bin1.Seek(0,soFromBeginning);
//    self.saveFormat:=fCyr;
//    self.SaveToFile('wtf1.txt');
    //теперь можно поменять имя второй команде, если там не существует компонента с таким же
    //именем
    tmpName:=Name;
    if Assigned(t.Owner) and (Name<>t.Name) and Assigned(t.Owner.FindComponent(Name)) then begin
      //небольшая чехарда
      ComponentWithSameName:=t.Owner.FindComponent(Name);
      ComponentWithSameName.Name:='';
      //возможно, что это были мы...
    end
    else ComponentWithSameName:=nil;
    //можем безболезненно провести сравнение "на месте"
    buName:=t.Name;
    buNext:=t.next;
    buPrev:=t.prev;
    buBranch:=t.Branch;
    buActiveBranch:=t.ActiveBranch;
    buTurnLeft:=t.TurnLeft;
    t.Name:=tmpName;  //очень вероятно, что мы переименовали себя чтобы избежать конфликта
    t.Next:=Next;
    t.Prev:=Prev;
    t.Branch:=Branch;
    t.ActiveBranch:=ActiveBranch;
    t.TurnLeft:=TurnLeft;
    if t is THashedCommand then begin
      buHash:=THashedCommand(t).fHash;
      THashedCommand(t).fHash:=(self as THashedCommand).fHash;
    end;

    bin2:=TMemoryStream.Create;
    bin2.WriteComponent(t);
    bin2.Seek(0,soFromBeginning);
    if bin1.Size<>bin2.Size then Result:=false
    else Result:=Comparemem(bin1.Memory,bin2.Memory,bin1.Size);
//    if Result=false then begin
//      what.saveFormat:=fCyr;
//      what.SaveToFile('wtf2.txt');
//    end;
    bin1.Free;
    bin2.Free;

    t.Name:=buName;
    t.Next:=buNext;
    t.Prev:=buPrev;
    t.Branch:=buBranch;
    t.ActiveBranch:=buActiveBranch;
    t.TurnLeft:=buTurnLeft;

    if Assigned(ComponentWithSameName) then
      ComponentWithSameName.Name:=tmpName;
    if t is THashedCommand then
      THashedCommand(t).fHash:=buHash;
  end
  else Result:=false;
end;


function TAbstractTreeCommand.NameToDateTime: TDateTime;
var t: TTimeStamp;
begin
  t.Date:=StrToInt('0x'+midstr(Name,2,8));
  t.Time:=StrToInt('0x'+MidStr(Name,10,8));
  Result:=TimeStampToDateTime(t);
end;

function TAbstractTreeCommand.NameToDate(aname: TComponentName): Integer;
begin
  Result:=StrToInt('0x'+midstr(aName,2,8));
end;

procedure TAbstractTreeCommand.ResolveMemory;
begin
  //здесь можно отправиться в прошлое/альтернат. вселенную,
  //чтобы узнать необходимую информацию
  //назад в будущее можно не возвращаться, эту процедуру вызовет только CommandTree,
  //он сам потом возвратится в настоящее.
end;

(*
            TInfoCommand
                                          *)
constructor TInfoCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=12;
end;

function TInfoCommand.Execute: Boolean;
begin
  //эта команда-лишь заглушка, чтобы можно было делать сколько угодно ветвлений
  //и чтобы дерево сразу же имело корень
  Result:=true;
  //иначе команда не выполнится и не будет добавлена в список
end;

function TInfoCommand.Undo: Boolean;
begin
  Result:=true;
  //сегодня я добрый
end;

function TInfoCommand.SmartDateTimeToStr: string;
var last: TAbstractTreeCommand;
begin
  last:=prev;
  while Assigned(last) and not (last is TInfoCommand) do last:=last.Prev;
  if Assigned(last) and (NameToDate(last.Name)=NameToDate(Name)) then
    Result:=TimeToStr(NameToDateTime)
  else
    Result:=DateTimeToStr(NameToDateTime);
end;

(*
            TBranchCommand
                                          *)
function TBranchCommand.caption: string;
resourcestring
  BranchCommandMsg = 'Ветвь создана %s';
begin
  Result:=Format(BranchCommandMsg,[SmartDateTimeToStr]);
end;

(*
          TSavedAsInfoCommand
                                        *)
constructor TSavedAsInfoCommand.Create(aFileName: string);
begin
  Create(nil);
  fFileName:=aFileName;
end;

function TSavedAsInfoCommand.caption: string;
var last: TAbstractTreeCommand;
resourcestring
  SavedAsInfoCommandSavedMsg='Сохранен %s';
  SavedAsInfoCommandSavedAsMsg='Сохранен как %s %s';
begin
  last:=prev;
  while Assigned(last) and not (last is TSavedAsInfoCommand) do last:=last.Prev;
  if Assigned(last) and (TSavedAsInfoCommand(last).FileName=FileName) then
    Result:=Format(SavedAsInfoCommandSavedMsg,[SmartDateTimeToStr])
  else
    Result:=Format(SavedAsInfoCommandSavedAsMsg,[fFileName,SmartDateTimeToStr]);
end;

(*
            THashedCommand
                                          *)
constructor THashedCommand.Create(owner: TComponent);
var i: Integer;
begin
  inherited Create(owner);
  for i:=0 to 3 do fHash[i]:=0;
end;

procedure THashedCommand.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Hash',ReadHash,WriteHash,HashNotEmpty);
end;

function THashedCommand.HashNotEmpty: boolean;
begin
  Result:=(fHash[0]<>0) and (fHash[1]<>0) and (fHash[2]<>0) and (fHash[3]<>0);
end;

procedure THashedCommand.WriteHash(stream: TStream);
begin
  stream.Write(fHash[0],SizeOf(fHash));
end;

procedure THashedCommand.ReadHash(stream: TStream);
begin
  stream.Read(fHash[0],SizeOf(fHash));
end;

function THashedCommand.HashIsEqual(value: T4x4LongWordRecord): boolean;
var i: Integer;
begin
  Result:=true;
  for i:=0 to 3 do
    if value[i]<>fHash[i] then Result:=false;
end;

(*
            THashingThread
                                          *)
constructor THashingThread.Create(command: THashedCommand;isUndo: Boolean);
begin
  inherited Create(true);
  priority:=tpIdle;
  fcommand:=command;
  fIsUndo:=isUndo;
  FreeOnTerminate:=true;
  Resume;
end;

procedure THashingThread.ShowError;
begin
  application.MessageBox(PAnsiChar(fErrorString),'HashedCommand');
end;

procedure THashingThread.Execute;
var tmpHash: T4x4LongWordRecord;
begin
  (fcommand.FindOwner as TAbstractDocument).fCriticalSection.Acquire;
  BeginHashEvent.SetEvent;
  tmpHash:=(fcommand.FindOwner as TAbstractDocument).Hash;
  if (fcommand.HashNotEmpty) and not (fcommand.HashIsEqual(tmpHash)) then begin
    if fIsUndo then fErrorString:='отмене' else fErrorString:='повторе';
    fErrorString:='Несовпадение хэша при '+fErrorString+' команды '+fcommand.Name+' ('+fcommand.ClassName+')';
  end
  else
    fcommand.fHash:=tmpHash;
  (fcommand.FindOwner as TAbstractDocument).fCriticalSection.Release;
  if fErrorString<>'' then Synchronize(ShowError);
end;

(*
              TChangePropertyCommand
                                        *)
constructor TChangePropertyCommand.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  fImageIndex:=13;
end;

function TChangePropertyCommand.NewGetPropInfo: PPropInfo;
begin
  Result:=GetPropInfo(fComponent,fPropName);
  if Result=nil then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s not exist',[fPropName]);
  if Result.SetProc=nil then Raise Exception.CreateFmt('ChangeIntegerCommand: write to read-only property %s',[fPropName]);
end;

procedure TChangePropertyCommand.ResolveMemory;
begin
  (Owner as TCommandTree).JumpToBranch(self); //попадаем на состояние документа
  //после выполнения нашей команды
  (Owner as TCommandTree).Undo;
  fComponentNameStr:=GetComponentValue(fComponent,fComponent.FindOwner);
end;

function TChangePropertyCommand.Execute: Boolean;
begin
  fComponentNameStr:=GetComponentValue(fComponent,fComponent.FindOwner);
  Result:=true;
end;

(*
              TChangeIntegerCommand
                                        *)

constructor TChangeIntegerCommand.Create(aComponent: TStreamingClass;propName: string; value: Integer);
begin
  inherited Create(nil);
  fComponent:=aComponent;
  fPropName:=propName;
  fVal:=value;
end;

function TChangeIntegerCommand.Execute: boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkInteger then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s is not integer',[fPropName]);
  fBackup:=GetOrdProp(fComponent,propInfo);
  if fBackup=fVal then result:=false
  else begin
    SetOrdProp(fComponent,propInfo,fVal);
    Result:=true;
  end;
end;

function TChangeIntegerCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkInteger then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s is not integer',[fPropName]);
  SetOrdProp(fComponent,propInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeIntegerCommand.Caption: string;
var IntToIdent: TIntToIdent;
    propInfo: PPropInfo;
    s: string;
begin
  propInfo:=NewGetPropInfo;
  IntToIdent:=FindIntToIdent(propInfo.PropType^);
  if Assigned(IntToIdent) then
    IntToIdent(fVal,s)
  else
    s:=IntToStr(fVal);
  Result:=fComponentNameStr+'.'+fPropName+'='+s;
end;

(*
          TChangeStringCommand
                                      *)
constructor TChangeStringCommand.Create(aComponent: TStreamingClass;PropName: string; value: String);
begin
  inherited Create(nil);
  fcomponent:=acomponent;
  fPropName:=PropName;
  fString:=value;
end;

function TChangeStringCommand.Execute: Boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  propInfo:=NewGetPropInfo;
  if not (propInfo.PropType^.Kind in [tkString,tkLString,tkWString]) then Raise Exception.CreateFmt('ChangeStringCommand: property %s is not string',[fPropName]);
  fBackup:=GetStrProp(fComponent,propInfo);
  if fBackup=fstring then Result:=false
  else begin
    SetStrProp(fComponent,propInfo,fstring);
    Result:=true;
  end;
end;

function TChangeStringCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if not (propInfo.PropType^.Kind in [tkString,tkLString,tkWString]) then Raise Exception.CreateFmt('ChangeStringCommand: property %s is not string',[fPropName]);
  SetStrProp(fComponent,propInfo,fBackup);
  fBackup:='';
  Result:=true;
end;

function TChangeStringCommand.Caption: string;
begin
  Result:=fComponentNameStr+'.'+fPropName+'='+fstring;
end;

(*
          TChangeFloatCommand
                                      *)
constructor TChangeFloatCommand.Create(acomponent: TStreamingClass;PropName: string;value: Real);
begin
  inherited Create(nil);
  fcomponent:=acomponent;
  fPropName:=PropName;
  fVal:=value;
end;

function TChangeFloatCommand.Execute: Boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkFloat then Raise Exception.CreateFmt('ChangeFloatCommand: property %s is not float',[fPropName]);
  fBackup:=GetFloatProp(fComponent,propInfo);
  if fBackup=fVal then Result:=false //совпадение чисел с плав. точкой - очень редко,
  // но если не совпадает, то с чистой совестью меняем
  else begin
    SetFloatProp(fComponent,propInfo,fval);
    Result:=true;
  end;
end;

function TChangeFloatCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkFloat then Raise Exception.CreateFmt('ChangeFloatCommand: property %s is not float',[fPropName]);
  SetFloatProp(fComponent,propInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeFloatCommand.Caption: string;
begin
  Result:=fComponentNameStr+'.'+fPropName+'='+FloatToStr(fVal);
end;



(*
            TChangeBoolCommand
                                      *)
constructor TChangeBoolCommand.Create(aComponent: TStreamingClass;PropName: string; value: Boolean);
begin
  inherited Create(nil);
  fComponent:=aComponent;
  fPropName:=PropName;
  fVal:=value;
end;

function TChangeBoolCommand.Execute: boolean;
var PropInfo: PPropInfo;
  res: LongInt;
begin
  inherited Execute;
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.CreateFmt('ChangeBoolCommand.Execute: property %s is not boolean', [fPropName] );
  res:=GetOrdProp(fComponent,PropInfo);
  if fVal=Boolean(res) then result:=false
  else begin
    SetOrdProp(fComponent,PropInfo,Integer(fVal));
    Result:=true;
  end;
end;

function TChangeBoolCommand.Undo: boolean;
var PropInfo: PPropInfo;
begin
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.CreateFmt('ChangeBoolCommand.Execute: property %s is not boolean', [fPropName] );
  SetOrdProp(fComponent,PropInfo,Integer(not fVal));
  Result:=true;
end;

function TChangeBoolCommand.Caption: string;
begin
  Result:=fComponentNameStr+'.'+fPropName+'='+BoolToStr(fVal,true);
end;


(*
              TAbstractDocument
                                      *)

constructor TAbstractDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SaveWithUndo:=true;
  fCriticalSection:=TCriticalSection.Create;
end;

constructor TAbstractDocument.LoadFromFile(aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  FileName:=aFileName;
  new_commands_added:=false;
  if (UndoTree<>nil) then initial_pos:=UndoTree.current;
end;

constructor TAbstractDocument.LoadFromTemporaryFile(aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  new_commands_added:=false;
end;

procedure TAbstractDocument.afterConstruction;
var i: Integer;
    buCurrent: TAbstractCommand;
begin
  if UndoTree=nil then begin
    UndoContainer:=TCommandTree.Create(self);
    with UndoContainer as TCommandTree do begin
      Name:='UndoContainer';
      Root:=TBranchCommand.Create(UndoTree);
      Current:=Root;
      Root.ActiveBranch:=true;
    end;
    initial_pos:=UndoTree.current;
  end
  else begin
    buCurrent:=UndoTree.Current;
    for i:=0 to UndoTree.ComponentCount-1 do
      if UndoTree.Components[i] is TAbstractTreeCommand then
        TAbstractTreeCommand(UndoTree.Components[i]).ResolveMemory;
    UndoTree.JumpToBranch(buCurrent);
  end;
end;

procedure TAbstractDocument.Release;
begin
  fCriticalSection.Acquire;
  Free;
end;

destructor TAbstractDocument.Destroy;
begin
  UndoTree.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TAbstractDocument.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
  fList: TStringList;
begin
  fList:=TStringList.Create;
  for i := 0 to ComponentCount-1 do
    if not (csSubComponent in Components[i].ComponentStyle) and (((Components[i]<>UndoTree) and (Components[i]<>Tool))  or SaveWithUndo) then
      fList.AddObject(Components[i].Name,Components[i]);
  fList.Sort;
  //тем самым объекты расположатся в алфавитном порядке а не в порядке создания
  //поскольку разные манипуляции могут изменить порядок и не совпадет хэш
  //нам этого не надо!
  for i:=0 to fList.Count-1 do
    Proc( TComponent(fList.Objects[i]) );
  fList.Free;
end;

function TAbstractDocument.NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean;
var i: integer;
    c: TComponent;
begin
  c:=FindComponent(proposedName);
  Result:=Assigned(c) and (c<>me);
  if not Result then
    for i:=0 to ComponentCount-1 do begin
      if (Components[i] is TStreamingClass) and not (Components[i] is TCommandTree) and not (Components[i] is TAbstractToolAction) then begin
        Result:=Result or TStreamingClass(Components[i]).NameExistsSomewhere(proposedName,me);
        if Result=true then break;
      end;
    end;
end;

procedure TAbstractDocument.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fActionList) then
    fActionList:=nil;
end;

function TAbstractDocument.isEmpty: Boolean;
begin
  Result:=(UndoTree.Root.Next=nil);
end;

function TAbstractDocument.Changed: Boolean;
begin
  Result:=(UndoTree.current<>initial_pos) or new_commands_added;
end;

procedure TAbstractDocument.RegisterActionList(value: TActionList);
begin
  fActionList:=value;
  if Assigned(value) then
    value.FreeNotification(self); //если будет удаляться, то пусть мы об этом
    //узнаем и выставим знач. в nil
end;

function TAbstractDocument.DispatchCommand(command: TAbstractCommand): Boolean;
begin
  fCriticalSection.Acquire;
  BeginHashEvent.SetEvent;
  undotree.InsertComponent(command);
  //может быть, не нужно исполнять конкретно эту команду, она уже есть
  //именно когда обе команды еще не исполнены, их можно сравнивать
  if undotree.CheckForExistingCommand(command) then begin
    //состояние уже поменялось, мы выполнили существующую команду
    change;
    //но history не надо перестраивать, лишь указать тек. команду
    if Assigned(fActionList) and (fActionList is TAbstractDocumentActionList) then
      TAbstractDocumentActionList(fActionList).RefreshHistoryHighlights;
    command.Free;
    Result:=false;
  end
  else
    if command.Execute then begin
      undotree.RemoveComponent(command);
      UndoTree.Add(command);
      Change;
      if Assigned(fActionList) and (fActionList is TAbstractDocumentActionList) then
        TAbstractDocumentActionList(fActionList).ChangeHistory;
      new_commands_added:=true;
      if (command is THashedCommand) then begin
        BeginHashEvent.ResetEvent;
        THashingThread.Create(THashedCommand(command),false);
      end;
      Result:=true;
    end
    else begin
      command.Free;
      Result:=false;
    end;
  fCriticalSection.Leave;
  WaitForHashEvent;
end;

procedure TAbstractDocument.Save;
begin
  Assert(FileName<>'','WTF: empty filename');
  TSavingThread.Create(self);
//  self.SaveToFile(FileName);
  initial_pos:=UndoTree.current;
  new_commands_added:=false;
end;

procedure TAbstractDocument.Undo;
begin
  if UndoTree.UndoEnabled then begin
//    fCriticalSection.Acquire;
      UndoTree.Undo;
//    fCriticalSection.Leave;
    Change;
    if Assigned(fActionList) and (fActionList is TAbstractDocumentActionList) then
      TAbstractDocumentActionList(fActionList).RefreshHistoryHighlights;
  end;
end;

procedure TAbstractDocument.Redo;
begin
  if UndoTree.RedoEnabled then begin
//    fCriticalSection.Acquire;
      UndoTree.Redo;
//    fCriticalSection.Leave;
    Change;
    if Assigned(fActionList) and (fActionList is TAbstractDocumentActionList) then
      TAbstractDocumentActionList(fActionList).RefreshHistoryHighlights;
  end;
end;

procedure TAbstractDocument.JumpToBranch(Branch: TAbstractCommand);
begin
//  fCriticalSection.Acquire;
    UndoTree.JumpToBranch(Branch);
//  fCriticalSection.Leave;
  Change;
  if Assigned(fActionList) and (fActionList is TAbstractDocumentActionList) then
    TAbstractDocumentActionList(fActionList).RefreshHistoryHighlights;
end;

procedure TAbstractDocument.Change;
begin
  if Assigned(onDocumentChange) then onDocumentChange(self);
end;

procedure TAbstractDocument.DoLoad;
begin
  if Assigned(onLoad) then onLoad(self);
end;

function TAbstractDocument.UndoTree: TCommandTree;
begin
  Result:=UndoContainer as TCommandTree;
end;

function TAbstractDocument.Hash: T4x4LongWordRecord;
var buSaveWithUndo: boolean;
    str: TMemoryStream;
begin
//  fCriticalSection.Acquire;
  buSaveWithUndo:=SaveWithUndo; //потом вернем
  SaveWithUndo:=false;  //чтобы найти хэш
  str:=TMemoryStream.Create;
  str.WriteComponent(self);
  str.Seek(0,soFromBeginning);
  with TIdHashMessageDigest5.Create do begin
    try
    Result:=HashValue(str);
    finally
    Free;
    end;
  end;
  str.Free;
//  self.SaveToFile(TIDHash128.AsHex(Result)+'.txt');
  SaveWithUndo:=buSaveWithUndo;
//  fCriticalSection.Leave;
end;

procedure TAbstractDocument.SetOnDocumentChange(value: TNotifyEvent);
begin
  fOnDocumentChange:=value;
  Change;
end;

procedure TAbstractDocument.SetOnLoad(value: TNotifyEvent);
begin
  fOnLoad:=value;
  DoLoad;
end;




(*
      TCommandTree
                      *)
constructor TCommandTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCommandTree.Clear;
begin
  fRoot:=nil;
  fCurrent:=nil;
end;

function TCommandTree.FindExistingCommand(command: TAbstractTreeCommand;position: TAbstractTreeCommand): boolean;
begin
  if position=nil then Result:=false
  else if position.EqualsByAnyOtherName(command) then begin
    JumpToBranch(position);
    Result:=true;
  end
  else if (position is TInfoCommand) then
    Result:=FindExistingCommand(command,position.Next) or FindExistingCommand(command,position.Branch)
  else Result:=false;
end;

function TCommandTree.CheckForExistingCommand(command: TAbstractCommand): boolean;
begin
//может, код действительно станет более читаемым, если здоровенный, здоровенный
//add разделить на 2 части
//поиск в глубину, сначала идем прямо по курсу
if current is TInfoCommand then
  Result:=FindExistingCommand(command as TAbstractTreeCommand,current)
else
  Result:=FindExistingCommand(command as TAbstractTreeCommand,current.Next) or FindExistingCommand(command as TAbstractTreeCommand,current.Branch);
end;

procedure TCommandTree.Add(command: TAbstractCommand);
var treecom: TAbstractTreeCommand;
begin
  while (current.Next<>nil) and (current.Next is TInfoCommand) do begin
    current.ActiveBranch:=true;
    current:=current.Next;
  end;
  if (current.Next<>nil) then begin
    //придется отпочковать целую ветвь
    //убедимся, что прокрутили все уже отпочкованные ветви
    //чаще всего этот цикл не будет выполняться ни разу
    current.ActiveBranch:=true;
    current.TurnLeft:=true;
    while (current.Branch<>nil) do begin
      current.ActiveBranch:=true;
      current.TurnLeft:=true;
      current:=current.Branch;
    end;
    //создаем новую ветвь
    current.Branch:=TBranchCommand.Create(self);
    current.Branch.Prev:=current;
    //делаем ее текущей
    current:=current.Branch;
    current.ActiveBranch:=true;
    current.TurnLeft:=false;
  end;
  //теперь заведомо концевой узел, просто добавляем новый элемент
  treecom:=command as TAbstractTreeCommand;
  treecom.ensureCorrectName(treecom.Name,self);
  insertComponent(treecom);

  current.Next:=treecom;
  treecom.ActiveBranch:=true;
  treecom.TurnLeft:=false;
  treecom.Prev:=current;
  current:=treecom;
end;

procedure TCommandTree.Undo;
begin
  (Owner as TAbstractDocument).fCriticalSection.Acquire;
  //проверка UndoEnabled гарантирует, что вызов undo будет произведен
  //когда его можно сделать
  if current.Undo then begin
    current.ActiveBranch:=false;
    current:=current.Prev;
    while (current is TInfoCommand) and (current.prev<>nil) do begin
      current.ActiveBranch:=false;
      current:=current.Prev;
    end;
    if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),true)
    else BeginHashEvent.SetEvent;
  end
  else Raise Exception.Create('Undo command failed');
  (Owner as TAbstractDocument).fCriticalSection.Leave;
  WaitForHashEvent;
end;

procedure TCommandTree.Redo;
begin
  (owner as TAbstractDocument).fCriticalSection.Acquire;
  //проверка RedoEnabled гарантирует, что вызов undo будет произведен
  //когда его можно сделать
  repeat
    if current.TurnLeft then current:=current.Branch
    else current:=current.Next;
    current.ActiveBranch:=true;
  until (not (current is TInfoCommand));
  if not current.Execute then Raise Exception.Create('Redo command failed');
  if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),false)
  else BeginHashEvent.SetEvent;
  (owner as TAbstractDocument).fCriticalSection.Leave;

  WaitForHashEvent;
end;

procedure TCommandTree.JumpToBranch(command: TAbstractCommand);
var b: TAbstractTreeCommand;
begin
  (owner as TAbstractDocument).fCriticalSection.Acquire;
  //самая веселая команда
  //нужно перейти на произвольное состояние
  //первым делом, надо проторить путь от command до активного пути
  if command=nil then command:=root;
  b:=command as TAbstractTreeCommand;
  while not b.ActiveBranch do begin
    b.ActiveBranch:=true;
    b.Prev.TurnLeft:=(b.Prev.Branch=b);
    b:=b.Prev;
  end;
  //теперь b - это точка ветвления
  //если command стояла выше по активной ветви, чем current, то b=current
  //пойдем от текущего положения до b, по пути отменяя все операции
  while current<>b do begin
    if not current.Undo then Raise Exception.Create('Undo command failed');
//    if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),true);
    current.ActiveBranch:=false;
    current:=current.Prev;
    while (current is TInfoCommand) and (current<>b) do begin
      current.ActiveBranch:=false;
      current:=current.Prev;
    end;
  end;
  //все, сомкнулись. Теперь дотопаем от b до command
  while current<>command do begin
    while current.TurnLeft and (current<>command) do current:=current.Branch;
    if current<>command then begin
      current:=current.Next;
      if not current.Execute then Exception.Create('Redo command failed');
    end;
  end;

  (owner as TAbstractDocument).fCriticalSection.Leave;
end;

function TCommandTree.UndoEnabled: Boolean;
var t: TAbstractTreeCommand;
begin
  t:=current;
  while (t is TInfoCommand) and (t.Prev<>nil) do t:=t.Prev;
  Result:=(t.Prev<>nil);
end;

function TCommandTree.RedoEnabled: Boolean;
var t: TAbstractTreeCommand;
begin
  t:=current;
  repeat
    if t.TurnLeft then t:=t.Branch
    else t:=t.Next;
  until (t=nil) or (not (t is TInfoCommand));
  Result:=Assigned(t);
end;

function TCommandTree.CurrentExecutedCommand: TAbstractCommand;
begin
  //здесь мы игнорируем инфокоманды и инициализируем итератор
  fIterator:=current;
  while Assigned(fIterator) and (fIterator is TInfoCommand) do fIterator:=fIterator.Prev;
  Result:=fIterator;
  if fIterator=nil then fIterator:=root;
end;

function TCommandTree.PrevCommand: TAbstractCommand;
begin
  if Assigned(fIterator) then begin
    fIterator:=fIterator.Prev;
    while Assigned(fIterator) and (fIterator is TInfoCommand) do fIterator:=fIterator.Prev;
    Result:=fIterator;
  end
  else Result:=nil;
end;

function TCommandTree.NextCommand: TAbstractCommand;
begin
  if Assigned(fIterator) then begin
    repeat
      if fIterator.TurnLeft then fIterator:=fIterator.Branch
      else fIterator:=fIterator.Next;
    until (fIterator=nil) or (not (fIterator is TInfoCommand));
    Result:=fIterator;
  end
  else Result:=nil;
end;

procedure TCommandTree.RecursiveCompare(t1,t2: TAbstractTreeCommand;var same,plus,minus: Integer);
begin
  if t1=nil then begin
    if t2<>nil then begin
      //у второго дерева оказалась ветвь, которой нет у нас
      inc(plus);
      RecursiveCompare(nil,t2.Next,same,plus,minus);
      RecursiveCompare(nil,t2.Branch,same,plus,minus);
    end
  end
  else begin
    if t2=nil then begin
      //у нас есть ветвь, которой нет у второго дерева
      inc(minus);
      RecursiveCompare(t1.Next,nil,same,plus,minus);
      RecursiveCompare(t1.Branch,nil,same,plus,minus);
    end
    else begin
      //ветвь есть у обеих, но одинаковы ли команды?
      if t1.EqualsByAnyOtherName(t2) then begin
        inc(same);
        RecursiveCompare(t1.Next,t2.Next,same,plus,minus);
        RecursiveCompare(t1.Branch,t2.Branch,same,plus,minus);
      end
      else begin
        inc(plus);
        inc(minus);
        RecursiveCompare(t1.Next,nil,same,plus,minus);
        RecursiveCompare(t1.Branch,nil,same,plus,minus);
        RecursiveCompare(nil,t2.Next,same,plus,minus);
        RecursiveCompare(nil,t2.Branch,same,plus,minus);
      end;
    end;
  end;
end;

procedure TCommandTree.CompareWith(tree: TCommandTree;var same,plus,minus: Integer);
var backup1,backup2: TAbstractCommand;
begin
  backup1:=current;
  backup2:=tree.Current;
  //чтобы все команды вернулись в исходное (невыполненное) состояние
  JumpToBranch(root);
  tree.JumpToBranch(tree.Root);
  //начальная установка, корень за совпадение не считаем
  same:=0;
  plus:=0;
  minus:=0;
  RecursiveCompare(root.Next,tree.Root.Next,same,plus,minus);
  RecursiveCompare(root.Branch,tree.Root.Branch,same,plus,minus);
  JumpToBranch(backup1);
  tree.JumpToBranch(backup2);
end;

procedure TCommandTree.MergeWith(tree: TCommandTree);
var backup1: TAbstractCommand;
begin
  //добавляем ветви, которых у нас не было.
  //второе дерево в сущности можно "пустить на мясо",
  //то есть выкусывам из него ветви и присоединяем к нам.
  backup1:=current;
  JumpToBranch(root);
  tree.JumpToBranch(tree.root);
  RecursiveMerge(root,tree.Root);
  JumpToBranch(backup1);
  //а дереву 2 незачем прыгать, оно скоро уничтожится
end;

procedure TCommandTree.RecursiveMerge(t1,t2: TAbstractTreeCommand);
var iterator: TAbstractTreeCommand;
begin
  if t1.Next=nil then begin
    if t2.Next<>nil then begin
      //перецепляем
      //повтора не будет, ведь t1 вообще концевой
      t1.Next:=t2.Next;
      t1.Next.Prev:=t1;
      //меняем владельца
      Assimilate(t2.Next);
      //осталось посмотреть, может еще и ветвь есть?
      if t2.Branch<>nil then begin
        //t1 был концевой, повторов быть не может
        t1.Branch:=t2.Branch;
        t1.Branch.Prev:=t1;
        assimilate(t2.Branch);
      end;
    end;
  end
  else if t2.Next<>nil then begin
    if t1.Next.EqualsByAnyOtherName(t2.Next) then RecursiveMerge(t1.Next,t2.Next)
    else begin
      //раз не совпадают, нужно эту "неведомую" ветвь прицепить сбоку
      //но не будем торопиться, сначала проверим branch
      iterator:=t1;
      while iterator.Branch<>nil do iterator:=iterator.Branch;
      iterator.Branch:=TBranchCommand.Create(self);
      iterator.Branch.Prev:=iterator;
      iterator:=iterator.Branch;
      iterator.Next:=t2.Next;
      iterator.Next.Prev:=iterator;
      //перецепили одну команду, теперь нужно перетянуть к себе все хвосты
      assimilate(t2.Next);
    end;
    //и еще посмотрим, может и ветвь есть?
    //если у t2 нет ветвей, то и незачем париться
    if t2.Branch<>nil then begin
      if (t1.Branch<>nil) and (t1.Branch.EqualsByAnyOtherName(t2.Branch)) then RecursiveMerge(t1.Branch,t2.Branch)
      else begin
        iterator:=t1;
        while iterator.Branch<>nil do iterator:=iterator.Branch;
        iterator.Branch:=t2.Branch;
        iterator.Branch.Prev:=iterator;
        Assimilate(t2.Branch);
      end;
    end;
  end;
end;

procedure TCommandTree.Assimilate(t: TAbstractTreeCommand);
begin
  t.Owner.RemoveComponent(t);
  InsertComponent(t);
  if Assigned(t.Next) then Assimilate(t.Next);
  if Assigned(t.Branch) then Assimilate(t.Branch);
end;

(*
      TSavingThread
                        *)
constructor TSavingThread.Create(docToSave: TAbstractDocument);
begin
  inherited Create(true);
  fdoc:=docToSave;
  FreeOnTerminate:=true;
  Resume;
end;

procedure TSavingThread.Execute;
begin
  fdoc.fCriticalSection.Acquire;
    fdoc.SaveToFile(fdoc.FileName);
  fdoc.fCriticalSection.Leave;
end;


(*
        TAbstractDocumentAction
                                      *)
function TAbstractDocumentAction.GetDoc: TAbstractDocument;
begin
  if (ActionList is TAbstractDocumentActionList) and Assigned(TAbstractDocumentActionList(ActionList).doc) then
    Result:=TAbstractDocumentActionList(ActionList).doc^
  else
    Result:=nil;
  //получается, что результат nil в след. случаях:
  //- действие принадлежит неправильному ActionList'у (не имеющему свойства doc)
  //- doc=nil, т.е. actionList ни на что не ссылается
  //- doc^=nil, т.е actionList ссылается на переменную, которая ссылается на nil
end;

function TAbstractDocumentAction.HandlesTarget(Target: TObject): Boolean;
begin
//выполнима ли команда или пора ее сразу отключить, от греха подальше
  Result:=Assigned(Target) and (Target is TAbstractDocument);
//классы-потомки могут начинать свою проверку с inherited HandlesTarget(Target) and ...
//по принципу short cut, если даже это ложно, дальше он не полезет.
end;

function TAbstractDocumentAction.Update: boolean;
var doc: TabstractDocument;
begin
//это место будет вызываться когда не лень, чтобы выяснить, не надо ль
//отключить элем. управления или еще что-нибудь в этом духе
  doc:=getDoc;
  Enabled:=Assigned(doc);
  Result:=true; //то есть мы выяснили все что хотели и дальше бегать не надо
//если отсутствует документ вообще, тогда разумеется и действие отключаем
end;

(*
      TAbstractToolAction
                                *)
procedure TAbstractToolAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
  ToolClass: TAbstractToolActionClass;
begin
  doc:=Target as TAbstractDocument;
  //слишком много переливаний из порожнего в пустое
  //надо все-таки разграничить классы - один отвечает за красивые кнопочки
  //и их вкл/выкл вовремя
  //а другой - за работу инструмента и сохранение данных в документе
  //а пока оставим один класс, но упростим процедуры
  ToolClass:=TAbstractToolActionClass(self.classType);
  if Assigned(doc.Tool) then begin
    if not (doc.Tool is ToolClass) then begin
      doc.Tool.Unselect;
      doc.Tool.Free;
      doc.Tool:=ToolClass.Create(doc);
      doc.Tool.Name:='Tool';
    end;
  end
  else begin
    doc.Tool:=ToolClass.Create(doc);
    doc.Tool.Name:='Tool';
  end;
  doc.Tool.Select;
end;

destructor TAbstractToolAction.Destroy;
begin
  inherited Destroy;
end;

procedure TAbstractToolAction.SetStatusPanel(text: string);
var data: TAbstractDocument;
begin
  if (owner<>nil) then begin
    data:=owner as TAbstractDocument;
    if data.StatusPanel<>nil then
      data.StatusPanel.Text:=text;
  end;
end;

procedure TAbstractToolAction.KeyDown(var Msg: TWMKey; var Handled: Boolean);
begin
  //далеко не всегда нужно реагировать на клавиши, поэтому сделаем "пустую" реализацию, чтобы
  //не ругалась чуть что
end;

procedure TAbstractToolAction.MouseMove(Shift: TShiftState; X,Y: Integer);
begin

end;

procedure TAbstractToolAction.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin

end;

initialization
RegisterClasses([TCommandTree,TBranchCommand,TInfoCommand,TSavedAsInfoCommand,
TChangeIntegerCommand,TChangeBoolCommand,TChangeFloatCommand,TChangeStringCommand]);
BeginHashEvent:=TEvent.Create(nil,false,false,'AbstractDocumentBeginHash');

finalization
BeginHashEvent.Free;

end.
