unit command_class_lib;

interface

uses streaming_class_lib,classes,TypInfo,IdHash,SyncObjs,actnlist,controls,comctrls,messages;

type
  TAbstractCommand=class(TStreamingClass)  //чтобы историю изменений можно было хранить вместе со всем остальным
    private
      fNext,fPrev,fBranch: TAbstractCommand;
      fTurnLeft: Boolean;
    protected
      fActiveBranch: Boolean;
      fImageIndex: Integer;
    public
      constructor Create(Aowner: TComponent); override;
      procedure Clear; override;
      function Execute: Boolean; virtual; abstract;
      function Undo: boolean; virtual; abstract;
      function caption: string; virtual;
      procedure ResolveMemory; virtual;
      function NameToDateTime: TDateTime;
      function NameToDate(aName: TComponentName): Integer;
      property ImageIndex: Integer read fImageIndex;
      function EqualsByAnyOtherName(what: TStreamingClass): boolean; override;
    published
      property Prev: TAbstractCommand read fPrev write fPrev;
      property Next: TAbstractCommand read fNext write fNext;
      property Branch: TAbstractCommand read fBranch write fBranch;
      property ActiveBranch: Boolean read fActiveBranch write fActiveBranch default false;
      property TurnLeft: Boolean read fTurnLeft write fTurnLeft default false;
    end;

  TInfoCommand=class(TAbstractCommand)
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

  THashedCommand=class(TAbstractCommand)
    private
      fHash: T4x4LongWordRecord;
      procedure WriteHash(stream: TStream);
      procedure ReadHash(stream: TStream);
      function HashNotEmpty: boolean;
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(owner: TComponent); override;
      function Execute: boolean; override;
      function Undo: boolean; override;
      property Hash: T4x4LongWordRecord read fHash;
    end;

  TChangePropertiesCommand=class(TAbstractCommand)
    protected
      instance: TPersistent;
      fPropInfo: PPropInfo;
      procedure _getPropInfo(propPath: string);
  end;

  TChangeFloatProperty=class(TChangePropertiesCommand)
    private
      fPropPath: string;
      fBackUp,fVal: Real;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
      procedure ReadBackup(reader: TReader);
      procedure WriteBackup(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: Real);reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: boolean; override;
      function caption: string; override;
  end;
//  TChIntCaptionFormat=(cfDec,cfHex);
  TChangeIntegerProperty=class(TChangePropertiesCommand)
    private
      fPropPath: string;
      fBackUp,fVal: Integer;
//      fCaptionFormat: TChIntCaptionFormat;
      fCaption: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
      procedure ReadBackup(reader: TReader);
      procedure WriteBackup(writer: TWriter);
      procedure ReadCaption(reader: TReader);
      procedure WriteCaption(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: Integer; aCaption: string=''); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;


  TChangeBoolProperty=class(TChangePropertiesCommand)
    private
      fPropPath: string;
      fVal: Boolean;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string;value: Boolean); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;

  TChangeStringProperty=class(TChangePropertiesCommand)
    private
      fPropPath: string;
      fVal: string;
      fBackUp: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: string); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;

  TChangeEnumProperty=class(TChangePropertiesCommand)
    private
      fPropPath:string;
      fValName: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValName(reader: TReader);
      procedure WriteValName(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; valName: string); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;


  TCommandList=class(TStreamingClass) //список для undo/redo и даже для сохранения данных в файл
    private
      fRoot: TComponent;
      fcount: Integer;
      fcurrent: Integer; //наше данное положение - куда добавлять команду. Т.е по умолчанию - 0

      procedure ReadCount(reader: TReader);
      procedure WriteCount(writer: TWriter);
      procedure ReadCurrent(reader: TReader);
      procedure WriteCurrent(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create(Aowner: TComponent); override;


      procedure Add(command: TAbstractCommand);
      procedure Undo;
      procedure Redo;
      function UndoEnabled: Boolean;
      function RedoEnabled: Boolean;
      destructor Destroy; override;
      procedure Clear; override;
      property count: Integer read fcount;
      property current: Integer read fcurrent;
    end;

  TCommandTree=class(TStreamingClass) //дерево для undo/redo с многими ветвями
    private
      fRoot: TAbstractCommand;
      fCurrent: TAbstractCommand;
      procedure RecursiveCompare(t1,t2: TabstractCOmmand;var same,plus,minus: Integer);
      procedure RecursiveMerge(t1,t2: TAbstractCommand);
      procedure Assimilate(t: TAbstractCommand);
    protected
      function FindExistingCommand(command: TAbstractCommand;position: TAbstractCommand): boolean;
    public
      constructor Create(AOwner: TComponent); override;
      function CheckForExistingCommand(command: TAbstractCommand): boolean;
      procedure Add(command: TAbstractCommand);
      procedure Undo;
      procedure Redo;
      procedure JumpToBranch(command: TAbstractCommand);
      function UndoEnabled: Boolean;
      function RedoEnabled: Boolean;
      procedure CompareWith(tree: TCommandTree;var same,plus,minus: Integer);
      procedure MergeWith(tree: TCommandTree);
      procedure Clear; override;
    published
      property Root: TAbstractCommand read fRoot write fRoot;
      property Current: TAbstractCommand read fCurrent write fCurrent;
    end;
  TAbstractToolAction=class;
  TAbstractDocument=class(TStreamingClass) //документ вместе со списком undo/redo
    private
      initial_pos: TAbstractCommand;  //узнать, сместилось ли состояние после сохр.
      new_commands_added: Boolean;  //и добавлены ли новые команды
      fOnDocumentChange: TNotifyEvent;
      fOnLoad: TNotifyEvent;
      fCriticalSection: TCriticalSection;
      procedure SetOnDocumentChange(value: TNotifyEvent);
      procedure SetOnLoad(value: TNotifyEvent);
    protected
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    public
      SaveWithUndo: boolean;
      StatusPanel: TStatusPanel;
      FileName: string;
      constructor Create(Aowner: TComponent); override;
      constructor LoadFromFile(aFileName: string); override;
      procedure AfterConstruction; override;
      destructor Destroy; override;
      procedure Release;

      procedure Undo;
      procedure Redo;
      procedure JumpToBranch(Branch: TAbstractCommand);

      function isEmpty: Boolean;
      function Changed: Boolean;
      procedure DispatchCommand(command: TAbstractCommand);
      procedure Save;
      procedure Change; virtual;
      procedure DoLoad; virtual;
      property onDocumentChange: TNotifyEvent read fOnDocumentChange write SetOnDocumentChange;
      property onLoad: TNotifyEvent read fOnLoad write SetOnLoad;
      function Hash: T4x4LongWordRecord;
    published
      UndoTree: TCommandTree;
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
  end;

  TAbstractToolAction=class(TAbstractDocumentAction)
  public
    procedure SetStatusPanel(text: string);
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Select; virtual; abstract;
    procedure Unselect; virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var Msg: TWMKey; var Handled: Boolean); virtual;
  end;

  TAbstractToolActionClass=class of TAbstractToolAction;

implementation

uses SysUtils,StrUtils,IdHashMessageDigest,abstract_document_actions;

(*
        TAbstractCommand
                                 *)
constructor TAbstractCommand.Create(AOwner: TComponent);
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

procedure TAbstractCommand.Clear;
begin
  Next:=nil;
  Prev:=nil;
  Branch:=nil;
  ActiveBranch:=false;
  fImageIndex:=-1;
end;


function TAbstractCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var our_class: TStreamingClassClass;
    t: TAbstractCommand;
    buName: string;
    buNext,buPrev,buBranch: TAbstractCommand;
    buActiveBranch,buTurnLeft: boolean;

begin
  if ClassType=what.ClassType then begin
    t:=what as TAbstractCommand;
    if Assigned(t.Owner) and (Name<>t.Name) and Assigned(t.Owner.FindComponent(Name)) then begin
      //не получится переименовать объект, не "выдергивая" его с насиженного места
      //придется клонировать
      //ссылки на объекты могут потеряться, тогда нужно писать свой вариант
      //EqualsByAnyOtherName
      our_class:=TStreamingClassClass(ClassType);
      t:=(our_class.Clone(what)) as TAbstractCommand;
      t.Name:=Name;
      t.Next:=Next;
      t.Prev:=Prev;
      t.Branch:=Branch;
      t.ActiveBranch:=ActiveBranch;
      t.TurnLeft:=TurnLeft;
      Result:=IsEqual(t);
      t.Free;
    end
    else begin
      //можем безболезненно провести сравнение "на месте"
      buName:=t.Name;
      buNext:=t.next;
      buPrev:=t.prev;
      buBranch:=t.Branch;
      buActiveBranch:=t.ActiveBranch;
      buTurnLeft:=t.TurnLeft;
      t.Name:=Name;
      t.Next:=Next;
      t.Prev:=Prev;
      t.Branch:=Branch;
      t.ActiveBranch:=ActiveBranch;
      t.TurnLeft:=TurnLeft;
      Result:=IsEqual(t);
      t.Name:=buName;
      t.Next:=buNext;
      t.Prev:=buPrev;
      t.Branch:=buBranch;
      t.ActiveBranch:=buActiveBranch;
      t.TurnLeft:=buTurnLeft;
    end;
  end
  else Result:=false;
end;


function TAbstractCommand.NameToDateTime: TDateTime;
var t: TTimeStamp;
begin
  t.Date:=StrToInt('0x'+midstr(Name,2,8));
  t.Time:=StrToInt('0x'+MidStr(Name,10,8));
  Result:=TimeStampToDateTime(t);
end;

function TAbstractCommand.NameToDate(aname: TComponentName): Integer;
begin
  Result:=StrToInt('0x'+midstr(aName,2,8));
end;

function TAbstractCommand.caption: string;
begin
  result:=self.ClassName;
end;

procedure TAbstractCommand.ResolveMemory;
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
var last: TAbstractCommand;
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
begin
//  Result:='Ветвь создана '+DateTimeToStr(NameToDateTime);
  Result:='Ветвь создана '+SmartDateTimeToStr;
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
var last: TAbstractCommand;
begin
  last:=prev;
  while Assigned(last) and not (last is TSavedAsInfoCommand) do last:=last.Prev;
  if Assigned(last) and (TSavedAsInfoCommand(last).FileName=FileName) then
    Result:='Сохранен '+SmartDateTimeToStr
  else
    Result:='Сохранен как '+fFileName+' '+SmartDateTimeToStr;
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

function THashedCOmmand.Execute: boolean;
begin
  //к этому моменту все необходимые действия над документом уже выполнены
  if not HashNotEmpty then
    fHash:=(FindOwner as TAbstractDocument).Hash;
//  else Result:=
  Result:=true;
end;

function THashedCOmmand.Undo: boolean;
begin
  Result:=true;
end;
(*
            TChangePropertiesCommand
                                          *)

procedure TChangePropertiesCommand._getPropInfo(propPath: string);
begin
  myGetPropInfo(propPath,instance,fPropInfo);
end;


(*
            TChangeFloatCommand
                                          *)
constructor TChangeFloatProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeFloatProperty.Create(aPropPath: string; value: Real);
begin
  Create(nil);
  fPropPath:=aPropPath;
  fVal:=value;
end;


function TChangeFloatProperty.Execute: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetFloatProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetFloatProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeFloatProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  SetFloatProp(instance,fPropInfo,fBackup);
  fBackUp:=0; //чтобы места не занимал
  Result:=true;
end;

function TChangeFloatProperty.caption: string;
begin
  Result:=fPropPath+'='+FloatToStr(fVal);
end;

procedure TChangeFloatProperty.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Path',ReadPath,WritePath,true); //не будем жадничать, путь всегда ненулевой!
  Filer.DefineProperty('value',ReadValue,WriteValue,(fVal<>0));
  Filer.DefineProperty('backup',ReadBackup,WriteBackup,(fBackup<>0));
end;

procedure TChangeFloatProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeFloatProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeFloatProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadFloat;
end;

procedure TChangeFloatProperty.WriteValue(writer: TWriter);
begin
  writer.WriteFloat(fVal);
end;

procedure TChangeFloatProperty.ReadBackup(reader: TReader);
begin
  fBackup:=reader.ReadFloat;
end;

procedure TChangeFloatProperty.WriteBackup(writer: TWriter);
begin
  writer.WriteFloat(fBackup);
end;

(*
              TChangeIntegerProperty
                                        *)
constructor TChangeIntegerProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeIntegerProperty.Create(aPropPath: string; value: Integer; aCaption: string='');
begin
  Create(nil);
  fPropPath:=aPropPath;
  fVal:=value;
  fCaption:=aCaption;
end;


function TChangeIntegerProperty.Execute: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkInteger then Raise Exception.Create('error: property is not integer');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetOrdProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetOrdProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeIntegerProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkInteger then Raise Exception.Create('error: property is not integer');
  SetOrdProp(instance,fPropInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeIntegerProperty.caption: string;
begin
  if fCaption='' then Result:=fPropPath+'='+IntToStr(fVal)
  else Result:=fCaption;
end;

procedure TChangeIntegerProperty.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Path',ReadPath,WritePath,true); //не будем жадничать, путь всегда ненулевой!
  Filer.DefineProperty('value',ReadValue,WriteValue,(fVal<>0));
  Filer.DefineProperty('backup',ReadBackup,WriteBackup,(fBackup<>0));
  Filer.DefineProperty('caption',ReadCaption,WriteCaption,(fCaption<>''));
end;

procedure TChangeIntegerProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeIntegerProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeIntegerProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadInteger;
end;

procedure TChangeIntegerProperty.WriteValue(writer: TWriter);
begin
  writer.WriteInteger(fVal);
end;

procedure TChangeIntegerProperty.ReadBackup(reader: TReader);
begin
  fBackup:=reader.ReadInteger;
end;

procedure TChangeIntegerProperty.WriteBackup(writer: TWriter);
begin
  writer.WriteInteger(fBackup);
end;

procedure TChangeIntegerProperty.ReadCaption(reader: TReader);
begin
  fCaption:=reader.ReadString;
end;

procedure TChangeIntegerProperty.WriteCaption(writer: TWriter);
begin
  writer.WriteString(fCaption);
end;


(*
              TChangeBoolProperty
                                        *)

constructor TChangeBoolProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeBoolProperty.Create(aPropPath: string; value: Boolean);
begin
  Create(nil);
  fpropPath:=aPropPath;
  fVal:=value;
end;

function TChangeBoolProperty.Execute: boolean;
var res: LongInt;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not boolean');
  res:=GetOrdProp(instance,fPropInfo);
  if fVal=Boolean(res) then result:=false
  else begin
    SetOrdProp(instance,fPropInfo,Integer(fVal));
    Result:=true;
  end;
end;

function TChangeBoolProperty.Undo: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not float number');
    SetOrdProp(instance,fPropInfo,Integer(not fVal));
  Result:=true;
end;

function TChangeBoolProperty.Caption: string;
begin
  Result:=fPropPath+'='+BoolToStr(fVal,true);
end;

procedure TChangeBoolProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeBoolProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeBoolProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadBoolean;
end;

procedure TChangeBoolProperty.WriteValue(writer: TWriter);
begin
  writer.WriteBoolean(fVal);
end;

procedure TChangeBoolProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('Path',ReadPath,WritePath,true);
  filer.DefineProperty('Value',ReadValue,WriteValue,true);
end;

(*
            TChangeEnumProperty
                                    *)
constructor TChangeEnumProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeEnumProperty.Create(aPropPath: string; valName: string);
begin
  Create(nil);
  fPropPath:=aPropPath;
  fValName:=valName;
end;

procedure TChangeEnumProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('Path',ReadPath,WritePath,true);
  filer.DefineProperty('ValName',ReadValName,WriteValName,true);
end;

procedure TChangeEnumProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeEnumProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeEnumProperty.ReadValName(reader: TReader);
begin
  fValName:=reader.ReadString;
end;

procedure TChangeEnumProperty.WriteValName(writer: TWriter);
begin
  writer.WriteString(fValName);
end;

function TChangeEnumProperty.Caption: string;
begin
  Result:=fPropPath+'='+fValName;
end;

function TChangeEnumProperty.Execute: Boolean;
var tmp: string;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not enumeration');
  tmp:=GetEnumProp(instance,fPropInfo);
  if fValName=tmp then result:=false
  else begin
    SetEnumProp(instance,fPropInfo,fValName);
    Result:=true;
  end;
end;

function TChangeEnumProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not enumeration');
  SetEnumProp(instance,fPropInfo,fValName);
  Result:=true;
end;


(*
          TChangeStringProperty
                                  *)
constructor TChangeStringProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeStringProperty.Create(aPropPath: string; value: String);
begin
  Create(nil);
  fVal:=value;
  fPropPath:=aPropPath;
end;

procedure TChangeStringProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeStringProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadString;
end;

procedure TChangeStringProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeStringProperty.WriteValue(writer: TWriter);
begin
  writer.WriteString(fVal);
end;

procedure TChangeStringProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('path',ReadPath,WritePath,true);
  filer.DefineProperty('value',ReadValue,WriteValue,Length(fval)>0);
end;

function TChangeStringProperty.Caption: string;
begin
  result:=fPropPath+'='+fVal;
end;

function TChangeStringProperty.Execute: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if not (fPropInfo.PropType^.Kind in [tkString,tkLstring,tkWstring]) then Raise Exception.Create('error: property is not string');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetStrProp(instance,fPropInfo);
//  GetFloatProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetStrProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeStringProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if not (fPropInfo.PropType^.Kind in [tkString,tkLstring,tkWstring]) then Raise Exception.Create('error: property is not string');
  SetStrProp(instance,fPropInfo,fBackup);
  fBackUp:=''; //чтобы места не занимал
  Result:=true;
end;

(*
            TCommandList
                                  *)

constructor TCommandList.Create(AOwner: TComponent);
var tmp: TComponent;
begin
  inherited Create(AOwner);
  if Aowner=nil then FRoot:=self
  else begin
    tmp:=Aowner;
    repeat
      FRoot:=tmp;
      tmp:=tmp.Owner;
    until tmp=nil;
  end;
end;

procedure TCommandList.Add(command: TAbstractCommand);
var i:Integer;
begin
  for i:=fcount-1 downto fcurrent do components[i].Free;
  insertComponent(command);
  inc(fcurrent);
  fcount:=fcurrent;
end;

procedure TCommandList.Undo;
var res: Boolean;
begin
  if UndoEnabled then begin
    dec(fcurrent);
    res:=(components[fcurrent] as TAbstractCommand).Undo;
    Assert(res,'undo command failed');
  end;
end;

procedure TCommandList.Redo;
var res: Boolean;
begin
  if RedoEnabled then begin
    res:=(components[fcurrent] as TAbstractCommand).Execute;
    Assert(res,'redo command failed');
    inc(fcurrent);
  end;
end;

function TCommandList.UndoEnabled: boolean;
begin
  Result:=(fcurrent>0);
end;

function TCommandList.RedoEnabled: boolean;
begin
  Result:=(fcurrent<fcount);
end;

destructor TCommandList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCommandList.Clear;
begin
  self.DestroyComponents;
  fcurrent:=0;
  fcount:=0;
end;

procedure TCommandList.DefineProperties(filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('count',ReadCount,WriteCount,(fcount>0));
  Filer.DefineProperty('current',ReadCurrent,WriteCurrent,(fcurrent>0));
end;

procedure TCommandList.ReadCount(reader: TReader);
begin
  fcount:=reader.ReadInteger;
end;

procedure TCommandList.WriteCount(writer: TWriter);
begin
  writer.writeInteger(fcount);
end;

procedure TCommandList.ReadCurrent(reader: TReader);
begin
  fcurrent:=reader.ReadInteger;
end;

procedure TCommandList.WriteCurrent(writer: TWriter);
begin
  writer.writeInteger(fcurrent);
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
  if Assigned(UndoTree) then initial_pos:=UndoTree.current;
end;

procedure TAbstractDocument.afterConstruction;
var i: Integer;
    buCurrent: TAbstractCommand;
begin
  if UndoTree=nil then begin
    UndoTree:=TCommandTree.Create(self);
    with UndoTree do begin
      Name:='UndoTree';
      Root:=TBranchCommand.Create(UndoTree);
      Current:=Root;
      Root.ActiveBranch:=true;
    end;
    initial_pos:=UndoTree.current;
  end
  else begin
    buCurrent:=UndoTree.Current;
    for i:=0 to UndoTree.ComponentCount-1 do
      if UndoTree.Components[i] is TAbstractCommand then
        TAbstractCommand(UndoTree.Components[i]).ResolveMemory;
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
begin
  for i := 0 to ComponentCount-1 do
    if not (csSubComponent in Components[i].ComponentStyle) and ((Components[i]<>UndoTree) or SaveWithUndo) then
      Proc( Components[i] );
end;

function TAbstractDocument.isEmpty: Boolean;
begin
  Result:=(UndoTree.Root.Next=nil);
end;

function TAbstractDocument.Changed: Boolean;
begin
  Result:=(UndoTree.current<>initial_pos) or new_commands_added;
end;

procedure TAbstractDocument.DispatchCommand(command: TAbstractCommand);
begin
  fCriticalSection.Acquire;
  undotree.InsertComponent(command);
  //может быть, не нужно исполнять конкретно эту команду, она уже есть
  //именно когда обе команды еще не исполнены, их можно сравнивать
  if undotree.CheckForExistingCommand(command) then begin
    change;
    command.Free;
  end
  else if command.Execute then begin
    undotree.RemoveComponent(command);
    UndoTree.Add(command);
    Change;
    new_commands_added:=true;
    end
    else command.Free;

  fCriticalSection.Leave;
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
    fCriticalSection.Acquire;
      UndoTree.Undo;
    fCriticalSection.Leave;
    Change;
  end;
end;

procedure TAbstractDocument.Redo;
begin
  if UndoTree.RedoEnabled then begin
    fCriticalSection.Acquire;
      UndoTree.Redo;
    fCriticalSection.Leave;
    Change;
  end;
end;

procedure TAbstractDocument.JumpToBranch(Branch: TAbstractCommand);
begin
  fCriticalSection.Acquire;
    UndoTree.JumpToBranch(Branch);
  fCriticalSection.Leave;
  Change;
end;

procedure TAbstractDocument.Change;
begin
  if Assigned(onDocumentChange) then onDocumentChange(self);
end;

procedure TAbstractDocument.DoLoad;
begin
  if Assigned(onLoad) then onLoad(self);
end;

function TAbstractDocument.Hash: T4x4LongWordRecord;
var buSaveWithUndo: boolean;
    str: TMemoryStream;
begin
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
  SaveWithUndo:=buSaveWithUndo;
end;

procedure TAbstractDocument.SetOnDocumentChange(value: TNotifyEvent);
begin
  fOnDocumentChange:=value;
  if Assigned(fOnDocumentChange) then fOnDocumentChange(self);
end;

procedure TAbstractDocument.SetOnLoad(value: TNotifyEvent);
begin
  fOnLoad:=value;
  if Assigned(fOnLoad) then fOnLoad(self);
end;

(*
      TCommandTree
                      *)
constructor TCommandTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRoot:=nil;
  fCurrent:=nil;
end;

procedure TCommandTree.Clear;
begin
  fRoot:=nil;
  fCurrent:=nil;
end;

function TCommandTree.FindExistingCommand(command: TAbstractCommand;position: TAbstractCommand): boolean;
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
  Result:=FindExistingCommand(command,current)
else
  Result:=FindExistingCommand(command,current.Next) or FindExistingCommand(command,current.Branch);
end;

procedure TCommandTree.Add(command: TAbstractCommand);
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
  command.ensureCorrectName(command.Name,self);
  insertComponent(command);

  current.Next:=command;
  command.ActiveBranch:=true;
  command.TurnLeft:=false;
  command.Prev:=current;
  current:=command;
end;

procedure TCommandTree.Undo;
begin
  //проверка UndoEnabled гарантирует, что вызов undo будет произведен
  //когда его можно сделать
  if current.Undo then begin
    current.ActiveBranch:=false;
    current:=current.Prev;
    while (current is TInfoCommand) and (current.prev<>nil) do begin
      current.ActiveBranch:=false;
      current:=current.Prev;
    end;
  end
  else Raise Exception.Create('Undo command failed');
end;

procedure TCommandTree.Redo;
begin
  //проверка RedoEnabled гарантирует, что вызов undo будет произведен
  //когда его можно сделать
  while current.TurnLeft do begin
    current:=current.Branch;
    current.ActiveBranch:=true;
  end;
  repeat
    if current.TurnLeft then current:=current.Branch
    else current:=current.Next;
    current.ActiveBranch:=true;
  until (not (current is TInfoCommand));
  if not current.Execute then Raise Exception.Create('Redo command failed');
end;

procedure TCommandTree.JumpToBranch(command: TAbstractCommand);
var b: TAbstractCommand;
begin
  //самая веселая команда
  //нужно перейти на произвольное состояние
  //первым делом, надо проторить путь от command до активного пути
  b:=command;
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
end;

function TCommandTree.UndoEnabled: Boolean;
var t: TAbstractCommand;
begin
  t:=current;
  while (t is TInfoCommand) and (t.Prev<>nil) do t:=t.Prev;
  Result:=(t.Prev<>nil);
end;

function TCommandTree.RedoEnabled: Boolean;
var t: TAbstractCommand;
begin
  t:=current;
  while (t.Next<>nil) and (t.Next is TInfoCommand) do t:=t.Next;
  Result:=(t.Next<>nil);
end;

procedure TCommandTree.RecursiveCompare(t1,t2: TAbstractCommand;var same,plus,minus: Integer);
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

procedure TCommandTree.RecursiveMerge(t1,t2: TAbstractCommand);
var iterator: TAbstractCommand;
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

procedure TCommandTree.Assimilate(t: TAbstractCommand);
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
  doc.tool.free;
  ToolClass:=TAbstractToolActionClass(self.classType);
  doc.Tool:=ToolClass.Create(doc);
  doc.Tool.Name:='Tool';
  doc.Tool.Assign(self);
  doc.Tool.Select;
end;

destructor TAbstractToolAction.Destroy;
begin
  Unselect;
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
RegisterClasses([TCommandList,TChangeFloatProperty,TChangeBoolProperty,TChangeEnumProperty,TChangeIntegerProperty,TChangeStringProperty,TBranchCommand,TInfoCommand,TSavedAsInfoCommand]);

end.
