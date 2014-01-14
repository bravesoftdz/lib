unit command_class_lib;

interface
uses streaming_class_lib,classes,TypInfo;
type
  TAbstractCommand=class(TStreamingClass)  //чтобы историю изменений можно было хранить вместе со всем остальным
    private
      fNext,fPrev,fBranch: TAbstractCommand;
      fActiveBranch: Boolean;
      fTurnLeft: Boolean;
    protected
      fImageIndex: Integer;
      procedure ensureCorrectName(proposedName: string; aowner: TComponent);
    public
      constructor Create(Aowner: TComponent); override;
      function Execute: Boolean; virtual; abstract;
      function Undo: boolean; virtual; abstract;
      function caption: string; virtual;
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
      procedure Clear;
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
    published
      property Root: TAbstractCommand read fRoot write fRoot;
      property Current: TAbstractCommand read fCurrent write fCurrent;
    end;

  TAbstractDocument=class(TStreamingClass) //документ вместе со списком undo/redo
    private
      initial_pos: TAbstractCommand;
      new_commands_added: Boolean;
    protected
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    public
      SaveWithUndo: boolean;
      FileName: string;
      onDocumentChange: TNotifyEvent;
      onLoad: TNotifyEvent;
      constructor Create(Aowner: TComponent); override;
      constructor LoadFromFile(aFileName: string); override;
      procedure AfterConstruction; override;
      destructor Destroy; override;

      procedure Undo;
      procedure Redo;
      procedure JumpToBranch(Branch: TAbstractCommand);

      function isEmpty: Boolean;
      function Changed: Boolean;
      procedure DispatchCommand(command: TAbstractCommand);
      procedure Save;
      procedure Change; virtual;
      procedure DoLoad; virtual;
    published
//      UndoList: TCommandList;
        UndoTree: TCommandTree;
//      property UndoList: TCommandList read GetUndoList write FUndoList stored false;
    end;

implementation

uses SysUtils,StrUtils;

(*
        TAbstractCommand
                                  *)
procedure TAbstractCommand.ensureCorrectName(proposedName: string; aowner: TComponent);
var FullName: string;
    i: Integer;
begin
  FullName:=proposedName;
  if assigned(aowner) then begin
    i:=0;
    while aowner.FindComponent(FullName)<>nil do begin
      FullName:=proposedName+IntToStr(i);
      inc(i);
    end;
  end;
  Name:=FullName;
end;

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
  Next:=nil;
  Prev:=nil;
  Branch:=nil;
  ActiveBranch:=false;
  fImageIndex:=-1;
end;


function TAbstractCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var our_class: TStreamingClassClass;
    t: TAbstractCommand;
begin
  if ClassType=what.ClassType then begin
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
            TChangePropertiesCommand
                                          *)

procedure TChangePropertiesCommand._getPropInfo(propPath: string);
var i,j,L: Integer;
  PropValue: TObject;
  fPropName: string;
begin
  i := 1;
  L := Length(propPath);
  Instance := FindOwner;
  while True do
    begin
      j := i;
      while (i <= L) and (PropPath[i] <> '.') do Inc(i);
      FPropName := Copy(PropPath, j, i - j);
      if i > l then Break;
      fPropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
      if fPropInfo = nil then
          Raise Exception.Create('Property '+FPropName+' not found');
      PropValue := nil;
      if fPropInfo^.PropType^.Kind = tkClass then
        PropValue := TObject(GetOrdProp(Instance, fPropInfo));
      if not (PropValue is TPersistent) then Raise Exception.Create('Wrong property path');
      Instance := TPersistent(PropValue);
      Inc(I);
    end;
    fPropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
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
//  UndoList:=nil;
  UndoTree:=nil;
  FileName:='';
  onDocumentChange:=nil;
  onLoad:=nil;
  SaveWithUndo:=true;
  initial_pos:=nil;
  new_commands_added:=false;
end;

constructor TAbstractDocument.LoadFromFile(aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  FileName:=aFileName;
  new_commands_added:=false;
  if Assigned(UndoTree) then initial_pos:=UndoTree.current;
end;

procedure TAbstractDocument.afterConstruction;
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
  end;
end;

destructor TAbstractDocument.Destroy;
begin
  UndoTree.Free;
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
  self.InsertComponent(command);
  //может быть, не нужно исполнять конкретно эту команду, она уже есть
  //именно когда обе команды еще не исполнены, их можно сравнивать
  if undotree.CheckForExistingCommand(command) then begin
    change;
    command.Free;
  end
  else if command.Execute then begin
    self.RemoveComponent(command);
    UndoTree.Add(command);
    Change;
    new_commands_added:=true;
    end
    else command.Free;
end;

procedure TAbstractDocument.Save;
begin
  Assert(FileName<>'','WTF: empty filename');
  self.SaveToFile(FileName);
  initial_pos:=UndoTree.current;
  new_commands_added:=false;
end;

procedure TAbstractDocument.Undo;
begin
  if UndoTree.UndoEnabled then begin
    UndoTree.Undo;
    Change;
  end;
end;

procedure TAbstractDocument.Redo;
begin
  if UndoTree.RedoEnabled then begin
    UndoTree.Redo;
    Change;
  end;
end;

procedure TAbstractDocument.JumpToBranch(Branch: TAbstractCommand);
begin
  UndoTree.JumpToBranch(Branch);
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

(*
      TCommandTree
                      *)
constructor TCommandTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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


initialization
RegisterClasses([TCommandList,TChangeFloatProperty,TChangeBoolProperty,TChangeEnumProperty,TChangeIntegerProperty,TChangeStringProperty,TBranchCommand,TInfoCommand,TSavedAsInfoCommand]);

end.
