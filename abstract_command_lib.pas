unit abstract_command_lib;

interface

uses Introspected_streaming_class,classes;

type

TAbstractCommand=class(TIntrospectedStreamingClass)  //чтобы историю изменений можно было хранить вместе со всем остальным
  public
    class function ImageIndex: Integer; virtual;
    constructor Create(Aowner: TComponent); override;
    function Execute: Boolean; virtual; abstract;
    function Undo: boolean; virtual; abstract;
    function caption: string; virtual;
    procedure ResolveMemory; virtual;
    function NameToDateTime: TDateTime;
    function NameToDate(aName: TComponentName): Integer;
  end;
//самые-самые "зайчатки", заработает даже в command_list

TAbstractCommandIterator = class
  public
    function GetCommand(out command: TAbstractCommand): Boolean; virtual; abstract;
end;

TAbstractCommandContainer=class(TIntrospectedStreamingClass)
  public
    procedure Add(command: TAbstractCommand); virtual; abstract;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
    function UndoEnabled: Boolean; virtual; abstract;
    function RedoEnabled: Boolean; virtual; abstract;
    function CheckForExistingCommand(command: TAbstractCommand): boolean; virtual; abstract;
    procedure JumpToBranch(command: TAbstractCommand); virtual; abstract;
    function currentExecutedCommand: TAbstractCommand; virtual; abstract;  //и просто тек. команду возвр.
    //и как начало итератора. Executed-чтобы помнить, мы стоим на состоянии ПОСЛЕ ее вып.
    function GetUndoIterator: TAbstractCommandIterator; virtual; abstract;
    function GetRedoIterator: TAbstractCommandIterator; virtual; abstract;  //для списков undo/redo
    function GetAllCommandsIterator: TAbstractCommandIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
  end;

//команда, после которой не должно выполняться других команд, взамен - возврат на
//предыдущий шаг и новое ветвление. Мотивация: сохранение в формат с потерями,
//запрет на продолжение работы с искаженным документом, возвращение к исходному
  ITerminalCommand = interface
  ['{902F14E5-FCFB-4F72-ABCB-B71819D36D8A}']
    //увы, "классовая дискриминация":функциональность закладывается в TCommandTree,
    //она по-разному обрабатывает команды в зависимость от их "наследственности"
  end;




TAbstractCommandContainerClass = class of TAbstractCommandContainer;

procedure RegisterCommandContainerClass(value: TAbstractCommandContainerClass);
function GetCommandContainerClass: TAbstractCommandContainerClass;

implementation

uses sysutils,strutils;

var ContainerClass: TAbstractCommandContainerClass;

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

function TAbstractCommand.caption: string;
begin
  result:=self.ClassName;
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

procedure TAbstractCommand.ResolveMemory;
begin
  //здесь можно отправиться в прошлое/альтернат. вселенную,
  //чтобы узнать необходимую информацию
  //назад в будущее можно не возвращаться, эту процедуру вызовет только CommandTree,
  //он сам потом возвратится в настоящее.
end;

class function TAbstractCommand.ImageIndex: Integer;
begin
  Result:=-1;
end;

procedure RegisterCommandContainerClass(value: TAbstractCommandContainerClass);
begin
  if Assigned(ContainerClass) then
    raise Exception.CreateFMT(
      'Cannot register command container %s because %s already registered',
      [value.ClassName,ContainerClass.ClassName]);
  ContainerClass:=value;
end;

function GetCommandContainerClass: TAbstractCommandContainerClass;
begin
  Result:=ContainerClass;
end;

end.
