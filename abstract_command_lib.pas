unit abstract_command_lib;

interface

uses Introspected_streaming_class,classes;

type

TAbstractCommand=class(TIntrospectedStreamingClass)  //����� ������� ��������� ����� ���� ������� ������ �� ���� ���������
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
//�����-����� "��������", ���������� ���� � command_list

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
    function currentExecutedCommand: TAbstractCommand; virtual; abstract;  //� ������ ���. ������� �����.
    //� ��� ������ ���������. Executed-����� �������, �� ����� �� ��������� ����� �� ���.
    function GetUndoIterator: TAbstractCommandIterator; virtual; abstract;
    function GetRedoIterator: TAbstractCommandIterator; virtual; abstract;  //��� ������� undo/redo
    function GetAllCommandsIterator: TAbstractCommandIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
  end;

//�������, ����� ������� �� ������ ����������� ������ ������, ������ - ������� ��
//���������� ��� � ����� ���������. ���������: ���������� � ������ � ��������,
//������ �� ����������� ������ � ���������� ����������, ����������� � ���������
  ITerminalCommand = interface
  ['{902F14E5-FCFB-4F72-ABCB-B71819D36D8A}']
    //���, "��������� �������������":���������������� ������������� � TCommandTree,
    //��� ��-������� ������������ ������� � ����������� �� �� "����������������"
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
  //� ����� ��������� ���� ������� ������ ���� ���
  //���������� � ���� ����� � ���� �������� ����������
  t:=DateTimeToTimeStamp(Now);
  //���� � ����� �� �� ��� �� ����������� ������������ - ����� ������ ����� ����.
  //������������
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
  //����� ����� ����������� � �������/���������. ���������,
  //����� ������ ����������� ����������
  //����� � ������� ����� �� ������������, ��� ��������� ������� ������ CommandTree,
  //�� ��� ����� ����������� � ���������.
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
