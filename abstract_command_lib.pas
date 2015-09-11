unit abstract_command_lib;

interface

uses streaming_class_lib,classes;

type

TAbstractCommand=class(TStreamingClass)  //����� ������� ��������� ����� ���� ������� ������ �� ���� ���������
  protected
    fImageIndex: Integer; //���������� ��������
  public
    constructor Create(Aowner: TComponent); override;
    function Execute: Boolean; virtual; abstract;
    function Undo: boolean; virtual; abstract;
    function caption: string; virtual;
    function NameToDateTime: TDateTime;
    function NameToDate(aName: TComponentName): Integer;
  published
    property ImageIndex: Integer read fImageIndex write fImageIndex;
  end;
//�����-����� "��������", ���������� ���� � command_list

TAbstractCommandContainer=class(TStreamingClass)
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
    function PrevCommand: TAbstractCommand; virtual; abstract;
    function NextCommand: TAbstractCommand; virtual; abstract;
  end;

implementation

uses sysutils,strutils;

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

end.
