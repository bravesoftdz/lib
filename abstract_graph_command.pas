unit abstract_graph_command;

interface

uses abstract_command_lib, Hash_wrapper,classes;

type

TCommandState = (csForward,csBackward,csDummy);

TAbstractGraphCommand = class (TAbstractCommand)
  private
    fPrevState,fNextState: THash; //������ �� ���������, ������ ��� ��� � ����� ��� �����.
    //��������� �����
    fCommandState: TCommandState;
    procedure ReadPrev(stream: TStream);  //��� ��� ����������� �� ������������
    procedure ReadNext(stream: TStream);
    procedure WritePrev(stream: TStream);
    procedure WriteNext(stream: TStream);
  protected
    procedure DefineProperties(filer: TFiler); override;
  published
    property CommandState: TCommandState read fCommandState write fCommandState;
end;

TCommandGraph = class (TAbstractCommandContainer)
  private
    
  public
    procedure Add(command: TAbstractCommand); override;
    procedure Undo; override;
    procedure Redo; override;
    function UndoEnabled: Boolean; override;
    function RedoEnabled: Boolean; override;
    function CheckForExistingCommand(command: TAbstractCommand): boolean; override;
    procedure JumpToBranch(command: TAbstractCommand); override;
    function currentExecutedCommand: TAbstractCommand; override;  //� ������ ���. ������� �����.
    //� ��� ������ ���������. Executed-����� �������, �� ����� �� ��������� ����� �� ���.
    function PrevCommand: TAbstractCommand; override;
    function NextCommand: TAbstractCommand; override;
end;


implementation

(*
      TAbstractGraphCommand
                                *)
procedure TAbstractGraphCommand.DefineProperties(filer: TFiler);
begin
  filer.DefineBinaryProperty('PrevState',ReadPrev,WritePrev,True);
  filer.DefineBinaryProperty('NextState',ReadNext,WriteNext,True);
  //����� ������� ������� � ����, �� ����� � ���������, � �������� ���������
end;

procedure TAbstractGraphCommand.ReadPrev(stream: TStream);
begin
  stream.Read(fPrevState[0],SizeOf(fPrevState));
end;

procedure TAbstractGraphCommand.WritePrev(stream: TStream);
begin
  stream.Write(fPrevState[0],SizeOf(fPrevState));
end;

procedure TAbstractGraphCommand.ReadNext(stream: TStream);
begin
  stream.Read(fNextState[0],SizeOf(fNextState));
end;

procedure TAbstractGraphCommand.WriteNext(stream: TStream);
begin
  stream.Write(fNextState[0],SizeOf(fNextState));
end;

(*
    TCommandGraph
                      *)


end.
