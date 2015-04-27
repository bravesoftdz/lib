unit abstract_command_lib;

interface

uses streaming_class_lib;

type

TAbstractCommand=class(TStreamingClass)  //чтобы историю изменений можно было хранить вместе со всем остальным
  protected
    fImageIndex: Integer; //картиночку показать
  public
    function Execute: Boolean; virtual; abstract;
    function Undo: boolean; virtual; abstract;
    function caption: string; virtual;
  published
    property ImageIndex: Integer read fImageIndex write fImageIndex;
  end;
//самые-самые "зайчатки", заработает даже в command_list

TAbstractCommandContainer=class(TStreamingClass)
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
    function PrevCommand: TAbstractCommand; virtual; abstract;
    function NextCommand: TAbstractCommand; virtual; abstract;
  end;

implementation

function TAbstractCommand.caption: string;
begin
  result:=self.ClassName;
end;

end.
