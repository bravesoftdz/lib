unit command_class_lib;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0 }
    {$DEFINE NEW_HASH}
  {$IFEND}
{$ENDIF}

interface

uses streaming_class_lib,IntrospectionLib,classes,TypInfo,IdHash,SyncObjs,actnlist,controls,
comctrls,messages,abstract_command_lib,types;

type



//TInfoCommand ������ �� ������, ��� undo/redo � ���������� ����������
  TInfoCommand=class(TAbstractTreeCommand)
    public
      constructor Create(AOwner: TComponent); override;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function SmartDateTimeToStr: string;
    end;
//��������� ������������� TInfoCommand - � ��� ���������� ������ ����� �����
//����������, ���������� �������� ������ � ������� (� �������������� ����������� ������)
  TBranchCommand=class(TInfoCommand)
    public
      function caption: string; override;
    end;
//"���� ��������"
  TSavedAsInfoCommand=class(TInfoCommand)
    private
      fFileName: string;
    public
      constructor Create(aFileName: string); reintroduce; overload;
      function caption: string; override;
    published
      property FileName: string read fFileName write fFileName;
    end;

  {$IFDEF NEW_HASH}
    T4x4LongWordRecord =array [0..3] of LongWord;
  {$ENDIF}

  TAbstractDocumentInnerObject = class(TStreamingClass)
    public
      function Doc: TAbstractDocument;
      function SaveWithUndo: Boolean;
    end;


var DocumentHashTimeout: Cardinal = 3000; //ms

implementation

uses SysUtils,StrUtils,IdHashMessageDigest,abstract_document_actions,forms,
localized_string_lib, Streaming_Format_Cyr;

var BeginHashEvent: TEvent;

procedure WaitForHashEvent;
begin
  case BeginHashEvent.WaitFor(DocumentHashTimeout) of
    wrTimeout: raise Exception.Create('DispatchCommand: wait for hash begin timeout');
    wrError: raise Exception.Create('DispatchCommand: wait for hash begin error');
    wrAbandoned: raise Exception.Create('DispatchCommand: Hashing Thread abandoned');
  end;
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
  //��� �������-���� ��������, ����� ����� ���� ������ ������� ������ ���������
  //� ����� ������ ����� �� ����� ������
  Result:=true;
  //����� ������� �� ���������� � �� ����� ��������� � ������
end;

function TInfoCommand.Undo: Boolean;
begin
  Result:=true;
  //������� � ������
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
  BranchCommandMsg = '����� ������� %s';
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
  SavedAsInfoCommandSavedMsg='�������� %s';
  SavedAsInfoCommandSavedAsMsg='�������� ��� %s %s';
begin
  last:=prev;
  while Assigned(last) and not (last is TSavedAsInfoCommand) do last:=last.Prev;
  if Assigned(last) and (TSavedAsInfoCommand(last).FileName=FileName) then
    Result:=Format(SavedAsInfoCommandSavedMsg,[SmartDateTimeToStr])
  else
    Result:=Format(SavedAsInfoCommandSavedAsMsg,[fFileName,SmartDateTimeToStr]);
end;


(*
      TAbstractDocumentInnerObject
                                      *)
function TAbstractDocumentInnerObject.Doc: TAbstractDocument;
begin
  Result:=FindOwner as TAbstractDocument;
end;

function TAbstractDocumentInnerObject.SaveWithUndo: Boolean;
begin
  Result:=Doc.SaveWithUndo;
end;


initialization
RegisterClasses([TCommandTree,TBranchCommand,TInfoCommand,TSavedAsInfoCommand,
TChangeIntegerCommand,TChangeBoolCommand,TChangeFloatCommand,TChangeStringCommand,
TChangeLocaleStringCommand]);
BeginHashEvent:=TEvent.Create(nil,false,false,'AbstractDocumentBeginHash');

finalization
BeginHashEvent.Free;

end.
