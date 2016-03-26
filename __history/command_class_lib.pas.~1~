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



//TInfoCommand ничего не делает, при undo/redo её необходимо пропускать
  TInfoCommand=class(TAbstractTreeCommand)
    public
      constructor Create(AOwner: TComponent); override;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function SmartDateTimeToStr: string;
    end;
//важнейшая разновидность TInfoCommand - с ней начинается каждая новая ветвь
//фактически, превращает бинарное дерево в обычное (с неограниченным количеством ветвей)
  TBranchCommand=class(TInfoCommand)
    public
      function caption: string; override;
    end;
//"файл сохранен"
  TSavedAsInfoCommand=class(TInfoCommand)
    private
      fFileName: string;
    public
      constructor Create(aFileName: string); reintroduce; overload;
      function caption: string; override;
    published
      property FileName: string read fFileName write fFileName;
    end;

//команда, после которой не должно выполняться других команд, взамен - возврат на
//предыдущий шаг и новое ветвление. Мотивация: сохранение в формат с потерями,
//запрет на продолжение работы с искаженным документом, возвращение к исходному
  ITerminalCommand = interface
  ['{902F14E5-FCFB-4F72-ABCB-B71819D36D8A}']
    //увы, "классовая дискриминация":функциональность закладывается в TCommandTree,
    //она по-разному обрабатывает команды в зависимость от их "наследственности"
  end;

  {$IFDEF NEW_HASH}
    T4x4LongWordRecord =array [0..3] of LongWord;
  {$ENDIF}

//интерфейс-"флаг", что по логике работы программы, в документе имена компонентов
//зафиксированы, и значит, команды могут смело писать в caption текущие имена и пути,
//а не возвращаться "во времени" на момент выполнения, и не хранить лишние тексты в файле
  IConstantComponentName = interface
  ['{6BD88A2A-129F-4FEE-8B55-F82906B4D1D7}']
  end;




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
