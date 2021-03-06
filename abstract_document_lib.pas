unit abstract_document_lib;

interface

uses classes,sysUtils,actnList,introspected_streaming_class,abstract_command_lib,
  comCtrls, UITypes, Types, Messages, streaming_class_lib;

type

  TAbstractToolAction=class;

  TAbstractDocument=class(TIntrospectedStreamingClass) //�������� ������ �� ������� undo/redo
    private
      fOnDocumentChange: TNotifyEvent;
      fOnLoad: TNotifyEvent;
      fCriticalSection: TMultiReadExclusiveWriteSynchronizer;
      fActionList: TActionList;
      procedure SetOnDocumentChange(value: TNotifyEvent);
      procedure SetOnLoad(value: TNotifyEvent);
    protected
      initial_pos: TAbstractCommand;  //������, ���������� �� ��������� ����� ����.
      new_commands_added: Boolean;  //� ��������� �� ����� �������
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
      procedure Notification(aComponent: TComponent; operation: TOperation); override;
    public
      SaveWithUndo: boolean;
      StatusPanel: TStatusPanel;
      DoneStatusPanel: TStatusPanel;
      FileName: string;
      constructor Create(Aowner: TComponent); override;
      constructor LoadFromFile(const aFileName: string); override;
      constructor LoadFromTemporaryFile(aFileName: string);
      procedure AfterConstruction; override;
      destructor Destroy; override;
      procedure Release;

      function NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean; override;

      //��� �������������� � ActionList
      procedure RegisterActionList(value: TActionList);

      procedure Undo;
      procedure Redo;
      procedure JumpToBranch(Branch: TAbstractCommand);

      function isEmpty: Boolean;
      function Changed: Boolean;
      function DispatchCommand(command: TAbstractCommand): boolean;
      procedure Save;
      procedure Autosave;
      procedure Change; virtual;
      procedure DoLoad; virtual;
      property onDocumentChange: TNotifyEvent read fOnDocumentChange write SetOnDocumentChange;
      property onLoad: TNotifyEvent read fOnLoad write SetOnLoad;
      property CriticalSection: TMultiReadExclusiveWriteSynchronizer read fCriticalSection;

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
    procedure MouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure MouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure KeyDown(var Msg: TWMKey; var Handled: Boolean); virtual;
  end;

  TAbstractToolActionClass=class of TAbstractToolAction;

  IHistoryEvents = interface
  ['{9514240A-83A8-41A6-A6EC-6644F1EA98CC}']
    procedure RefreshHistoryHighlights;
    procedure ChangeHistory;
  end;

//���������-"����", ��� �� ������ ������ ���������, � ��������� ����� �����������
//�������������, � ������, ������� ����� ����� ������ � caption ������� ����� � ����,
//� �� ������������ "�� �������" �� ������ ����������, � �� ������� ������ ������ � �����
  IConstantComponentName = interface
  ['{6BD88A2A-129F-4FEE-8B55-F82906B4D1D7}']
  end;

var CurProjectFileName: string='current_project.txt'; //not to translate
    DocumentDefaultDir: string;
    DocumentDefaultSaveFormat: TStreamingClassSaveFormat = sfAscii;


implementation
(*
              TAbstractDocument
                                      *)

constructor TAbstractDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SaveWithUndo:=true;
  fCriticalSection:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

constructor TAbstractDocument.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  FileName:=aFileName;
  new_commands_added:=false;
  if UndoContainer<>nil then initial_pos:=UndoContainer.currentExecutedCommand;
end;

constructor TAbstractDocument.LoadFromTemporaryFile(aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  new_commands_added:=false;
end;

procedure TAbstractDocument.afterConstruction;
var i: Integer;
    buCurrent: TAbstractCommand;
    iterator: TAbstractCommandIterator;
    cur: TAbstractCommand;
begin
  if UndoContainer=nil then begin
    UndoContainer:=GetCommandContainerClass.Create(self);
    UndoContainer.Name:='UndoContainer';
    initial_pos:=UndoContainer.currentExecutedCommand;
  end
  else begin
    buCurrent:=UndoContainer.currentExecutedCommand;
    iterator:=UndoContainer.GetAllCommandsIterator;
    try
      while iterator.GetCommand(cur) do
        cur.ResolveMemory;
    finally
      iterator.Free;
    end;
    UndoContainer.JumpToBranch(buCurrent);
  end;
end;

procedure TAbstractDocument.Release;
begin
  if Assigned(self) then begin
    fCriticalSection.BeginWrite;
    Destroy;
  end;
end;

destructor TAbstractDocument.Destroy;
begin
  UndoContainer.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TAbstractDocument.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
begin
  for i := 0 to ComponentCount-1 do
    if not (csSubComponent in Components[i].ComponentStyle) and
    (((Components[i]<>UndoContainer) and (Components[i]<>Tool)) or SaveWithUndo) then
      Proc(Components[i]);
end;

function TAbstractDocument.NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean;
var i: integer;
    c: TComponent;
begin
  c:=FindComponent(proposedName);
  Result:=Assigned(c) and (c<>me);
  if not Result then
    for i:=0 to ComponentCount-1 do begin
      if (Components[i] is TIntrospectedStreamingClass) and
         not (Components[i]=UndoContainer) and
         not (Components[i]=Tool) then begin
        Result:=Result or
          TIntrospectedStreamingClass(Components[i]).NameExistsSomewhere(proposedName,me);
        if Result=true then break;
      end;
    end;
end;

procedure TAbstractDocument.Notification(aComponent: TComponent; operation: TOperation);
begin
  if (operation=opRemove) and (aComponent=fActionList) then
    fActionList:=nil;
  inherited;
end;

function TAbstractDocument.Changed: Boolean;
begin
  Result:=(UndoContainer.currentExecutedCommand<>initial_pos) or new_commands_added;
end;

procedure TAbstractDocument.RegisterActionList(value: TActionList);
begin
  fActionList:=value;
  if Assigned(value) then
    value.FreeNotification(self); //���� ����� ���������, �� ����� �� �� ����
    //������ � �������� ����. � nil
end;

function TAbstractDocument.DispatchCommand(command: TAbstractCommand): Boolean;
var term: ITerminalCommand;
    historyEvents: IHistoryEvents;
begin
  //����� ���������, ����� �� �� ����� ��������� ������� � ������ �����
  if undoContainer.currentExecutedCommand.GetInterface(ITerminalCommand,term)
    and undoContainer.UndoEnabled then
      UndoContainer.Undo;

  undoContainer.InsertComponent(command);
  //����� ����, �� ����� ��������� ��������� ��� �������, ��� ��� ����
  //������ ����� ��� ������� ��� �� ���������, �� ����� ����������
  if undoContainer.CheckForExistingCommand(command) then begin
    //��������� ��� ����������, �� ��������� ������������ �������
    change;
    //�� history �� ���� �������������, ���� ������� ���. �������
    if Assigned(fActionList) and
      fActionList.GetInterface(IHistoryEvents,historyEvents) then
        historyEvents.RefreshHistoryHighlights;
    command.Free;
    Result:=false;
  end
  else if command.Execute then begin
    undoContainer.RemoveComponent(command);
    undoContainer.Add(command);
    Change;
    if Assigned(fActionList) and
      fActionList.GetInterface(IHistoryEvents, historyEvents) then
        historyEvents.ChangeHistory;
    new_commands_added:=true;
    Result:=true;
  end
  else begin
    command.Free;
    Result:=false;
  end;
end;

procedure TAbstractDocument.Save;
begin
  TSavingThread.Create(self);
  initial_pos:=UndoContainer.currentExecutedCommand;
  new_commands_added:=false;
end;

procedure TAbstractDocument.Autosave;
var buCurDir: string;
    buSaveFormat: TStreamingClassSaveFormat;
    buSaveWithUndo: boolean;
begin
  buCurDir:=GetCurrentDir;
  buSaveFormat:=saveFormat;
  buSaveWithUndo:=SaveWithUndo;
  SetCurrentDir(DocumentDefaultDir);
  SaveFormat:=DocumentDefaultSaveFormat;
  SaveWithUndo:=true;
  SaveToFile(CurProjectFileName);

  saveFormat:=buSaveFormat;
  SaveWithUndo:=buSaveWithUndo;
  SetCurrentDir(buCurDir);
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
  //����������, ��� ��������� nil � ����. �������:
  //- �������� ����������� ������������� ActionList'� (�� �������� �������� doc)
  //- doc=nil, �.�. actionList �� �� ��� �� ���������
  //- doc^=nil, �.� actionList ��������� �� ����������, ������� ��������� �� nil
end;

function TAbstractDocumentAction.HandlesTarget(Target: TObject): Boolean;
begin
//��������� �� ������� ��� ���� �� ����� ���������, �� ����� ��������
  Result:=Assigned(Target) and (Target is TAbstractDocument);
//������-������� ����� �������� ���� �������� � inherited HandlesTarget(Target) and ...
//�� �������� short cut, ���� ���� ��� �����, ������ �� �� �������.
end;

function TAbstractDocumentAction.Update: boolean;
var doc: TabstractDocument;
begin
  inherited Update;
//��� ����� ����� ���������� ����� �� ����, ����� ��������, �� ���� ��
//��������� ����. ���������� ��� ��� ���-������ � ���� ����
  doc:=getDoc;
  Enabled:=Assigned(doc);
  Result:=true; //�� ���� �� �������� ��� ��� ������ � ������ ������ �� ����
//���� ����������� �������� ������, ����� ���������� � �������� ���������
end;

(*
      TAbstractToolAction
                                *)
procedure TAbstractToolAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
  ToolClass: TAbstractToolActionClass;
begin
  doc:=Target as TAbstractDocument;
  //������� ����� ����������� �� ��������� � ������
  //���� ���-���� ������������ ������ - ���� �������� �� �������� ��������
  //� �� ���/���� �������
  //� ������ - �� ������ ����������� � ���������� ������ � ���������
  //� ���� ������� ���� �����, �� �������� ���������
  ToolClass:=TAbstractToolActionClass(self.classType);
  if Assigned(doc.Tool) then begin
    if not (doc.Tool is ToolClass) then begin
      doc.Tool.Unselect;
      doc.Tool.Free;
      doc.Tool:=ToolClass.Create(doc);
      doc.Tool.Name:='Tool';
      doc.Tool.Assign(self);
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
  //������ �� ������ ����� ����������� �� �������, ������� ������� "������" ����������, �����
  //�� �������� ���� ���
end;

procedure TAbstractToolAction.MouseMove(Shift: TShiftState; X,Y: Integer);
begin

end;

procedure TAbstractToolAction.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin

end;

procedure TAbstractToolAction.MouseWheelUp(Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled:=false;
end;

procedure TAbstractToolAction.MouseWheelDown(Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled:=false;
end;


end.
