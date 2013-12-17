unit abstract_document_actions;

interface

uses actnlist,command_class_lib,classes,dialogs;

type

TAbstractDocumentClass=class of TAbstractDocument;

PAbstractDocument=^TabstractDocument;

TAbstractDocumentActionList=class(TActionList)
  public
    Doc: PAbstractDocument;
    constructor Create(owner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): boolean; override;
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

TNewProjectAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: boolean; override;
  end;

TOpenProjectAction=class(TAbstractDocumentAction)
  private
    fOpenDialog: TOpenDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OpenDialog: TOpenDialog read fOpenDialog;
  end;
TSaveProjectAsAction=class(TAbstractDocumentAction)
  private
    fSaveDialog: TSaveDialog;
    procedure CheckExistingFile(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property SaveDialog: TSaveDialog read fSaveDialog;
  end;

TSaveProjectAction=class(TSaveProjectAsAction)
  public
    constructor Create(AOwner: TComponent); override; //������ ��������
    procedure ExecuteTarget(Target: TObject); override; //������ �� �����. ���� ���� ����
    function Update: boolean; override; //���������� ������ ��������, ���� ��� ���.
  end;

procedure Register;

implementation

uses menus,forms,windows,sysutils;

procedure Register;
begin
  RegisterActions('AbstractDocumentActions',[TNewProjectAction,TOpenProjectAction,TSaveProjectAction,TSaveProjectAsAction],nil);
  RegisterComponents('CautiousEdit',[TAbstractDocumentActionList]);
end;

(*
      TAbstractDocumentActionList
                                        *)
constructor TAbstractDocumentActionList.Create(owner: TComponent);
begin
  inherited Create(owner);
  doc:=nil;
end;

function TAbstractDocumentActionList.ExecuteAction(Action: TBasicAction): boolean;
var Handled: Boolean;
begin
  if Assigned(onExecute) then begin
    onExecute(Action, Handled);
    Result:=Handled;
  end
  else begin
    //���� ���� doc^=nil, ������ ���������, HandlesTarget ��� ��������� ��������
    //�� ����� ����, ��� doc=nil, ��� ���������� � ����, ��� ����� ���������
    if Assigned(doc) and Action.HandlesTarget(doc^) then begin
      Action.ExecuteTarget(doc^);
      Result:=true;
    end
    else
      Result:=false;
  end;
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
//��� ����� ����� ���������� ����� �� ����, ����� ��������, �� ���� ��
//��������� ����. ���������� ��� ��� ���-������ � ���� ����
  doc:=getDoc;
  Enabled:=Assigned(doc);
  Result:=true; //�� ���� �� �������� ��� ��� ������ � ������ ������ �� ����
//���� ����������� �������� ������, ����� ���������� � �������� ���������
end;

(*
        TNewProjectAction
                                *)
constructor TNewProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='����� ������';
  Hint:='����� ������|������� ��� ������������� ������ ������� ������� � �������� �����';
  ShortCut:=TextToShortcut('Ctrl+N');
end;

procedure TNewProjectAction.ExecuteTarget(Target: TObject);
var doc_class: TAbstractDocumentClass;
    doc: TAbstractDocument;
begin
  //���� ��� ������� �������, ������ ����� �������: Target ��������� �� TAbstractDocument
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('��� �������� � ������� ��������� ����� ��������. ����������?','����� ������',MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);
    doc.Free;
    (ActionList as TAbstractDocumentActionList).Doc^:=doc_class.Create(nil);
    //����� �������� ����� ��� �� ����, ���� �����������, ��� �����
  end;
end;

function TNewProjectAction.Update: boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and (not doc.isEmpty);
  Result:=true;
end;

(*
        TOpenProjectAction
                                *)
constructor TOpenProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOpenDialog:=TOpenDialog.Create(self);
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.SetSubComponent(true);
  fOpenDialog.DefaultExt:='txt';
  fOpenDialog.Filter:='��������� ������|*.txt|�������� ������|*.dat|��� �����|*.*';
  Caption:='������� ������';
  Hint:='������� ������|������� ��� ������������� ������ ������� ������� � ��������� �����';
  ShortCut:=TextToShortcut('Ctrl+O');
end;

destructor TOpenProjectAction.Destroy;
begin
  fOpenDialog.Free;
  inherited Destroy;
end;

procedure TOpenProjectAction.ExecuteTarget(Target: TObject);
var tmp,doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('��� ������������� �������� � ������� ��������� ����� ��������. ����������?','������� ������',MB_YesNo)=IDYes) then begin
    if fOpenDialog.Execute then begin
      doc_class:=TAbstractDocumentClass(doc.ClassType);
      tmp:=doc_class.LoadFromFile(fOpenDialog.FileName);
      //���� ���� ������������, �� ���� ����� ���������� ���������
      //������ ������ ��������� �����������
      //���� �� ���������� ������������, ������ ����������� ��� ����
      //��������� ������ ������
      doc.Free;
      //�������� ������
      (ActionList as TAbstractDocumentActionList).Doc^:=tmp;
    end;
  end;
end;

(*
        TSaveProjectAsAction
                                  *)
constructor TSaveProjectAsAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSaveDialog:=TSaveDialog.Create(self);
  fSaveDialog.Name:='SaveDialog';
  fSaveDialog.SetSubComponent(true);
  fSaveDialog.DefaultExt:='txt';
  fSaveDialog.Filter:='��������� ������|*.txt|�������� ������|*.dat|��� �����|*.*';
  fSaveDialog.OnCanClose:=CheckExistingFile;
  Caption:='��������� ������ ���...';
  Hint:='��������� ������ ���...|��������� ������ ��� ����� ������';
end;

destructor TSaveProjectAsAction.Destroy;
begin
  fSaveDialog.Free;
  inherited Destroy;
end;

procedure TSaveProjectAsAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if fSaveDialog.Execute then begin
    doc.FileName:=fSaveDialog.filename;
    doc.Save;
  end;
end;

procedure TSaveProjectAsAction.CheckExistingFile(Sender: TObject; var CanClose: Boolean);
var FileName: string;
    doc_class: TAbstractDocumentClass;
    tmp: TAbstractDocument;
begin
  //������������ ������ ���� ��� ����������, �� ����� ���������, �� ������ �� �� ��� ����������
  FileName:=(Sender as TSaveDialog).FileName;
  if FileExists(FileName) then begin
    //��������� ��� �������, ��� ���
    doc_class:=TAbstractDocumentClass((ActionList as TAbstractDocumentActionList).doc^.ClassType);
    try
      tmp:=doc_class.LoadFromFile(FileName);
      //��������, �� ������� ��������
      //����� ��������� �������, ����������� ���������, ��� ��� ������ ������ ������ � ���� ��
    finally

    end;
  end;
end;

(*
      TSaveProjectAction
                              *)
constructor TSaveProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='��������� ������ |��������� ������ ��� ��� �� ������';
  ShortCut:=TextToShortcut('Ctrl+S');
end;

procedure TSaveProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if doc.FileName='' then inherited ExecuteTarget(Target)
  else  doc.Save;
end;

function TSaveProjectAction.Update: Boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and doc.Changed;
  Result:=true;
end;

end.
