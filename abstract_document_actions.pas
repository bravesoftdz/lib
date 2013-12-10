unit abstract_document_actions;

interface

uses actnlist,command_class_lib,classes;

type

TAbstractDocumentClass=class of TAbstractDocument;

TAbstractDocumentAction=class(TCustomAction)
  private
    fDoc: TAbstractDocument;
    procedure SetDoc(value: TAbstractDocument);
  protected
    function GetDoc: TAbstractDocument;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property Caption;
end;

TNewProjectAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
end;

procedure Register;

implementation

uses menus,forms,windows;

procedure Register;
begin
  RegisterActions('AbstractDocumentActions',[TNewProjectAction],nil);
end;

(*
        TAbstractDocumentAction
                                      *)
destructor TAbstractDocumentAction.Destroy;
begin
//����� ��������� ���������� ������ �� ��� ����������, ���� �����
//�������� ����������� ������. �������, ��� ���� ��.
  if Assigned(fDoc) then
    fDoc.RemoveFreeNotification(self);
    inherited Destroy;
end;

function TAbstractDocumentAction.GetDoc: TAbstractDocument;
begin
//����� ��������� ��� � "�������� �����", � �� ������ ������� �� ������
//�� ����, ������ �� ��� ��� �����
  if Assigned(fDoc) then Result:=fDoc
  else Result:=nil;
end;

function TAbstractDocumentAction.HandlesTarget(Target: TObject): Boolean;
begin
//��������� �� ������� ��� ���� �� ����� ���������, �� ����� ��������
  Result:=Assigned(fDoc);
end;

procedure TAbstractDocumentAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
//���� �������� ���������������, �� ��� ����������� � �� ������� ������ �� ����
  inherited Notification(AComponent, Operation);
  if (operation=opRemove) and (AComponent=fDoc) then
    fDoc:=nil;
end;

procedure TAbstractDocumentAction.SetDoc(value: TAbstractDocument);
begin
//���, ������ ��������, �� ������� ���������, ����� �� ��� ��������
//� ����� �����������
  if fDoc<>value then begin
    if Assigned(fDoc) then
      fDoc.RemoveFreeNotification(self);
    fDoc:=value;
    if Assigned(fDoc) then
      fDoc.FreeNotification(self);
  end;
end;

procedure TAbstractDocumentAction.UpdateTarget(Target: TObject);
begin
//��� ����� ����� ���������� ����� �� ����, ����� ��������, �� ���� ��
//��������� ����. ���������� ��� ��� ���-������ � ���� ����
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
var doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=GetDoc;
  if (not doc.Changed) or (Application.MessageBox('��� �������� � ������� ��������� ����� ��������. ����������?','����� ������',MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);
    doc.Free;
    doc:=doc_class.Create(nil);
    SetDoc(doc);
  end;
end;

end.
