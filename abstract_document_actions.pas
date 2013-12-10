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
//чтобы автоматом обнулилась ссылка на это безобразие, если вдруг
//действие уничтожится раньше. Впрочем, это вряд ли.
  if Assigned(fDoc) then
    fDoc.RemoveFreeNotification(self);
    inherited Destroy;
end;

function TAbstractDocumentAction.GetDoc: TAbstractDocument;
begin
//можно отправить его в "активный поиск", а не просто перейти по ссылке
//не знаю, сильно ли мне это нужно
  if Assigned(fDoc) then Result:=fDoc
  else Result:=nil;
end;

function TAbstractDocumentAction.HandlesTarget(Target: TObject): Boolean;
begin
//выполнима ли команда или пора ее сразу отключить, от греха подальше
  Result:=Assigned(fDoc);
end;

procedure TAbstractDocumentAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
//если документ самоуничтожился, он нас предупредит и мы выкинем ссылку на него
  inherited Notification(AComponent, Operation);
  if (operation=opRemove) and (AComponent=fDoc) then
    fDoc:=nil;
end;

procedure TAbstractDocumentAction.SetDoc(value: TAbstractDocument);
begin
//ага, просим документ, на который ссылаемся, чтобы он нас уведомил
//о своем уничтожении
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
//это место будет вызываться когда не лень, чтобы выяснить, не надо ль
//отключить элем. управления или еще что-нибудь в этом духе
end;

(*
        TNewProjectAction
                                *)
constructor TNewProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='Новый проект';
  Hint:='Новый проект|стирает все несохраненные данные старого проекта и начинает новый';
  ShortCut:=TextToShortcut('Ctrl+N');
end;

procedure TNewProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=GetDoc;
  if (not doc.Changed) or (Application.MessageBox('Все действия и история изменений будут потеряны. Продолжить?','Новый проект',MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);
    doc.Free;
    doc:=doc_class.Create(nil);
    SetDoc(doc);
  end;
end;

end.
