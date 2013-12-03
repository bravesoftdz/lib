unit PersistentStringGridRes;

interface

uses grids,classes,DesignEditors,DesignIntf,controls;

type

TPersistentStringEditor=class(TComponentEditor)
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
end;

Procedure Register;

implementation

uses PersistentStringGridEditor;

constructor TPersistentStringEditor.Create(AComponent: TComponent;ADesigner: IDesigner);
begin
  inherited Create(AComponent,ADesigner);
  PersistentStringGridEditorForm:=TPersistentStringGridEditorForm.Create(nil);
end;

destructor TPersistentStringEditor.Destroy;
begin
  PersistentStringGridEditorForm.Free;
  inherited Destroy;
end;

procedure TPersistentStringEditor.Edit;
begin
  PersistentStringGridEditorForm.subj.Assign(Component);
  if PersistentStringGridEditorForm.ShowModal=mrOK then
    Component.Assign(PersistentStringGridEditorForm.subj);
end;

Procedure Register;
begin
  RegisterComponents('CautiousEdit',[TPersistentStringGrid]);
  RegisterComponentEditor(TPersistentStringGrid,TPersistentStringEditor);
end;

end.
