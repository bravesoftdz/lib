unit PersistentStringGridEditor;
{$ObjExportAll On}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, PersistentStringGrid, StdCtrls, Buttons;

type
  TPersistentStringGridEditorForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    subj: TStringGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PersistentStringGridEditorForm: TPersistentStringGridEditorForm;

implementation

{$R *.dfm}

end.
