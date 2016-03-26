unit Change_Language_action;

interface

uses ActnList;

type
  TChangeLanguageAction = class(TCustomAction)
  private
    fUpdated: boolean;
  public
    function Update: Boolean; override;
  published
    property Caption;
    property Hint;
  end;

implementation

uses set_english_locale_if_not_sure;

function TChangeLanguageAction.Update: Boolean;
begin
  if not fUpdated then begin
    



  end;
end;

end.
