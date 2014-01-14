unit formMergeOrRewrite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmMergeOrRewrite = class(TForm)
    lblInfo: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblSame: TLabel;
    lblMinus: TLabel;
    lblPlus: TLabel;
    Label7: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMergeOrRewrite: TfrmMergeOrRewrite;

implementation

{$R *.dfm}

end.
