unit formShowAnalysisResults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, TeeProcs, TeEngine, Chart, ExtDlgs,
  Series;

type
  TfrmShowAnalysisResults = class(TForm)
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    SavePictureDialog1: TSavePictureDialog;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmShowAnalysisResults: TfrmShowAnalysisResults;

implementation
uses graphicEx;

{$R *.dfm}

procedure TfrmShowAnalysisResults.FormResize(Sender: TObject);
begin

  if PageControl1.ActivePageIndex>=0 then
    StatusBar1.Panels[0].Text:=IntToStr(PageControl1.Pages[PageControl1.ActivePageIndex].Width)+
      'x'+IntToStr(PageControl1.Pages[PageControl1.ActivePageIndex].Height);

end;

procedure TfrmShowAnalysisResults.FormCreate(Sender: TObject);
begin
  SavePictureDialog1.Filter:=FileFormatList.GetGraphicFilter([ftEnableSaving],fstNone,[foIncludeExtension],nil);
end;

procedure TfrmShowAnalysisResults.ChartMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var tmpX,tmpY: Double;
    chart: TChart;
begin
  chart:=Sender as TChart;
  if PtInRect(Chart.ChartRect,Point(X-Chart.Width3D,Y+Chart.Height3D)) and (chart.SeriesCount>0) then begin
    With chart.Series[0] do
    begin
      GetCursorValues(tmpX,tmpY);  { <-- get values under mouse cursor }
      StatusBar1.Panels[1].Text:=GetHorizAxis.LabelValue(tmpX);
      StatusBar1.Panels[2].Text:=GetVertAxis.LabelValue(tmpY);
    end;
  end
  else begin
    StatusBar1.Panels[1].Text:='';
    StatusBar1.Panels[2].Text:='';
  end;

end;


end.
