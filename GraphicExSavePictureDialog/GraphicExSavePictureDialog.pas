unit GraphicExSavePictureDialog;

interface

uses
  SysUtils, Classes, Dialogs, ExtDlgs, graphics,controls,types, JPEG, buttons,extCtrls;

type

  TGraphicExSavePictureDialog = class(TSavePictureDialog)
  private
    { Private declarations }
  protected
    { Protected declarations }
      FGraphic: TGraphic;
      FGraphicToCompress: TGraphic;
      //paparazzi: stealing privacy from these controls
      //by FindComponent(name) - you can't hide if I know your name!
      CheatPreviewButton: TSpeedButton;
      CheatPicturePanel: TPanel;
      CheatPaintPanel: TPanel;
      ext: string;
      FMemoryStream: TMemoryStream;

      procedure DoTypeChange; override;
      procedure DoShow; override;
  public
    { Public declarations }
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
      procedure DoSelectionChange; override;
      procedure ReloadPicture;
    //graphic is not owned by SavePictureDialog, property is just a reference to it
      property GraphicToSave: TGraphic read FGraphic write FGraphic;
  published
    { Published declarations }
  end;

  IGraphicPreferences = interface
  ['{36F23C04-76C9-4CDF-AA7D-FA7A556C8DF1}']
    procedure ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
  end;

  TGraphicExJPG = class(TJPEGImage, IGraphicPreferences)
    protected
      procedure CompressionRateChange(Sender: TObject);
    public
      procedure ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
  end;

procedure Register;

implementation

uses GraphicEx,comCtrls,graphicStrings,stdCtrls;

procedure Register;
begin
  RegisterComponents('Nabbla', [TGraphicExSavePictureDialog]);
end;

constructor TGraphicExSavePictureDialog.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fMemoryStream:=TMemoryStream.Create;
  CheatPreviewButton:=FindComponent('PreviewButton') as TSpeedButton;
  CheatPicturePanel:=FindComponent('PicturePanel') as TPanel;
  CheatPaintPanel:=FindComponent('PaintPanel') as TPanel;
end;

destructor TGraphicExSavePictureDialog.Destroy;
begin
  fMemoryStream.Free;
  inherited Destroy;
end;

procedure TGraphicExSavePictureDialog.DoSelectionChange;
begin
  //we don't want to show picture which is in file we want to overwrite
  //so we don't care at all about selection change, except event handling
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TGraphicExSavePictureDialog.DoTypeChange;
var i,j,k,counter: Integer;
    intf: IGraphicPreferences;
    PrefRect: TRect;
begin
//  PictureLabel.Caption:=IntToStr(FilterIndex);
  counter:=1; //т.е идет 1-й интервал
  j:=0;
  for i:=1 to Length(Filter) do
    if Filter[i]='|' then begin
      inc(counter);
      if counter=2*FilterIndex then begin
        j:=i;
        break;
      end;
    end;
  if j=0 then Exit;
  k:=0;
  for i:=j+1 to Length(Filter) do
    if Filter[i]='|' then begin
      k:=i;
      break;
    end;
  if k=0 then k:=Length(Filter)+1;

  ext:=Copy(Filter,j+3,k-j-3);

  CheatPreviewButton.Enabled:=Assigned(FGraphic);
  if Assigned(FGraphic) then begin
//    ImageCtrl.Picture.Graphic:=ConvertToGraphicType(FGraphic,ext);
    FGraphicToCompress.Free;
    FGraphicToCompress:=ConvertToGraphicType(FGraphic,ext);
    ReloadPicture;
    i:=0;
    while i<CheatPicturePanel.ComponentCount do
      if CheatPicturePanel.Components[i].Tag<>FilterIndex then
        CheatPicturePanel.Components[i].Free
      else
        inc(i);
    if FGraphicToCompress.GetInterface(IGraphicPreferences,intf) then begin
      PrefRect:=CheatPicturePanel.ClientRect;
      PrefRect.Top:=30;
      intf.ShowPreferences(CheatPaintPanel,PrefRect,FilterIndex);
    end;

  end;



  inherited DoTypeChange;
end;

procedure TGraphicExSavePictureDialog.ReloadPicture;
var i: Integer;
begin
    FMemoryStream.Clear;
//    if FGraphicToCompress is TGraphicExJPG then
//      TGraphicExJPG(FGraphicToCompress).CompressionQuality:=1;
    FGraphicToCompress.SaveToStream(FMemoryStream);
    PictureLabel.Caption:=IntToStr(FMemoryStream.Size div 1024)+' KiB';
    FMemoryStream.Seek(0,soFromBeginning);
    ImageCtrl.Picture.Graphic:=FGraphicToCompress;
    ImageCtrl.Picture.Graphic.LoadFromStream(FMemoryStream);
end;

procedure TGraphicExSavePictureDialog.DoShow;
begin
  inherited DoShow;
  DoTypeChange;
end;


(*
      TGraphicExJPG
                        *)
resourceString
   JPEGCompressionCaption = 'Качество: ';

procedure TGraphicExJPG.ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
var labl: TLabel;
begin
  labl:=TLabel.Create(control);
  with labl do begin
    name:='lblCompressionRate';
    Parent:=control;
    Align:=alTop;
    Caption:=JPEGCompressionCaption;
    Tag:=aFilterIndex;
  end;
  with TTrackBar.Create(control) do begin
    Parent:=control;
    Top:=labl.Top+labl.Height;
    Align:=alTop;
    Min:=1;
    Max:=100;
    Tag:=aFilterIndex;
    aBoundsRect.Bottom:=aBoundsRect.Top+height;
    onChange:=CompressionRateChange;
    Position:=80;
  end;
end;

procedure TGraphicExJPG.CompressionRateChange(Sender: TObject);
var val: Integer;
    labl: TLabel;
    master: TGraphicExSavePictureDialog;
begin
  val:=(Sender as TTrackBar).Position;
  labl:=(Sender as TTrackBar).Owner.FindComponent('lblCompressionRate') as TLabel;

  master:=(Sender as TTrackBar).Owner.Owner as TGraphicExSavePictureDialog;
  master.FGraphicToCompress.Assign(master.FGraphic);
  (master.FGraphicToCompress as TGraphicExJPG).CompressionQuality:=val;
  (master.ImageCtrl.Picture.Graphic as TGraphicExJPG).Compress;
//  labl.Caption:=JPEGCompressionCaption+IntToStr(val);
  labl.Caption:=JPEGCompressionCaption+IntToStr((master.FGraphicToCompress as TGraphicExJPG).compressionQuality);

  master.ReloadPicture;
end;

initialization
  FileFormatList.RegisterFileFormat('jpg',gesJPGImages,'',[ftRaster,ftEnableSaving], true, False, TGraphicExJPG);

end.
