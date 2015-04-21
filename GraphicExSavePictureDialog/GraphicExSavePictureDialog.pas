unit GraphicExSavePictureDialog;

interface

uses
  SysUtils, Classes, Dialogs, ExtDlgs, graphics,controls,types, JPEG, buttons,
  extCtrls,pngImage;

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
      procedure DoClose; override;
  public
    { Public declarations }
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
      procedure DoSelectionChange; override;
      procedure ReloadPicture;
      procedure Save;
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


  TGraphicExPNG = class(TPNGObject, IGraphicPreferences)
    public
      procedure ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
  end;

  TGraphicExBMP = class(TBitmap, IGraphicPreferences)
    protected
      procedure Monochromechange(Sender: TObject);
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
    while i<CheatPaintPanel.ComponentCount do
      if CheatPaintPanel.Components[i].Tag<>FilterIndex then
        CheatPaintPanel.Components[i].Free
      else
        inc(i);
    if FGraphicToCompress.GetInterface(IGraphicPreferences,intf) then begin
      intf.ShowPreferences(CheatPaintPanel,PrefRect,FilterIndex);
    end;

  end;



  inherited DoTypeChange;
end;

resourcestring
  KiBcaption = ' КиБ';

procedure TGraphicExSavePictureDialog.ReloadPicture;
begin
    FMemoryStream.Clear;
    FGraphicToCompress.SaveToStream(FMemoryStream);
    PictureLabel.Caption:=IntToStr(FMemoryStream.Size div 1024)+KiBcaption;
    FMemoryStream.Seek(0,soFromBeginning);
    ImageCtrl.Picture.Graphic:=FGraphicToCompress;
    ImageCtrl.Picture.Graphic.LoadFromStream(FMemoryStream);
end;

procedure TGraphicExSavePictureDialog.DoShow;
begin
  inherited DoShow;
  DoTypeChange;
end;

procedure TGraphicExSavePictureDialog.Save;
var FileStream: TFileStream;
begin
  if Execute then begin
    FileStream:=TFileStream.Create(FileName,fmCreate);
    try
      FMemoryStream.Seek(0,soFromBeginning);
      FileStream.CopyFrom(FMemoryStream,FMemoryStream.Size);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TGraphicExSavePictureDialog.DoClose;
var i: Integer;
begin
  i:=0;
  while i<CheatPaintPanel.ComponentCount do
    if CheatPaintPanel.Components[i].Tag<>0 then
      CheatPaintPanel.Components[i].Free
    else
      inc(i);
  inherited DoClose;
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
var labl: TLabel;
    master: TGraphicExSavePictureDialog;
begin
  CompressionQuality:=(Sender as TTrackBar).Position;
  labl:=(Sender as TTrackBar).Owner.FindComponent('lblCompressionRate') as TLabel;

  master:=(Sender as TTrackBar).Owner.Owner as TGraphicExSavePictureDialog;
  Assign(master.FGraphic);
  Compress;
  labl.Caption:=JPEGCompressionCaption+IntToStr(CompressionQuality);

  master.ReloadPicture;
end;

(*
    TGraphicExPNG
                      *)
procedure TGraphicExPNG.ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
begin
  //we want maximum compression we can get (it's lossless, so no need to ask)
  filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  (control.Owner as TGraphicExSavePictureDialog).ReloadPicture;
end;

(*
    TGraphicExBMP
                      *)
resourcestring
  strIsMonochrome='Черно-белое';

procedure TGraphicExBMP.ShowPreferences(control: TWinControl; var aBoundsRect: TRect; aFilterIndex: Integer);
var chkBox: TCheckBox;
begin
  chkBox:=TCheckBox.Create(control);
  with chkBox do begin
    name:='chkMonochrome';
    Parent:=control;
    Align:=alTop;
    Tag:=aFilterIndex;
    caption:=strIsMonochrome;
    OnClick:=MonochromeChange;
  end;
end;

procedure TGraphicExBMP.Monochromechange(Sender: TObject);
begin
  Monochrome:=(Sender as TCheckBox).Checked;
  ((Sender as TCheckBox).Owner.Owner as TGraphicExSavePictureDialog).ReloadPicture;
end;

initialization
  with FileFormatList do begin
    RegisterFileFormat('jpg',gesJPGImages,'',[ftRaster,ftEnableSaving], true, False, TGraphicExJPG);
    RegisterFileFormat('jfif',gesJPGImages,'',[ftRaster,ftEnableSaving], true, False, TGraphicExJPG);
    RegisterFileFormat('jpe',gesJPEImages,'',[ftRaster,ftEnableSaving], true, False, TGraphicExJPG);
    RegisterFileFormat('jpeg',gesJPEGImages,'',[ftRaster,ftEnableSaving], true, False, TGraphicExJPG);
    RegisterFileFormat('png', gesPortableNetworkGraphic, '', [ftRaster,ftEnableSaving], True, True, TGraphicExPNG);
    RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster,ftEnableSaving], true, False, TGraphicExBMP);
  end;
end.
