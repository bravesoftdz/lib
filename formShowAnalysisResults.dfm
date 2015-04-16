object frmShowAnalysisResults: TfrmShowAnalysisResults
  Left = 287
  Top = 126
  Width = 593
  Height = 350
  Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090#1099' '#1084#1086#1076#1077#1083#1080#1088#1086#1074#1072#1085#1080#1103
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 291
    Width = 585
    Height = 25
    Panels = <
      item
        Width = 100
      end
      item
        Width = 200
      end
      item
        Width = 100
      end>
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 585
    Height = 291
    Align = alClient
    TabOrder = 1
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = 'bmp'
    Left = 240
    Top = 24
  end
end
