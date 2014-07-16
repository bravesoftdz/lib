object PersistentStringGridEditorForm: TPersistentStringGridEditorForm
  Left = 204
  Top = 655
  Width = 393
  Height = 215
  Caption = 'PersistentStringGrid editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    385
    188)
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 292
    Top = 150
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 150
    Width = 81
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    Kind = bkCancel
  end
  object subj: TStringGrid
    Left = 8
    Top = 8
    Width = 369
    Height = 137
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
  end
end
