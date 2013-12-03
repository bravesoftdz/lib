object PersistentStringGridEditorForm: TPersistentStringGridEditorForm
  Left = 342
  Top = 344
  Width = 393
  Height = 215
  Caption = 'PersistentStringGrid editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    365
    166)
  PixelsPerInch = 120
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 359
    Top = 185
    Width = 110
    Height = 30
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 10
    Top = 185
    Width = 100
    Height = 30
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    Kind = bkCancel
  end
  object subj: TStringGrid
    Left = 10
    Top = 10
    Width = 454
    Height = 168
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
  end
end
