object FrameHistory: TFrameHistory
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    OnPaint = FormPaint
  end
end
