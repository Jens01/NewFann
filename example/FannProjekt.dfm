object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object mmo1: TMemo
    Left = 208
    Top = 32
    Width = 441
    Height = 297
    TabOrder = 0
  end
  object mmoEvent: TMemo
    Left = 208
    Top = 360
    Width = 441
    Height = 145
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnTrain: TButton
    Left = 24
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Train'
    TabOrder = 2
    OnClick = btnTrainClick
  end
  object btnExec: TButton
    Left = 24
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Exec'
    TabOrder = 3
    OnClick = btnExecClick
  end
  object edtError: TEdit
    Left = 24
    Top = 208
    Width = 75
    Height = 21
    TabOrder = 4
    Text = 'edtError'
  end
end
