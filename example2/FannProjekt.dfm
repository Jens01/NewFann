object Form1: TForm1
  Left = 274
  Top = 309
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 1443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 1128
    Top = 40
    Width = 307
    Height = 497
  end
  object lblLine: TLabel
    Left = 15
    Top = 384
    Width = 47
    Height = 13
    Caption = 'LineWidth'
  end
  object mmo1: TMemo
    Left = 208
    Top = 32
    Width = 441
    Height = 297
    ScrollBars = ssVertical
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
  object grpXor: TGroupBox
    Left = 8
    Top = 32
    Width = 177
    Height = 105
    Caption = 'Xor'
    TabOrder = 2
    object btnTrain: TButton
      Left = 7
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Train'
      TabOrder = 0
      OnClick = btnTrainClick
    end
    object btnExec: TButton
      Left = 7
      Top = 63
      Width = 75
      Height = 25
      Caption = 'Exec'
      TabOrder = 1
      OnClick = btnExecClick
    end
    object edtError: TEdit
      Left = 88
      Top = 28
      Width = 75
      Height = 21
      TabOrder = 2
      Text = 'edtError'
    end
  end
  object Cascade: TGroupBox
    Left = 8
    Top = 176
    Width = 185
    Height = 153
    Caption = 'Cascade'
    TabOrder = 3
    object btnCascade: TButton
      Left = 7
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Cascade'
      TabOrder = 0
      OnClick = btnCascadeClick
    end
    object btnCTrain: TButton
      Left = 7
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Train'
      TabOrder = 1
      OnClick = btnCTrainClick
    end
    object btnCExec: TButton
      Left = 7
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Exec'
      TabOrder = 2
      OnClick = btnCExecClick
    end
    object edtCError: TEdit
      Left = 88
      Top = 20
      Width = 75
      Height = 21
      TabOrder = 3
      Text = 'edtError'
    end
    object btnTest: TButton
      Left = 7
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 4
      OnClick = btnTestClick
    end
  end
  object btnBreak: TButton
    Left = 32
    Top = 496
    Width = 75
    Height = 25
    Caption = 'Break'
    TabOrder = 4
  end
  object lvNeuron: TListView
    Left = 680
    Top = 32
    Width = 420
    Height = 289
    Columns = <
      item
        Caption = 'Name'
      end
      item
        Caption = 'Layer'
      end
      item
        Caption = 'Neuron'
        Width = 40
      end
      item
        AutoSize = True
        Caption = 'Function'
        MinWidth = 125
      end
      item
        Caption = 'steepness'
        MinWidth = 60
        Width = 60
      end
      item
        Caption = 'Bias'
      end>
    TabOrder = 5
    ViewStyle = vsReport
  end
  object lvCon: TListView
    Left = 680
    Top = 344
    Width = 420
    Height = 193
    Columns = <
      item
        Caption = 'Name'
        MinWidth = 50
      end
      item
        Caption = 'Neuron 1'
        MinWidth = 60
        Width = 60
      end
      item
        Caption = 'Neuron 2'
        MinWidth = 60
        Width = 60
      end
      item
        Alignment = taRightJustify
        Caption = 'Weight'
        MinWidth = 60
        Width = 60
      end>
    TabOrder = 6
    ViewStyle = vsReport
  end
  object edtLine: TSpinEdit
    Left = 15
    Top = 407
    Width = 75
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 2
  end
end
