object Form1: TForm1
  Left = 274
  Top = 309
  Caption = 'Form1'
  ClientHeight = 681
  ClientWidth = 1375
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
    Left = 1051
    Top = 56
    Width = 307
    Height = 481
    OnMouseDown = img1MouseDown
  end
  object lblTextToGraph: TLabel
    Left = 1056
    Top = 32
    Width = 209
    Height = 13
    Caption = 'pick neurons and connections by mouseclick'
  end
  object mmoEvent: TMemo
    Left = 252
    Top = 344
    Width = 329
    Height = 193
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object grpXor: TGroupBox
    Left = 8
    Top = 32
    Width = 217
    Height = 105
    Caption = 'Xor'
    TabOrder = 1
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
    Width = 217
    Height = 153
    Caption = 'Cascade'
    TabOrder = 2
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
    Left = 336
    Top = 569
    Width = 75
    Height = 25
    Caption = 'Break'
    TabOrder = 3
  end
  object lvNeuron: TListView
    Left = 612
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
    TabOrder = 4
    ViewStyle = vsReport
  end
  object lvCon: TListView
    Left = 612
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
    TabOrder = 5
    ViewStyle = vsReport
  end
  object grpDraw: TGroupBox
    Left = 8
    Top = 335
    Width = 217
    Height = 114
    Caption = 'Draw'
    TabOrder = 6
    object lblLine: TLabel
      Left = 114
      Top = 60
      Width = 47
      Height = 13
      Caption = 'LineWidth'
    end
    object btnDrawpartial: TButton
      Left = 16
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Draw partial '
      TabOrder = 0
      OnClick = btnDrawpartialClick
    end
    object edtLine: TSpinEdit
      Left = 114
      Top = 79
      Width = 75
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 2
    end
    object chkWeights: TCheckBox
      Left = 114
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Draw Weights'
      TabOrder = 2
    end
    object btnDraw: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Draw'
      TabOrder = 3
      OnClick = btnDrawClick
    end
  end
  object mmo1: TMemo
    Left = 252
    Top = 32
    Width = 329
    Height = 289
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object grpChangeCon: TGroupBox
    Left = 8
    Top = 455
    Width = 217
    Height = 105
    Caption = 'Change Connection'
    TabOrder = 8
    object lblCon1: TLabel
      Left = 16
      Top = 16
      Width = 50
      Height = 13
      Caption = 'Con Index'
    end
    object lblNewWeight: TLabel
      Left = 17
      Top = 56
      Width = 58
      Height = 13
      Caption = 'New Weight'
    end
    object edtConIndex: TEdit
      Left = 16
      Top = 32
      Width = 66
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object edtNewWeight: TEdit
      Left = 16
      Top = 72
      Width = 66
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object btnNewWeight: TButton
      Left = 104
      Top = 32
      Width = 85
      Height = 25
      Caption = 'Set New Weight'
      TabOrder = 2
      OnClick = btnNewWeightClick
    end
  end
  object grpChangeNeuron: TGroupBox
    Left = 8
    Top = 569
    Width = 217
    Height = 105
    Caption = 'Change Neuron'
    TabOrder = 9
    object lblNeuronIndex: TLabel
      Left = 5
      Top = 24
      Width = 102
      Height = 13
      Caption = 'Neuron L-indx,N-indx'
    end
    object lblNewSteep: TLabel
      Left = 17
      Top = 64
      Width = 58
      Height = 13
      Caption = 'New Weight'
    end
    object lblNewFunc: TLabel
      Left = 109
      Top = 66
      Width = 93
      Height = 13
      Caption = 'New FunctionIndex'
    end
    object edtNeuronIndex: TEdit
      Left = 16
      Top = 40
      Width = 66
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object btnNewNeuron: TButton
      Left = 112
      Top = 32
      Width = 85
      Height = 25
      Caption = 'Set New Param'
      TabOrder = 1
      OnClick = btnNewNeuronClick
    end
    object edtNewSteep: TEdit
      Left = 16
      Top = 80
      Width = 66
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object edtNewFunc: TEdit
      Left = 108
      Top = 80
      Width = 66
      Height = 21
      TabOrder = 3
      Text = '1'
    end
  end
end
