object frmSearchSize: TfrmSearchSize
  Left = 650
  Top = 131
  Width = 377
  Height = 153
  Caption = 'frmSearchSize'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Themed = False
  OnCreate = EasyTaskPanelFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 126
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object seSizeTo: TSpinEdit
      Left = 56
      Top = 34
      Width = 65
      Height = 22
      Hint = 'Maksymalny rozmiar pliku'
      MaxValue = 10240
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object seSizeFrom: TSpinEdit
      Left = 56
      Top = 8
      Width = 65
      Height = 22
      Hint = 'Minimalny rozmiar pliku'
      MaxValue = 10240
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object cbSizeTo: TCheckBox
      Left = 8
      Top = 38
      Width = 39
      Height = 17
      Caption = 'Do'
      TabOrder = 2
      OnClick = cbSizeFromClick
    end
    object cbSizeFrom: TCheckBox
      Left = 8
      Top = 12
      Width = 39
      Height = 17
      Caption = 'Od'
      TabOrder = 3
      OnClick = cbSizeFromClick
    end
    object cbbSizeTo: TComboBox
      Left = 136
      Top = 35
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 4
      Text = 'kB'
      Items.Strings = (
        'B'
        'kB'
        'MB')
    end
    object cbbSizeFrom: TComboBox
      Left = 136
      Top = 7
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 5
      Text = 'kB'
      Items.Strings = (
        'B'
        'kB'
        'MB')
    end
  end
end
