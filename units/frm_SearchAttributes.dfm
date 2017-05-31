object frmSearchAttributes: TfrmSearchAttributes
  Left = 608
  Top = 125
  Width = 217
  Height = 115
  Caption = 'frmSearchAttributes'
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
    Width = 209
    Height = 88
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      209
      88)
    object cbAttribA: TCheckBox
      Left = 128
      Top = 7
      Width = 73
      Height = 17
      AllowGrayed = True
      Anchors = [akTop, akRight]
      Caption = 'Archiwalny'
      State = cbGrayed
      TabOrder = 0
      WordWrap = True
      OnClick = cbAttribRClick
    end
    object cbAttribR: TCheckBox
      Left = 8
      Top = 7
      Width = 105
      Height = 17
      AllowGrayed = True
      Caption = 'Tylko do odczytu'
      State = cbGrayed
      TabOrder = 1
      WordWrap = True
      OnClick = cbAttribRClick
    end
    object cbAttribS: TCheckBox
      Left = 8
      Top = 31
      Width = 73
      Height = 17
      AllowGrayed = True
      Caption = 'Systemowy'
      State = cbGrayed
      TabOrder = 2
      WordWrap = True
      OnClick = cbAttribRClick
    end
    object cbAttribH: TCheckBox
      Left = 128
      Top = 31
      Width = 65
      Height = 17
      AllowGrayed = True
      Anchors = [akTop, akRight]
      Caption = 'Ukryty'
      State = cbGrayed
      TabOrder = 3
      WordWrap = True
      OnClick = cbAttribRClick
    end
  end
end
