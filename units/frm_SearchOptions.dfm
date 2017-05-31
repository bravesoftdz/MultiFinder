object frmSearchOptions: TfrmSearchOptions
  Left = 368
  Top = 162
  Width = 332
  Height = 183
  Caption = 'frmSearchOptions'
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
  object Label1: TLabel
    Left = 9
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Maks. ilo'#347#263' podfolder'#243'w:'
  end
  object seSubFolders: TSpinEdit
    Left = 137
    Top = 4
    Width = 49
    Height = 22
    MaxValue = 999
    MinValue = 0
    TabOrder = 0
    Value = 99
    OnChange = cbHiddenFoldersClick
  end
  object cbHiddenFolders: TCheckBox
    Left = 8
    Top = 32
    Width = 209
    Height = 17
    Caption = 'Przeszukuj katalogi ukryte i systemowe'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = cbHiddenFoldersClick
  end
  object cbHiddenFiles: TCheckBox
    Left = 8
    Top = 56
    Width = 209
    Height = 17
    Caption = 'Uwzgl'#281'dniaj pliki ukryte i systemowe'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = cbHiddenFoldersClick
  end
end
