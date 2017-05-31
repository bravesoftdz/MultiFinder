object frmSearchDate: TfrmSearchDate
  Left = 761
  Top = 375
  Width = 259
  Height = 135
  Caption = 'frmSearchDate'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = EasyTaskPanelFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 251
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object cbDateFrom: TCheckBox
      Left = 8
      Top = 12
      Width = 39
      Height = 17
      Caption = 'Od'
      TabOrder = 0
      OnClick = cbDateFromClick
    end
    object dtpDateFrom: TDateTimePicker
      Left = 56
      Top = 8
      Width = 73
      Height = 21
      Hint = 'Pocz'#261'tkowa data'
      Date = 39842.362290219910000000
      Format = 'dd-MM-yy'
      Time = 39842.362290219910000000
      TabOrder = 1
    end
    object dtpTimeFrom: TDateTimePicker
      Left = 136
      Top = 8
      Width = 73
      Height = 21
      Hint = 'Pocz'#261'tkowy czas'
      Date = 39842.362290219910000000
      Time = 39842.362290219910000000
      Kind = dtkTime
      TabOrder = 2
    end
    object cbDateTo: TCheckBox
      Left = 8
      Top = 38
      Width = 39
      Height = 17
      Caption = 'Do'
      TabOrder = 3
      OnClick = cbDateFromClick
    end
    object dtpDateTo: TDateTimePicker
      Left = 56
      Top = 34
      Width = 73
      Height = 21
      Hint = 'Pocz'#261'tkowa data'
      Date = 39842.362290219910000000
      Format = 'dd-MM-yy'
      Time = 39842.362290219910000000
      TabOrder = 4
    end
    object dtpTimeTo: TDateTimePicker
      Left = 136
      Top = 34
      Width = 73
      Height = 21
      Hint = 'Pocz'#261'tkowy czas'
      Date = 39842.362290219910000000
      Time = 39842.362290219910000000
      Kind = dtkTime
      TabOrder = 5
    end
  end
end
