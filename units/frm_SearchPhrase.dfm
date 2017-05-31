object frmSearchPhrase: TfrmSearchPhrase
  Left = 627
  Top = 409
  Width = 352
  Height = 246
  Caption = 'frmSearchPhrase'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Themed = False
  OnCreate = EasyTaskPanelFormCreate
  OnShow = EasyTaskPanelFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 212
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object reText: TRichEdit
      Left = 8
      Top = 8
      Width = 328
      Height = 160
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = reTextChange
    end
    object Panel2: TPanel
      Left = 8
      Top = 168
      Width = 328
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      BorderWidth = 8
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      DesignSize = (
        328
        36)
      object btnMaskHelp: TSpeedButton
        Left = 304
        Top = 4
        Width = 17
        Height = 17
        Anchors = [akTop, akRight]
        Caption = '?'
        Enabled = False
        Flat = True
        Visible = False
      end
      object cbUseMask: TCheckBox
        Left = 0
        Top = 19
        Width = 89
        Height = 17
        Caption = 'Maskowanie'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbUseMaskClick
      end
      object cbCaseSens: TCheckBox
        Left = 0
        Top = 3
        Width = 137
        Height = 17
        Caption = 'Uwzgl'#281'dnij wielko'#347#263' liter'
        TabOrder = 1
        OnClick = cbCaseSensClick
      end
    end
  end
end
