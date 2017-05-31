object frmPreview: TfrmPreview
  Left = 331
  Top = 216
  Width = 687
  Height = 542
  Caption = 'Podgl'#261'd'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormaShow
  Options.SaveOptions.Enabled = True
  Options.SaveOptions.RegPath = 'ZMsoft\Wyszukiwarka'
  Options.DoubleBuffered = True
  PixelsPerInch = 96
  TextHeight = 13
  object sBar: TJvStatusBar
    Left = 0
    Top = 498
    Width = 679
    Height = 17
    Panels = <>
    SimplePanel = True
  end
  object edt: TJvEditor
    Left = 0
    Top = 0
    Width = 679
    Height = 469
    Cursor = crIBeam
    GutterWidth = 50
    RightMarginVisible = False
    RightMargin = 120
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    SmartTab = False
    BracketHighlighting.Active = True
    BracketHighlighting.BorderColor = clPurple
    BracketHighlighting.StringEscape = #39#39
    OnGetLineAttr = edtGetLineAttr
    OnCaretChanged = edtCaretChanged
    OnPaintGutter = edtPaintGutter
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = PopupMenu1
  end
  object pnlNavigate: TPanel
    Left = 0
    Top = 469
    Width = 679
    Height = 29
    Align = alBottom
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 2
    DesignSize = (
      679
      29)
    object lblPos: TJvLabel
      Left = 584
      Top = 7
      Width = 25
      Height = 13
      Hint = 'Fraza'
      Caption = '1/10'
      Anchors = [akTop, akRight]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
    end
    object btnNextFile: TJvUpDown
      Left = 624
      Top = 2
      Width = 49
      Height = 25
      Hint = 'Poprzedni / Nast'#281'pny plik'
      Anchors = [akTop, akRight]
      Orientation = udHorizontal
      TabOrder = 0
      OnClick = btnNextFileClick
    end
    object tbLine: TJvScrollBar
      Left = 10
      Top = 5
      Width = 567
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Max = 20
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 1
      OnChange = tbLineChange
    end
  end
  object pnlWait: TJvPanel
    Left = 264
    Top = 200
    Width = 217
    Height = 81
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Caption = 'Wczytywanie pliku ...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
  object tmrOpen: TTimer
    Interval = 100
    OnTimer = tmrOpenTimer
    Left = 248
    Top = 344
  end
  object PopupMenu1: TPopupMenu
    Left = 496
    Top = 104
    object Zapiszorygianly1: TMenuItem
      Caption = 'Zapisz orygianly'
      OnClick = Zapiszorygianly1Click
    end
    object Zapiszjakotekst1: TMenuItem
      Caption = 'Zapisz jako tekst'
      OnClick = Zapiszjakotekst1Click
    end
  end
  object dlgSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 472
    Top = 48
  end
end
