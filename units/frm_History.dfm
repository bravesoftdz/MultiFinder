object frmHistory: TfrmHistory
  Left = 434
  Top = 331
  Width = 679
  Height = 454
  Caption = 'Historia'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 671
    Height = 420
    Align = alClient
    BorderWidth = 8
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 169
      Top = 9
      Width = 7
      Height = 402
    end
    object mmo: TJvMemo
      Left = 176
      Top = 9
      Width = 486
      Height = 402
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      OnKeyDown = mmoKeyDown
    end
    object Panel2: TPanel
      Left = 9
      Top = 9
      Width = 160
      Height = 402
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Panel3: TPanel
        Left = 0
        Top = 369
        Width = 160
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          160
          33)
        object btnLoad: TButton
          Left = 80
          Top = 8
          Width = 73
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Wczytaj'
          TabOrder = 0
          OnClick = btnLoadClick
          OnKeyDown = mmoKeyDown
        end
      end
      object lv: TJvListView
        Left = 0
        Top = 0
        Width = 160
        Height = 369
        Align = alClient
        Columns = <
          item
            Width = 20
          end
          item
            AutoSize = True
          end
          item
            AutoSize = True
          end>
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 1
        ViewStyle = vsReport
        OnDblClick = lvDblClick
        OnKeyDown = mmoKeyDown
        OnSelectItem = lvSelectItem
        ColumnsOrder = '0=20,1=68,2=68'
        Groups = <>
        ExtendedColumns = <
          item
          end
          item
          end
          item
          end>
      end
    end
  end
end
