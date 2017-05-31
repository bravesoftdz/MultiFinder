object frmSearchFolders: TfrmSearchFolders
  Left = 515
  Top = 100
  Width = 337
  Height = 306
  Caption = 'frmSearchFolders'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Themed = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 272
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object lv: TJvListView
      Left = 0
      Top = 4
      Width = 329
      Height = 268
      Align = alClient
      BorderStyle = bsNone
      Checkboxes = True
      Columns = <
        item
          Width = 20
        end
        item
          AutoSize = True
          Caption = 'Plik'
        end
        item
          Width = 20
        end>
      ReadOnly = True
      RowSelect = True
      ParentShowHint = False
      ShowColumnHeaders = False
      ShowHint = True
      TabOrder = 0
      ViewStyle = vsReport
      OnAdvancedCustomDrawSubItem = lvAdvancedCustomDrawSubItem
      OnDeletion = lvDeletion
      OnInfoTip = lvInfoTip
      OnInsert = lvInsert
      OnKeyDown = lvKeyDown
      OnMouseDown = lvMouseDown
      ColumnsOrder = '0=20,1=289,2=20'
      Groups = <>
      ExtendedColumns = <
        item
        end
        item
        end
        item
        end>
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 329
      Height = 4
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
    end
  end
end
