object frmSearchFiles: TfrmSearchFiles
  Left = 627
  Top = 273
  Width = 290
  Height = 233
  Caption = 'frmSearchFiles'
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
  object Panel1: TPanel
    Left = 0
    Top = 4
    Width = 282
    Height = 195
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object lv: TJvListView
      Left = 0
      Top = 0
      Width = 282
      Height = 195
      Align = alClient
      BorderStyle = bsNone
      Checkboxes = True
      Columns = <
        item
          Width = 20
        end
        item
          AutoSize = True
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
      ColumnsOrder = '0=20,1=242,2=20'
      Groups = <>
      ExtendedColumns = <
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end>
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 282
    Height = 4
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 1
  end
end
