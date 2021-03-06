unit frm_SearchFiles;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, EasyTaskPanelForm,
  StdCtrls, ExtCtrls, ComCtrls, JvListView, JvExComCtrls;

type
  TfrmSearchFiles = class(TEasyTaskPanelForm)
    Panel1: TPanel;
    lv: TJvListView;
    Panel2: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure lvKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvInsert(Sender: TObject; Item: TListItem);
    procedure lvDeletion(Sender: TObject; Item: TListItem);
    procedure lvInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
  private
    procedure lvEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtDestroy(Sender: TObject);
    procedure lvEditRow;
     procedure RefreshLV;
  public
     edt: TEdit;
  end;

 var
   frmSearchFiles: TfrmSearchFiles;

implementation

uses frm_Main;

{$R *.dfm}


procedure TfrmSearchFiles.lvEditRow;
var
  item: TListItem;
begin
  item := lv.Selected;
  if Item = nil then Exit;
  if not Assigned(edt) then edt := TEdit.Create(lv);
  edt.Visible := False;
  edt.Parent := lv;
  edt.Top := item.Top;
  edt.Text := lv.Selected.SubItems[0];
  if Trim(edt.Text) = CEditPrompt then edt.Text := '';
  edt.Left := lv.Columns[0].Width;
  edt.Width := lv.Columns[1].Width;
  edt.Height := 18;
  edt.Show;
  edt.SetFocus;
  edt.OnKeyDown := lvEditKeyDown;
  edt.OnExit := edtDestroy;
end;

procedure TfrmSearchFiles.lvEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then lv.SetFocus;
  if Key = VK_ESCAPE then
  begin
    if Trim(edt.Text) <> CEditPrompt then edt.Text := lv.Selected.SubItems[0];
    lv.SetFocus;
  end;

  if (Key = VK_UP) or (Key = VK_DOWN) then lv.SetFocus;

  if Key = VK_UP then
    if lv.ItemIndex > 0 then lv.ItemIndex := lv.ItemIndex - 1;

  if Key = VK_DOWN then
    if lv.ItemIndex < lv.Items.Count - 1 then lv.ItemIndex := lv.ItemIndex + 1;

  if (Key = VK_UP) or (Key = VK_DOWN) then
    if lv.Selected <> nil then
      lv.Selected .Focused := True;
end;

procedure TfrmSearchFiles.edtDestroy(Sender: TObject);
begin
  if lv.Selected = nil then Exit;
  if Trim(edt.Text) = '' then edt.Text := CEditPrompt
  else
  begin
    if lv.Selected.SubItems[0] = CEditPrompt then
      lv.Selected.Checked := True;
    lv.Selected.SubItems[0] := edt.Text;
  end;

  edt.Hide;

  if (Trim(edt.Text) <> '') and (Trim(edt.Text) <> CEditPrompt) then
    if lv.Selected.Index = 0 then lv.Items.Insert(0).SubItems.Add(CEditPrompt)
    else
  else
    if lv.Selected.Index > 0 then lv.Selected.Destroy;
end;

procedure TfrmSearchFiles.FormDestroy(Sender: TObject);
begin
  if Assigned(edt) then FreeAndNil(edt);
end;

procedure TfrmSearchFiles.FormCreate(Sender: TObject);
begin
  frmSearchFiles := Self;
  lv.Items.Add.SubItems.Add(CEditPrompt);
end;

procedure TfrmSearchFiles.lvAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  try
    DefaultDraw := True;

    if (SubItem = 0) and (Item.SubItems.Count > 0) then
      if Item.SubItems[0] = CEditPrompt then DefaultDraw := False;

    if Item.SubItems.Count > 0 then
      if Item.SubItems[0] = CEditPrompt then Sender.Canvas.Font.Color := clGray
      else Sender.Canvas.Font.Color := clWindowText;

    if (SubItem = 2) and (Item.SubItems.Count > 0) then
      if Item.SubItems[0] <> CEditPrompt then
      begin
        if Item.ImageIndex = 0 then
          Sender.Canvas.Draw(lv.Columns[0].Width + lv.Columns[1].Width, Item.Top ,
           frmMain.imgYes.Picture.Bitmap)
        else
          Sender.Canvas.Draw(lv.Columns[0].Width + lv.Columns[1].Width, Item.Top,
           frmMain.imgNo.Picture.Bitmap);
        DefaultDraw := False;
      end;
  except
  end;
end;

procedure TfrmSearchFiles.lvKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then frmMain.btnSearch.Click
  else
  if (Key = VK_DELETE) and (lv.Selected <> nil) then
    if lv.Selected.Index > 0 then lv.Selected.Delete
    else
  else
  if Key = VK_ESCAPE then frmMain.Close
  else
    lvEditRow;
end;

procedure TfrmSearchFiles.lvMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  item: TListItem;
begin
  if Button <> mbLeft then Exit;
  item := lv.GetItemAt(X, Y);
  if item = nil then Exit;

  if X > lv.Columns[0].Width + lv.Columns[1].WidthType then
  begin
    if item.ImageIndex = 1 then item.ImageIndex := 0 else
    item.ImageIndex := 1;
  end
  else
  if X > lv.Columns[0].Width then lvEditRow;

end;

procedure TfrmSearchFiles.RefreshLV;
begin
  lv.Columns[1].Width := lv.ClientWidth - lv.Columns[0].Width - lv.Columns[2].Width;;
  lv.Repaint;
end;

procedure TfrmSearchFiles.lvInsert(Sender: TObject; Item: TListItem);
begin
  RefreshLV;
end;

procedure TfrmSearchFiles.lvDeletion(Sender: TObject; Item: TListItem);
begin
  RefreshLV;
end;

procedure TfrmSearchFiles.lvInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: String);
begin
  if Item = nil then Exit;

  if Item.ImageIndex = 0 then InfoTip := 'Uwzględnij pliki "'
  else InfoTip := 'Wyklucz pliki "';

  InfoTip := InfoTip + Item.SubItems[0] + '"';
end;

end.


