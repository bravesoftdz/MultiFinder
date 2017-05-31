unit frm_History;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, JvExStdCtrls, JvMemo, JvListBox, Registry,
  ComCtrls, JvExComCtrls, JvListView;

type
  TfrmHistory = class(TForm)
    Panel1: TPanel;
    mmo: TJvMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    btnLoad: TButton;
    lv: TJvListView;
    procedure FormShow(Sender: TObject);
    procedure lvSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnLoadClick(Sender: TObject);
    procedure lvDblClick(Sender: TObject);
    procedure mmoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure LoadHistory;
    { Private declarations }
  public
  end;



implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmHistory.LoadHistory;
var
  n: Integer;
  stl: TStringList;
  reg: TRegistry;
  li: TListItem;
begin
  reg := TRegistry.Create;
  stl := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(CRegPath + 'History\', False) then Exit;

    reg.GetKeyNames(stl);

    for n := stl.Count-1 downto 0 do
    begin
      reg.CloseKey;
      reg.OpenKey(CRegPath + 'History\' + stl[n], True);
      li := lv.Items.Add;
      li.Caption := IntToStr(lv.Items.Count);
      li.SubItems.Add(DateToStr(reg.ReadDateTime('Date')));
      li.SubItems.Add(TimeToStr(reg.ReadDateTime('Date')));
      li.SubItems.Add(stl[n]);
      li.SubItems.Add(reg.ReadString('Description'));

    end;

  finally
    stl.Free;
    reg.Free;
  end;

end;

procedure TfrmHistory.FormShow(Sender: TObject);
begin
  LoadHistory;
end;

procedure TfrmHistory.lvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Item = nil then Exit;
  mmo.Lines.Text := Item.SubItems[3];
end;

procedure TfrmHistory.btnLoadClick(Sender: TObject);
begin
  if lv.Selected = nil then Exit;
  frmMain.LoadConfig(lv.Selected.SubItems[2], True);
  Close;
end;

procedure TfrmHistory.lvDblClick(Sender: TObject);
begin
  btnLoad.Click;
end;

procedure TfrmHistory.mmoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.
