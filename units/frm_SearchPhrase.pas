unit frm_SearchPhrase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EasyTaskPanelForm, StdCtrls, MPCommonObjects, EasyListview, ExtCtrls,
  ComCtrls, ShellCtrls, Mask, JvExMask, JvToolEdit, Buttons;

type
  TfrmSearchPhrase = class(TEasyTaskPanelForm)
    reText: TRichEdit;
    cbCaseSens: TCheckBox;
    cbUseMask: TCheckBox;
    btnMaskHelp: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure EasyTaskPanelFormCreate(Sender: TObject);
    procedure reTextChange(Sender: TObject);
    procedure cbCaseSensClick(Sender: TObject);
    procedure cbUseMaskClick(Sender: TObject);
    procedure EasyTaskPanelFormShow(Sender: TObject);
  private
    LastText: string;
  public
    { Public declarations }
  end;

  var
    frmSearchPhrase: TfrmSearchPhrase;
implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmSearchPhrase.EasyTaskPanelFormCreate(Sender: TObject);
begin
  frmSearchPhrase := Self;
end;

procedure TfrmSearchPhrase.reTextChange(Sender: TObject);
var
  n, v, u: Integer;
  isMsk: Boolean;
const
  chrs = ['?', '#', '$', '%', '@', '*', '^', '|', '_'];

begin
  if not frmMain.IsLoaded then Exit;

  frmMain.ControlChanged(Sender);
  try
    if reText.Text = LastText then Exit;

    reText.Enabled := False;
    v := reText.SelStart;
    u := reText.SelLength;

    isMsk := False;
    for n := 1 to Length(reText.Text) do
     if reText.Text[n] in chrs then isMsk := True;

    reText.SelStart := 0;
    reText.SelLength := Length(reText.Text);
    reText.SelAttributes.Color := clBlack;
    //reText.SelAttributes.Style := [];

    if isMsk then
      for n := 1 to Length(reText.Text) do
      begin
        reText.SelStart := n-1;
        reText.SelLength := 1;

        if (reText.Text[n] in chrs) and (cbUseMask.Checked) then
        begin
          reText.SelAttributes.Color := clRed;
         // reText.SelAttributes.Style := [fsBold];
        end;
      end;

     LastText := reText.Text;
     reText.SelStart := v;
     reText.SelLength := u;
     reText.Enabled := True;
     reText.SetFocus;
  except
  end;

end;

procedure TfrmSearchPhrase.cbCaseSensClick(Sender: TObject);
begin
  frmMain.ControlChanged(Sender);
end;

procedure TfrmSearchPhrase.cbUseMaskClick(Sender: TObject);
begin
  LastText := '';
  reTextChange(Sender);
end;

procedure TfrmSearchPhrase.EasyTaskPanelFormShow(Sender: TObject);
begin
  LastText := '';
  reTextChange(Sender);
end;

end.
