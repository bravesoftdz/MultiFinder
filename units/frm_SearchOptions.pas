unit frm_SearchOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EasyTaskPanelForm, StdCtrls, MPCommonObjects, EasyListview, ExtCtrls,
  ComCtrls, ShellCtrls, Mask, JvExMask, JvToolEdit, Spin, JvListView,
  JvExComCtrls;

type
  TfrmSearchOptions = class(TEasyTaskPanelForm)
    Label1: TLabel;
    seSubFolders: TSpinEdit;
    cbHiddenFolders: TCheckBox;
    cbHiddenFiles: TCheckBox;
    procedure cbHiddenFoldersClick(Sender: TObject);
    procedure EasyTaskPanelFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearchOptions: TfrmSearchOptions;

implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmSearchOptions.cbHiddenFoldersClick(Sender: TObject);
begin
  frmMain.ControlChanged(Sender);  
end;

procedure TfrmSearchOptions.EasyTaskPanelFormCreate(Sender: TObject);
begin
  frmSearchOptions := Self;
end;

end.
