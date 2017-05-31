unit frm_SearchSize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EasyTaskPanelForm, StdCtrls, MPCommonObjects, EasyListview, ExtCtrls,
  ComCtrls, ShellCtrls, Mask, JvExMask, JvToolEdit, Spin, JvListView,
  JvExComCtrls;

type
  TfrmSearchSize = class(TEasyTaskPanelForm)
    Panel1: TPanel;
    seSizeTo: TSpinEdit;
    seSizeFrom: TSpinEdit;
    cbSizeTo: TCheckBox;
    cbSizeFrom: TCheckBox;
    cbbSizeTo: TComboBox;
    cbbSizeFrom: TComboBox;
    procedure EasyTaskPanelFormCreate(Sender: TObject);
    procedure cbSizeFromClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearchSize: TfrmSearchSize;

implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmSearchSize.EasyTaskPanelFormCreate(Sender: TObject);
begin
  frmSearchSize := Self;
end;

procedure TfrmSearchSize.cbSizeFromClick(Sender: TObject);
begin
  frmMain.ControlChanged(Sender);  
end;

end.
