unit frm_SearchDate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EasyTaskPanelForm, StdCtrls, MPCommonObjects, EasyListview, ExtCtrls,
  ComCtrls, ShellCtrls, Mask, JvExMask, JvToolEdit, Spin, JvListView,
  JvExComCtrls;


type
  TfrmSearchDate = class(TEasyTaskPanelForm)
    Panel1: TPanel;
    cbDateFrom: TCheckBox;
    dtpDateFrom: TDateTimePicker;
    dtpTimeFrom: TDateTimePicker;
    cbDateTo: TCheckBox;
    dtpDateTo: TDateTimePicker;
    dtpTimeTo: TDateTimePicker;
    procedure EasyTaskPanelFormCreate(Sender: TObject);
    procedure cbDateFromClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearchDate: TfrmSearchDate;

implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmSearchDate.EasyTaskPanelFormCreate(Sender: TObject);
begin
  frmSearchDate := Self;
  dtpDateFrom.DateTime := Now - 7;
  dtpDateTo.DateTime := Now;

  dtpTimeFrom.DateTime := dtpDateFrom.DateTime;
  dtpTimeTo.DateTime := dtpDateTo.DateTime;
end;

procedure TfrmSearchDate.cbDateFromClick(Sender: TObject);
begin
  frmMain.ControlChanged(Sender);
end;

end.
