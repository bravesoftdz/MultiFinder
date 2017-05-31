unit frm_SearchAttributes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EasyTaskPanelForm, StdCtrls, MPCommonObjects, EasyListview, ExtCtrls,
  ComCtrls, ShellCtrls, Mask, JvExMask, JvToolEdit, Spin, JvListView,
  JvExComCtrls;

type
  TfrmSearchAttributes = class(TEasyTaskPanelForm)
    Panel1: TPanel;
    cbAttribA: TCheckBox;
    cbAttribR: TCheckBox;
    cbAttribS: TCheckBox;
    cbAttribH: TCheckBox;
    procedure EasyTaskPanelFormCreate(Sender: TObject);
    procedure cbAttribRClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearchAttributes: TfrmSearchAttributes;

implementation

uses frm_Main;

{$R *.dfm}

procedure TfrmSearchAttributes.EasyTaskPanelFormCreate(Sender: TObject);
begin
  frmSearchAttributes := self;
end;

procedure TfrmSearchAttributes.cbAttribRClick(Sender: TObject);
begin
  frmMain.ControlChanged(Sender);
end;

end.
