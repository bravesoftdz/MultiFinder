unit frm_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvGIF, ExtCtrls;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure ShowAbout(AOwner: TComponent);
  
implementation

{$R *.dfm}

procedure ShowAbout(AOwner: TComponent);
begin
  with TfrmAbout.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
