program MultiFinder;

uses
  MemCheck,
  Forms,
  frm_Main in 'units\frm_Main.pas' {frmMain},
  frm_SearchFolders in 'units\frm_SearchFolders.pas' {frmSearchFolders: TEasyTaskPanelForm},
  frm_SearchFiles in 'units\frm_SearchFiles.pas' {frmSearchFiles},
  frm_SearchPhrase in 'units\frm_SearchPhrase.pas' {frmSearchPhrase},
  frm_SearchSize in 'units\frm_SearchSize.pas' {frmSearchSize},
  frm_SearchAttributes in 'units\frm_SearchAttributes.pas' {frmSearchAttributes},
  frm_SearchDate in 'units\frm_SearchDate.pas' {frmSearchDate},
  fra_FileList in 'units\fra_FileList.pas' {fraFileList: TFrame},
  unt_FileAnalyzer in 'units\unt_FileAnalyzer.pas',
  frm_SearchOptions in 'units\frm_SearchOptions.pas' {frmSearchOptions},
  frm_Preview in 'units\frm_Preview.pas' {frmPreview: TForma},
  frm_History in 'units\frm_History.pas' {frmHistory},
  frm_About in 'units\frm_About.pas' {frmAbout};

{$R *.res}

begin
  MemChk;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
