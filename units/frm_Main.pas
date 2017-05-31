unit frm_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MPCommonObjects, EasyListview, EasyTaskPanelForm, ExtCtrls,
  StdCtrls, ImgList, JvComponentBase, JvSearchFiles, Menus, JvMenus,
  JvLabel, ComCtrls, JvExControls, JvSpeedButton, JvExComCtrls, JvListView,
  JvExExtCtrls, JvExtComponent, JvPanel, JvProgressBar, JvStatusBar,
  MPCommonUtilities, Mask, JvExMask, JvToolEdit, JvCombobox, XPMan, Registry,
   Math, fra_FileList, jpeg, ToolWin, Buttons, JvExButtons,
  JvBitBtn;

type
  TfrmMain = class(TForm)
    pnlLeft: TPanel;
    taskPanel: TEasyTaskPanelBand;
    pnlFiles: TPanel;
    Splitter1: TSplitter;
    sBar: TJvStatusBar;
    pBar: TJvProgressBar;
    tmrSearchDone: TTimer;
    tmrProgress: TTimer;
    dlgSave: TSaveDialog;
    ilmain: TImageList;
    XPManifest1: TXPManifest;
    Pages: TPageControl;
    btnCloseTab: TButton;
    pnlBackground: TPanel;
    imgBackground: TImage;
    btnInfo: TButton;
    imgYes: TImage;
    imgNo: TImage;
    pnlSearch: TPanel;
    btnSearch: TJvBitBtn;
    Bevel1: TBevel;
    btnHistory: TJvBitBtn;
    pmMain: TPopupMenu;
    About1: TMenuItem;
    pmPages: TPopupMenu;
    miCloseTab: TMenuItem;
    procedure taskPanelGetTaskPanel(Sender: TCustomEasyListview;
      Group: TEasyGroup; var TaskPanel: TEasyTaskPanelFormClass);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure taskPanelGroupExpand(Sender: TCustomEasyListview;
      Group: TEasyGroup);
    procedure ControlChanged(Sender: TObject);
    procedure btnCloseTabClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure pnlBackgroundResize(Sender: TObject);
    procedure btnHistoryClick(Sender: TObject);
    procedure LoadConfig(profile: string; FromHistory: Boolean = False);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure taskPanelKeyAction(Sender: TCustomEasyListview;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure About1Click(Sender: TObject);
  private
    procedure SaveConfig(profile, description: string);

    procedure CpuInfo;
    function GetDescription: string;
    procedure SaveHistory(description: string);

    { Private declarations }
  public
    CPU_CoreCount: Integer;
    CPU_Name: string;
    SearchCount: Integer;
    IsLoaded: Boolean;
  end;

var
  frmMain: TfrmMain;

const
  CRegPath = 'Software\ZMsoft\MultiFinder\';
  CEditPrompt = '< kliknij tytaj aby dodaæ >';

implementation

uses frm_SearchPhrase, frm_SearchFiles, frm_SearchFolders, frm_SearchSize,
  frm_SearchAttributes, frm_SearchDate, unt_MD5, frm_SearchOptions,
  frm_History, frm_About;


{$R *.dfm}

procedure TfrmMain.taskPanelGetTaskPanel(Sender: TCustomEasyListview;
  Group: TEasyGroup; var TaskPanel: TEasyTaskPanelFormClass);
begin
  case Group.Index of
    0: TaskPanel := TfrmSearchFiles;
    1: TaskPanel := TfrmSearchFolders;
    2: TaskPanel := TfrmSearchPhrase;
    3: TaskPanel := TfrmSearchOptions;
    4: taskPanel := TfrmSearchSize;
    5: taskPanel := TfrmSearchAttributes;
    6: taskPanel := TfrmSearchDate;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  taskPanel.Themed := True;
  DoubleBuffered := True;
  CpuInfo;
  btnSearch.DoubleBuffered := True;
  btnHistory.DoubleBuffered := True;
  btnCloseTab.DoubleBuffered := True;
  btnInfo.DoubleBuffered := True;
  pnlSearch.DoubleBuffered := True;
  pnlFiles.DoubleBuffered := True;
  pages.DoubleBuffered := True;
  btnInfo.DoubleBuffered := True;
  pnlBackground.DoubleBuffered := True;
  taskPanel.DoubleBuffered := True;
end;

procedure TfrmMain.LoadConfig(profile: string; FromHistory: Boolean);
var
  profileHash, s: string;
  n, i: Integer;
  reg: TRegistry;
  li: TListItem;
begin
  reg := TRegistry.Create;
  try
    IsLoaded := False;
    frmSearchFiles.lv.Items.Clear;
    frmSearchFolders.lv.Items.Clear;

    reg.RootKey := HKEY_CURRENT_USER;

    if not FromHistory then
    begin
      profileHash := LowerCase(MD5String(profile));
      if not reg.OpenKey(CRegPath + 'Profiles\' + profileHash, False) then Exit;
    end
    else
      if not reg.OpenKey(CRegPath + 'History\' + profile, False) then Exit;

    if reg.ValueExists('SubFolders') then
      frmSearchOptions.seSubFolders.Value := reg.ReadInteger('SubFolders');

    if reg.ValueExists('IncludeHiddenFolders') then
      frmSearchOptions.cbHiddenFolders.Checked := reg.ReadBool('IncludeHiddenFolders');

    if reg.ValueExists('IncludeHiddenFiles') then
      frmSearchOptions.cbHiddenFiles.Checked := reg.ReadBool('IncludeHiddenFiles');

    if reg.ValueExists('Phrase') then
      frmSearchPhrase.reText.Text := reg.ReadString('Phrase');
    if reg.ValueExists('CaseSensitive') then
      frmSearchPhrase.cbCaseSens.Checked := reg.ReadBool('CaseSensitive');
    if reg.ValueExists('UseMask') then
      frmSearchPhrase.cbUseMask.Checked := reg.ReadBool('UseMask');

    if reg.ValueExists('DateFrom') then
      frmSearchDate.cbDateFrom.Checked := reg.ReadBool('DateFrom');
    if reg.ValueExists('DateFromValue') then
      frmSearchDate.dtpDateFrom.DateTime := reg.ReadDateTime('DateFromValue');

     frmSearchDate.dtpTimeFrom.DateTime := frmSearchDate.dtpDateFrom.DateTime;

    if reg.ValueExists('DateTo') then
      frmSearchDate.cbDateTo.Checked := reg.ReadBool('DateTo');
    if reg.ValueExists('DateToValue') then
      frmSearchDate.dtpDateTo.DateTime := reg.ReadDateTime('DateToValue');

    frmSearchDate.dtpTimeTo.DateTime :=  frmSearchDate.dtpDateTo.DateTime;

    if reg.ValueExists('SizeFrom') then
      frmSearchSize.cbSizeFrom.Checked := reg.ReadBool('SizeFrom');

    if reg.ValueExists('SizeFromValue') then
      with frmSearchSize do
      begin
        i := reg.ReadInteger('SizeFromValue');
        if (i >= 1048576) and (i mod 1048576 = 0) then
        begin
          seSizeFrom.Value := i div 1048576;
          cbbSizeFrom.ItemIndex := 2;
        end
        else
        if (i >= 1024) and (i mod 1024 = 0) then
        begin
          seSizeFrom.Value := i div 1024;
          cbbSizeFrom.ItemIndex := 1;
        end
        else
        begin
          seSizeFrom.Value := i;
          cbbSizeFrom.ItemIndex := 0;
        end;
      end;

    if reg.ValueExists('SizeTo') then
      frmSearchSize.cbSizeTo.Checked := reg.ReadBool('SizeTo');

    if reg.ValueExists('SizeToValue') then
      with frmSearchSize do
      begin
        i := reg.ReadInteger('SizeToValue');
        if (i >= 1048576) and (i mod 1048576 = 0) then
        begin
          seSizeTo.Value := i div 1048576;
          cbbSizeTo.ItemIndex := 2;
        end
        else
        if (i >= 1024) and (i mod 1024 = 0) then
        begin
          seSizeTo.Value := i div 1024;
          cbbSizeTo.ItemIndex := 1;
        end
        else
        begin
          seSizeTo.Value := i;
          cbbSizeTo.ItemIndex := 0;
        end;
      end;

    with frmSearchAttributes do
    begin
      if reg.ValueExists('AttributeR') then
        cbAttribR.State := TCheckBoxState(reg.ReadInteger('AttributeR'));
      if reg.ValueExists('AttributeA') then
        cbAttribA.State := TCheckBoxState(reg.ReadInteger('AttributeA'));
      if reg.ValueExists('AttributeH') then
        cbAttribH.State := TCheckBoxState(reg.ReadInteger('AttributeH'));
      if reg.ValueExists('AttributeS') then
        cbAttribS.State := TCheckBoxState(reg.ReadInteger('AttributeS'));
    end;

    for n := 1 to 9999 do
    begin
      if not reg.ValueExists('File' + IntToStr(n)) then Break;
      s := reg.ReadString('File' + IntToStr(n));
      if Length(s) < 2 then Continue;
      li := frmSearchFiles.lv.Items.Add;
      li.Checked := s[1] = '0';
      li.ImageIndex := StrToInt(s[2]);
      Delete(s, 1, 2);
      li.SubItems.Add(s);
    end;

    for n := 1 to 9999 do
    begin
      if not reg.ValueExists('Folder' + IntToStr(n)) then Break;
      s := reg.ReadString('Folder' + IntToStr(n));
      if Length(s) < 2 then Continue;
      li := frmSearchFolders.lv.Items.Add;
      li.Checked := s[1] = '0';
      li.ImageIndex := StrToInt(s[2]);
      Delete(s, 1, 2);
      li.SubItems.Add(s);
    end;
  finally
    reg.Free;

    frmSearchFiles.lv.Items.Insert(0).SubItems.Add(CEditPrompt);
    frmSearchFolders.lv.Items.Insert(0).SubItems.Add(CEditPrompt);
    IsLoaded := True;
  end;
  ControlChanged(nil);
end;

procedure TfrmMain.SaveHistory(description: string);
var
  s, ss: string;
  n: Integer;
  stl: TStringList;
  reg: TRegistry;
begin
  with frmSearchDate do
  begin
    dtpDateFrom.Time := dtpTimeFrom.Time;
    dtpDateTo.Time := dtpTimeTo.Time;
  end;

  s := LowerCase(MD5String(description));
  ss := LowerCase(FormatDateTime('YYMMDDhhmmsszzz', Now)) + #32 + s;

  reg := TRegistry.Create;
  stl := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey(CRegPath + 'History\', True);

    reg.GetKeyNames(stl);

    for n := 0 to stl.Count-1 do
      if Pos(s, stl[n]) > 0 then reg.DeleteKey(stl[n]);

    while stl.Count > 34 do
      if reg.KeyExists(stl[n]) then
        reg.DeleteKey(stl[n]);

    reg.OpenKey(ss, True);

  finally
    stl.Free;
    reg.Free;
  end;
  SaveConfig(ss, description);
end;

procedure TfrmMain.SaveConfig(profile, description: string);
var
  profileHash, s: string;
  n: Integer;
  reg: TRegistry;
begin
  with frmSearchDate do
  begin
    dtpDateFrom.Time := dtpTimeFrom.Time;
    dtpDateTo.Time := dtpTimeTo.Time;
  end;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if description = '' then
    begin
      profileHash := LowerCase(MD5String(profile));
      if reg.KeyExists(profileHash) then reg.DeleteKey(profileHash);
      reg.OpenKey(CRegPath + 'Profiles\' + profileHash, True);
    end
    else
      reg.OpenKey(CRegPath + 'History\' + profile, True);

    reg.WriteString('Name', profile);

    reg.WriteString('Description', description);
    reg.WriteDateTime('Date', Now);

    reg.WriteInteger('SubFolders', frmSearchOptions.seSubFolders.Value);
    reg.WriteBool('IncludeHiddenFolders', frmSearchOptions.cbHiddenFolders.Checked);
    reg.WriteBool('IncludeHiddenFiles', frmSearchOptions.cbHiddenFiles.Checked);

    reg.WriteString('Phrase', frmSearchPhrase.reText.Text);
    reg.WriteBool('CaseSensitive', frmSearchPhrase.cbCaseSens.Checked);
    reg.WriteBool('UseMask', frmSearchPhrase.cbUseMask.Checked);

    reg.WriteBool('DateFrom', frmSearchDate.cbDateFrom.Checked);
    reg.WriteDateTime('DateFromValue', frmSearchDate.dtpDateFrom.DateTime);
    reg.WriteBool('DateTo', frmSearchDate.cbDateTo.Checked);
    reg.WriteDateTime('DateToValue', frmSearchDate.dtpDateTo.DateTime);

    reg.WriteBool('SizeFrom', frmSearchSize.cbSizeFrom.Checked);
    reg.WriteInteger('SizeFromValue',
      frmSearchSize.seSizeFrom.Value * Round(Power(1024, frmSearchSize.cbbSizeFrom.ItemIndex)));
    reg.WriteBool('SizeTo', frmSearchSize.cbSizeTo.Checked);
    reg.WriteInteger('SizeToValue',
      frmSearchSize.seSizeTo.Value * Round(Power(1024, frmSearchSize.cbbSizeTo.ItemIndex)));

    reg.WriteInteger('AttributeR', Integer(frmSearchAttributes.cbAttribR.State));
    reg.WriteInteger('AttributeA', Integer(frmSearchAttributes.cbAttribA.State));
    reg.WriteInteger('AttributeH', Integer(frmSearchAttributes.cbAttribH.State));
    reg.WriteInteger('AttributeS', Integer(frmSearchAttributes.cbAttribS.State));

    with frmSearchFiles.lv do
      for n := 1 to Items.Count-1 do
      begin
        if Items[n].Checked then s := '0' else s := '1';
        s := s + IntToStr(Items[n].ImageIndex);
        s := s + Items[n].SubItems[0];
        reg.WriteString('File' + IntToStr(n), s);
      end;

    with frmSearchFolders.lv do
      for n := 1 to Items.Count-1 do
      begin
        if Items[n].Checked then s := '0' else s := '1';
        s := s + IntToStr(Items[n].ImageIndex);
        s := s + Items[n].SubItems[0];
        reg.WriteString('Folder' + IntToStr(n), s);
      end;
  finally
    reg.Free;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfig(#0#0#0#0, '');
  while Pages.PageCount > 0 do Pages.ActivePage.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  n: Integer;
begin
  for n := 2 to 6 do
    taskPanel.Groups[n].Expanded := False;
  LoadConfig(#0#0#0#0);
  IsLoaded := True;
end;

procedure TfrmMain.taskPanelGroupExpand(Sender: TCustomEasyListview;
  Group: TEasyGroup);
var
  n: Integer;
begin
  if Group.Index < 2 then Exit;
  for n := 2 to 6 do
    if n <> Group.Index then
      taskPanel.Groups[n].Expanded := False;
end;

procedure TfrmMain.ControlChanged(Sender: TObject);
begin
   if Trim(frmSearchPhrase.reText.Text) <> '' then
     taskPanel.Groups[2].Caption := 'Fraza *'
   else
     taskPanel.Groups[2].Caption := 'Fraza';

   if frmSearchOptions.cbHiddenFolders.Checked
      or frmSearchOptions.cbHiddenFiles.Checked
      or (frmSearchOptions.seSubFolders.Value <> 99)
   then
     taskPanel.Groups[3].Caption := 'Opcje *'
   else
     taskPanel.Groups[3].Caption := 'Opcje';

   if frmSearchSize.cbSizeFrom.Checked or frmSearchSize.cbSizeTo.Checked then
     taskPanel.Groups[4].Caption := 'Rozmiar *'
   else
     taskPanel.Groups[4].Caption := 'Rozmiar';

   with frmSearchAttributes do
     if (cbAttribR.State <> cbGrayed) or (cbAttribA.State <> cbGrayed)
       or (cbAttribH.State <> cbGrayed) or (cbAttribS.State <> cbGrayed)
     then
       taskPanel.Groups[5].Caption := 'Atrybuty *'
     else
       taskPanel.Groups[5].Caption := 'Atrybuty';

  if frmSearchDate.cbDateFrom.Checked or frmSearchDate.cbDateTo.Checked then
     taskPanel.Groups[6].Caption := 'Data *'
  else
    taskPanel.Groups[6].Caption := 'Data';
end;

procedure TfrmMain.CpuInfo;
var
  reg: TRegistry;
  n: Integer;
  s: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('HARDWARE\DESCRIPTION\System\CentralProcessor\', False);
    for n := 0 to 64 do
      if not reg.KeyExists(IntToStr(n)) then Break;
     reg.OpenKey('0', False);
    s := reg.ReadString('ProcessorNameString');
    if s = '' then s := 'CPU';
    s := IntToStr(n) + ' x ' + Trim(s);
    CPU_Name := s;

    //SetStatusHint('Procesor: ' + s, 3000);

    CPU_CoreCount := n;
  finally
    if CPU_CoreCount > 16 then CPU_CoreCount := 16;
    if CPU_CoreCount < 1 then CPU_CoreCount := 1;
    reg.Free;
  end;
end;

procedure TfrmMain.btnCloseTabClick(Sender: TObject);
begin
  if Pages.ActivePage = nil then Exit;
  Pages.ActivePage.Free;
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
var
  ts: TFrameTabSheet;
  desc: string;
  n: Integer;
begin
  if Assigned(frmSearchFolders.edt) then frmSearchFolders.lv.SetFocus;
  if Assigned(frmSearchFiles.edt) then frmSearchFiles.lv.SetFocus;

  desc := GetDescription;

  ts := nil;

  for n := 0 to Pages.PageCount-1 do
    if TFrameTabSheet(Pages.Pages[n]).Frame.Description = desc then
      ts := TFrameTabSheet(Pages.Pages[n]);

  Inc(SearchCount);

  if ts = nil then
  begin
    ts := TFrameTabSheet.Create(Pages);
    ts.PageControl := Pages;
    ts.Visible := False;
    ts.Visible := True;
  end
  else
    ts.Frame.Free;

  ts.Frame := TfraFileList.Create(ts);
  ts.Frame.Parent := ts;
  ts.Frame.Align := alClient;
  ts.Frame.Description := desc;

  ts.Frame.lv.Clear;
  ts.Caption := IntToStr(SearchCount){ + '. ' + FormatDateTime('hh:mm:ss', now)};
  ts.Frame.CreateSearchFrame;
  Pages.ActivePage := ts;
  pnlFiles.Visible := True;
  pnlBackground.Visible := False;

  SaveHistory(desc);
end;

function TfrmMain.GetDescription: string;
var
  sh: string;
  n: Integer;
begin
  sh := 'Pliki:' + #13#10;
  with frmSearchFiles.lv do
    for n := 1 to Items.Count-1 do
      if Items[n].Checked then
        sh := sh + Format('  %s (%s)' + #13#10, [Items[n].SubItems[0],
          BoolToStr(not Boolean(Items[n].ImageIndex), True)]);

  sh := sh + #13#10 + 'Foldery:' + #13#10;

  with frmSearchFolders.lv do
    for n := 1 to Items.Count-1 do
      if Items[n].Checked then
        sh := sh + Format('  %s (%s)' + #13#10, [Items[n].Subitems[0],
          BoolToStr(not Boolean(Items[n].ImageIndex), True)]);

  sh := sh + #13#10 + 'Opcje:' + #13#10;

  with frmSearchOptions do
    sh := sh + '  Iloœæ podkatalogów: ' + IntToStr(seSubFolders.Value) + #13#10
      + '  Przeszukuj katalogi ukryte i systemowe: ' + BoolToStr(cbHiddenFolders.Checked, True) + #13#10
      + '  Uwzglêdniaj pliki ukryte i systemowe: ' + BoolToStr(cbHiddenFiles.Checked, True) + #13#10;

  with frmSearchPhrase do
    if Trim(reText.Text) <> '' then
    begin
      sh := sh + #13#10 + 'Fraza:' + #13#10;

      sh := sh + '  Fraza: "' + reText.Text + '"' + #13#10
        + '  Uwzglêdnij wielkoœæ liter: ' + BoolToStr(cbCaseSens.Checked, True) + #13#10
        + '  Maskowanie: ' + BoolToStr(cbUseMask.Checked, True) + #13#10;
    end;

  with frmSearchSize do
  begin
    if cbSizeFrom.Checked or cbSizeTo.Checked then
      sh := sh + #13#10 + 'Rozmiar:' + #13#10;

    if cbSizeFrom.Checked then
      sh := sh + Format('  Rozmiar minimalny: %d %s' + #13#10,
        [seSizeFrom.Value, cbbSizeFrom.Text]);
    if cbSizeTo.Checked then
      sh := sh + Format('  Rozmiar maksymalny: %d %s' + #13#10,
        [seSizeTo.Value, cbbSizeTo.Text]);
  end;

  with frmSearchDate do
  begin
    if cbDateFrom.Checked or cbDateTo.Checked then
      sh := sh + #13#10 + 'Data:' + #13#10;

    if cbDateFrom.Checked then
      sh := sh + Format('  Data od: %s' + #13#10, [DateTimeToStr(dtpDateFrom.DateTime)]);
    if cbDateTo.Checked then
      sh := sh + Format('  Data do: %s' + #13#10, [DateTimeToStr(dtpDateTo.DateTime)]);
  end;

  with frmSearchAttributes do
  begin
    if (cbAttribR.State <> cbGrayed) or (cbAttribA.State <> cbGrayed)
        or (cbAttribH.State <> cbGrayed) or (cbAttribS.State <> cbGrayed)
    then sh := sh + #13#10 + 'Atrybuty:' + #13#10;

    if cbAttribR.State <> cbGrayed then
      sh := sh + Format('  Tylko do odczytu: %s' + #13#10,
        [BoolToStr(cbAttribR.State = cbChecked, True)]);
    if cbAttribA.State <> cbGrayed then
      sh := sh + Format('  Archiwalny: %s' + #13#10,
        [BoolToStr(cbAttribA.State = cbChecked, True)]);
    if cbAttribH.State <> cbGrayed then
      sh := sh + Format('  Ukryty: %s' + #13#10,
      [BoolToStr(cbAttribH.State = cbChecked, True)]);
    if cbAttribS.State <> cbGrayed then
      sh := sh + Format('  Systemowy: %s' + #13#10,
      [BoolToStr(cbAttribS.State = cbChecked, True)]);
  end;
  
  Result := sh;
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
begin
  ShowMessage(TFrameTabSheet(Pages.ActivePage).Frame.Description);
end;

procedure TfrmMain.pnlBackgroundResize(Sender: TObject);
begin
  imgBackground.Left := pnlBackground.Width div 2 - imgBackground.Width div 2;
  imgBackground.Top := pnlBackground.Height div 2 - imgBackground.Height div 2;
end;

procedure TfrmMain.btnHistoryClick(Sender: TObject);
begin
  with TfrmHistory.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then btnSearch.Click;
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmMain.taskPanelKeyAction(Sender: TCustomEasyListview;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  FormKeyDown(Sender, CharCode, Shift);
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowAbout(Self);
end;

end.
