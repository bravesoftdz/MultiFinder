unit fra_FileList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExControls, JvAnimatedImage, JvGIFCtrl, JvComponentBase,
  JvSearchFiles, ComCtrls, JvExComCtrls, JvListView, StdCtrls, JvLabel,
  ExtCtrls, JvExExtCtrls, JvExtComponent, JvPanel, JvStatusBar, Math, IniFiles,
  JvSpeedButton, Registry, ShellAPI, ImgList, Masks, JvProgressBar, DateUtils,
  JvSplitter, ToolWin, JvCoolBar, Buttons, JvSpeedbar, CommCtrl, Menus,
  JvMenus, Forma, JvTypes, StrUtils;

type
  TPhrasePos = record
    Pos: Integer;
    Line: Integer;
    StrFrom: Integer;
    StrTo: Integer;
    txt: string;
  end;

  TFileClass = class
  public
    Points: array of TPhrasePos;
    Name: string;
    Root: string;
    TextPercent: Currency;
    Size: Int64;
    Date: TDateTime;
    destructor Destroy; override;
  end;

  TfraFileList = class(TFrame)
    pnlText: TJvPanel;
    btnNextPhrase: TJvSpeedButton;
    btnPrevPhrase: TJvSpeedButton;
    JvPanel1: TJvPanel;
    re: TRichEdit;
    pnlText2: TJvPanel;
    lblText: TJvLabel;
    pnlFile: TJvPanel;
    lbFileAttrib: TLabel;
    lbFileSize: TLabel;
    lbFileDate: TLabel;
    imgIcon: TImage;
    lbFileName: TLabel;
    btnAbort: TJvSpeedButton;
    lv: TJvListView;
    tmrSearchDone: TTimer;
    tmrProgress: TTimer;
    pmMain: TJvPopupMenu;
    miShow: TMenuItem;
    miShowFile: TMenuItem;
    miDelete: TMenuItem;
    miExport: TMenuItem;
    ImageList1: TImageList;
    dlgSave: TSaveDialog;
    miCloseTab: TMenuItem;
    procedure lvSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnAbortClick(Sender: TObject);
    procedure imgIconClick(Sender: TObject);
    procedure tmrSearchDoneTimer(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure lvDblClick(Sender: TObject);
    procedure btnNextPhraseClick(Sender: TObject);
    procedure btnPrevPhraseClick(Sender: TObject);
    procedure lvKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miShowClick(Sender: TObject);
    procedure miShowFileClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miExportClick(Sender: TObject);
    procedure pmMainPopup(Sender: TObject);
    procedure miCloseTabClick(Sender: TObject);
  private
    IsDestroying: Boolean;
    PhraseNr: Integer;
    Search: TJvSearchFiles;
    FListViewWndProc: TWndMethod;
    procedure SetPhrase(ANr: Integer);
    procedure ListViewWndProc(var Message: TMessage);
    procedure AddItem(fc: TFileClass; ResultCount: Integer = 0);
    procedure BeginSearch;
  public
    Description: string;
    TotalProgress: Int64;
    TotalFileSize: Int64;

    ProgressTime: TDateTime;
    SpeedProgress: int64;
    SpeedValue: Currency;

   // AnalyzeStartTime: TDateTime;
    FileClassList: TList;
    AnalyzeThList: TList;
    FileList: TList;
    AbortSearch: Boolean;
    CS, CS_Progress: TRTLCriticalSection;
    CaseSensitive: Boolean;
    Text: string;
    UseMask: Boolean;
    Phrase: Boolean;
    procedure CreateSearchFrame;
    destructor Destroy; override;
  end;


  TFrameTabSheet = class(TTabSheet)
  public
    Frame: TfraFileList;
  end;

implementation

uses frm_Main, unt_FileAnalyzer, frm_SearchAttributes, frm_SearchDate,
  frm_SearchFiles, frm_SearchFolders, frm_SearchPhrase, frm_SearchSize,
  frm_SearchOptions, unt_Common, frm_Preview;

type
  TFileAnalyzeTh = class(TThread)
  private
    Text: string;
    fcls: TFileClass;
    Analyzer: TFileAnalyzer;
    procedure OnProgress(Processed, Current, Total: Int64);
    procedure OnFindPhrase(Position, LineNr, SelFrom, SelTo: Integer; txt: string);
  public
    Frame: TfraFileList;
    procedure Execute; override;
    procedure SyncAddFile;
    constructor Create(AFrame: TfraFileList);
    destructor Destroy; override;
  end;


  TSearchTh = class(TThread)
  private
    fcls: TFileClass;
    FoldersIncl, FoldersExcl: TStringList;
    FolderIdx: Integer;
    Frame: TfraFileList;
  public
    ExcludeMask: TStringList;
    sr: TJvSearchFiles;
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
    procedure OnFindFile(Sender: TObject; const AName: String);
    procedure SyncAddFile;
    procedure SyncFindFinish;
  end;

{$R *.dfm}


procedure TfraFileList.CreateSearchFrame;
begin
  FileClassList := TList.Create;
  FListViewWndProc := lv.WindowProc;
  lv.WindowProc := ListViewWndProc;

  AnalyzeThList := TList.Create;
  FileList := TList.Create;

  InitializeCriticalSection(CS);
  InitializeCriticalSection(CS_Progress);
  DoubleBuffered := True;
  //pnlMain.DoubleBuffered := True;
  pnlText.DoubleBuffered := True;
  lv.DoubleBuffered := True;
  pnlText2.DoubleBuffered := True;
  btnPrevPhrase.DoubleBuffered := True;
  btnNextPhrase.DoubleBuffered := True;

  CaseSensitive := frmSearchPhrase.cbCaseSens.Checked;
  Text := frmSearchPhrase.reText.Text;
  Phrase := Trim(Text) <> '';
  UseMask := frmSearchPhrase.cbUseMask.Checked;

  if Phrase then
    Text := StringReplace(Text, #13#10, '|', [rfReplaceAll]);

  lblText.Caption := '';

  if not Phrase then
  begin
    lv.Columns.Delete(3);
    lv.Columns.Delete(3);
  end;

  if not Phrase then
    pnlText.Visible := False;

  BeginSearch;
end;

procedure TfraFileList.ListViewWndProc(var Message: TMessage);
begin
 // if IsClosing then Exit;
  ShowScrollBar(lv.Handle, SB_HORZ, False);
  FListViewWndProc(Message);
end;

procedure TfraFileList.SetPhrase(ANr: Integer);
begin
  lblText.Caption := '';
  if lv.Selected = nil then Exit;
  if not Phrase then Exit;
  if Length(TFileClass(lv.Selected.Data).Points) = 0 then Exit;

  PhraseNr := ANr;

  with TFileClass(lv.Selected.Data) do
  begin
    if PhraseNr < 0 then PhraseNr := High(Points);
    if PhraseNr > High(Points) then PhraseNr := 0;

    re.Lines.Text := '';
    if Length(Points) > 0 then
    begin
      re.Lines.Text := points[PhraseNr].txt;
      re.SelStart := 0;
      re.SelLength := Length(re.Lines.Text);
      re.SelAttributes.Name := 'Courier New';

      re.SelStart := Points[PhraseNr].StrFrom-1;
      re.SelLength := Points[PhraseNr].StrTo - Points[PhraseNr].StrFrom+1;
      re.SelAttributes.Style := [fsBold];
      re.SelAttributes.Color := clRed;
      re.SelLength := 0;
    end;
    lblText.Caption := Format('Fraza %d/%d, Linia: %d, Kolumna: %d', [
      PhraseNr+1, Length(Points), Points[PhraseNr].Line, Points[PhraseNr].StrFrom]);

  end;
end;

procedure TfraFileList.BeginSearch;
  function SetAttrib(state: TCheckBoxState): TJvAttrFlagKind;
  begin
    case state of
      cbUnchecked: Result := tsMustBeUnSet;
      cbChecked:   Result := tsMustBeSet;
    else
      Result := tsDontCare;
    end;
  end;
var
  n, o: Integer;
  st: TJvSearchTypes;
  stl: TStringList;
  s: string;
begin
  stl := TStringList.Create;
  with frmMain, TSearchTh.Create do
  try
   // btnAddFileClick(nil);
   // btnAddFolderClick(nil);

    Search := sr;
    sr.Options := [soSearchFiles];
    if frmSearchOptions.cbHiddenFolders.Checked then
      sr.Options :=  sr.Options + [soIncludeSystemHiddenDirs];
    if frmSearchOptions.cbHiddenFiles.Checked then
      sr.Options :=  sr.Options + [soIncludeSystemHiddenFiles];

    s := '';

    sr.FileParams.FileMasks.Clear;
    ExcludeMask.Clear;


    with frmSearchFiles.lv do
      for n := 0 to Items.Count-1 do
        if Items[n].Checked and (Items[n].ImageIndex = 0) then
        begin
           stl.Text := AnsiReplaceStr(Items[n].Subitems[0], ';', #13#10);
           for o := 0 to stl.Count-1 do
             if Trim(stl[o]) <> '' then
               AddToStringList(stl[o], sr.FileParams.FileMasks);
        end
        else
        if Items[n].Checked and (Items[n].ImageIndex = 1) then
        begin
           stl.Text := AnsiReplaceStr(Items[n].Subitems[0], ';', #13#10);
           for o := 0 to stl.Count-1 do
             if Trim(stl[o]) <> '' then
               AddToStringList(stl[o], ExcludeMask);
        end;

    if frmSearchOptions.seSubFolders.Value > 0 then sr.DirOption := doIncludeSubDirs
    else sr.DirOption := doExcludeSubDirs;

    sr.RecurseDepth := frmSearchOptions.seSubFolders.Value;

    st := [stFileMask];

    with frmSearchAttributes, sr.FileParams.Attributes do
    begin
      if (cbAttribR.State <> cbGrayed) or (cbAttribA.State <> cbGrayed)
          or (cbAttribH.State <> cbGrayed) or (cbAttribS.State <> cbGrayed)
      then st := st + [stAttribute];
      ReadOnly := SetAttrib(cbAttribR.State);
      Archive := SetAttrib(cbAttribA.State);
      Hidden := SetAttrib(cbAttribH.State);
      System := SetAttrib(cbAttribS.State);
    end;


    with frmSearchSize do
    begin
      if cbSizeFrom.Checked then st := st + [stMinSize];
      if cbSizeTo.Checked then st := st + [stMaxSize];

      sr.FileParams.MinSize :=  seSizeFrom.Value * Round(Power(1024, cbbSizeFrom.ItemIndex));
      sr.FileParams.MaxSize :=  seSizeTo.Value * Round(Power(1024, cbbSizeTo.ItemIndex));

    end;

    with frmSearchDate do
    begin
      dtpDateFrom.Time := dtpTimeFrom.Time;
      dtpDateTo.Time := dtpTimeTo.Time;

      if cbDateFrom.Checked then st := st + [stLastChangeAfter];
      if cbDateTo.Checked then st := st + [stLastChangeBefore];

      sr.FileParams.LastChangeAfter := dtpDateFrom.DateTime;
      sr.FileParams.LastChangeBefore := dtpDateTo.DateTime;
    end;

    sr.FileParams.SearchTypes := st;

    Frame := Self;
    FolderIdx := 0;
    sr.OnFindFile := OnFindFile;

    AbortSearch := False;

    FoldersIncl.Clear;
    FoldersExcl.Clear;

    with frmSearchFolders.lv do
      for n := 0 to Items.Count-1 do
        if (Trim(Items[n].Subitems[0]) <> '') and Items[n].Checked then
            if Items[n].ImageIndex = 0 then
              FoldersIncl.Add(Items[n].Subitems[0])
            else
              FoldersExcl.Add(Items[n].Subitems[0]);

    Resume;
  finally
    stl.Free;
  end;
end;

{ TSearchTh }

constructor TSearchTh.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  sr := TJvSearchFiles.Create(nil);
  ExcludeMask := TStringList.Create;
  FoldersIncl := TStringList.Create;
  FoldersExcl := TStringList.Create;
end;

destructor TSearchTh.Destroy;
begin
  sr.Free;
  FoldersIncl.Free;
  FoldersExcl.Free;
  Frame.Search := nil;
  ExcludeMask.Free;
  inherited;
end;

procedure TSearchTh.Execute;
var
  n: Integer;
begin
  inherited;
  try
    for n := 0 to FoldersIncl.Count-1 do
    begin
      sr.RootDirectory := FoldersIncl[n];
      sr.Search;

    end;
  finally
    Synchronize(SyncFindFinish);
    Terminate;
  end;
end;


procedure TfraFileList.AddItem(fc: TFileClass; ResultCount: Integer);
var
  v: Integer;
  s, t: string;
begin
  s := IncludeTrailingPathDelimiter(Trim(fc.Root));
  v := Length(s);

  with lv.Items.Add do
  begin
    s := fc.Name;
    Caption := IntToStr(lv.Items.Count);
    ImageIndex := 0;

    t := ExtractFileExt(s);
    if Length(t) > 0 then
      if t[1] = '.' then t := Copy(t, 2, Length(t)-1);
    SubItems.Add( ChangeFileExt(ExtractFileName(s), ''));
    SubItems.Add(t);

    if Phrase then
    begin
      SubItems.Add(IntToStr(ResultCount));
      SubItems.Add(FormatFloat('0 %', fc.TextPercent));
    end;

    fc.Size := FileSize(fc.Name);
    fc.Date := FileDateToDateTime(FileAge(fc.Name));

    s := IntToStr(fc.Size);

    {v := 1;
    for n := Length(s) downto 1 do
    begin
      if v = 4 then
      begin
        Insert(#32, s, n+1);
        v := 1;
      end
      else
        Inc(v);


    end;  }

    SubItems.Add(s);
     SubItems.Add(DateTimeToStr(fc.Date));


    s := ExtractFileDir(fc.Name);
    s := Copy(s, v+1, Length(s)-v);
    SubItems.Add(s);
    Data := fc;

  end;

end;

procedure TSearchTh.OnFindFile(Sender: TObject; const AName: String);
var
  n: Integer;
begin
  for n := 0 to ExcludeMask.Count-1 do
    if MatchesMask(AName, ExcludeMask[n]) then Exit;

  for n := 0 to Frame.FileList.Count-1 do
    if TFileClass(Frame.FileList[n]).Name = AName then Exit;

  for n := 0 to FoldersExcl.Count-1 do
    if Pos(FoldersExcl[n], AName) > 0 then Exit;


  if Frame.AbortSearch then sr.Abort;
  frmMain.sBar.SimpleText := ExtractFilePath(AName);
  fcls := TFileClass.Create;
  Frame.FileClassList.Add(fcls);

  fcls.Name := AName;
  fcls.Root := sr.RootDirectory;
  Frame.FileList.Add(fcls);
  if not Frame.Phrase then Synchronize(SyncAddFile);

end;


procedure TfraFileList.btnAbortClick(Sender: TObject);
var
  n: Integer;
begin
  EnterCriticalSection(CS);
  try
    AbortSearch := True;
    if Assigned(Search) then Search.Abort;

    for n := 0 to AnalyzeThList.Count-1 do
    begin
      TFileAnalyzeTh(AnalyzeThList[n]).Terminate;
      TFileAnalyzeTh(AnalyzeThList[n]).Analyzer.Abort := True;
    end;

  finally
    LeaveCriticalSection(CS);
  end;
end;


procedure TfraFileList.lvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  ExtNO: Word;
  fIco: TIcon;
  ff: TFileStream;
  fd: Integer;
  fa: Integer;
  fn: String;
  fs: Int64;
  sFs: string; //rozmiar pliku string
  sFa : string;
begin
  try
    if IsDestroying then Exit;
    if Item = nil then Exit;

    fIco := TIcon.Create;
    try
      fn := TFileClass(Item.Data).Name;
      if not FileExists(fn) then Exit;
      ff := TFileStream.Create(fn, fmOpenRead + fmShareDenyNone);
      fs := ff.Size;
      {$WARNINGS OFF}
      fa := FileGetAttr(fn);
      {$WARNINGS ON}
      fd := FileGetDate(ff.Handle);
      ff.Free;

      if fs >= 1048576 then sFs :=
        CurrToStr(StrToCurr(FormatFloat('0.00', fs / 1048576))) + ' MB'
      else
      if fs >= 1024 then sFs :=
        CurrToStr(StrToCurr(FormatFloat('0.00', fs / 1024))) + ' kB'
      else
      sFs := IntToStr(fs) + ' B';

      fico.Handle := ExtractAssociatedIcon(hInstance, PCHar(fn), ExtNO);
      ImgIcon.Picture.Icon := fIco;

      sFa := '----';
      if fa and 1 = 1 then sFa[1] := 'r';
      if fa and 32 = 32 then sFa[2] := 'a';
      if fa and 2 = 2 then sFa[3] := 'h';
      if fa and 4 = 4 then sFa[4] := 's';

      lbFileName.Caption := fn;
      lbFileSize.Caption := 'Rozmiar: ' + sFs;
      lbFileDate.Caption := 'Zmodyfikowany: ' +
        DateToStr(FileDateToDateTime(fd)) + ' ' + TimeToStr(FileDateToDateTime(fd));

      lbFileAttrib.Caption := 'Atrybuty: ' + sFa;
    finally
      fIco.Free;
    end;

    btnNextPhrase.Enabled :=  Length(TFileClass(Item.Data).Points) > 1;
    btnPrevPhrase.Enabled :=  btnNextPhrase.Enabled;

    SetPhrase(0);
    pnlFile.Visible := True;
    pnlText.Visible := Phrase;

  except
    //on E: Exception do SetStatusHint(E.Message, 3000);
  end;
end;

procedure TfraFileList.imgIconClick(Sender: TObject);
begin
  ShellExecute(Handle, 'OPEN', 'explorer.exe',
    PChar('/select, "' +lbFileName.Caption + '"'), nil, SW_NORMAL) ;

end;

procedure TSearchTh.SyncAddFile;
begin
  Frame.AddItem(fcls);
end;

procedure TSearchTh.SyncFindFinish;
var
  n: Integer;
begin
  EnterCriticalSection(Frame.CS);
  try
    Frame.TotalFileSize := sr.TotalFileSize;

    if (not Frame.Phrase) or (Frame.FileList.Count = 0) or Frame.AbortSearch then
      Frame.tmrSearchDone.Enabled := True
    else
    begin
      Frame.ProgressTime := Now;

     // Form.AnalyzeStartTime := Now;

      for n := 1 to frmMain.CPU_CoreCount do
        Frame.AnalyzeThList.Add(TFileAnalyzeTh.Create(Frame));

      Frame.tmrProgress.Enabled := True;
      frmMain.pBar.Visible := True;

    end;
  finally
    LeaveCriticalSection(Frame.CS);
  end;
end;

{ TFileAnalyzeTh }

constructor TFileAnalyzeTh.Create(AFrame: TfraFileList);
begin
  inherited Create(True);
  Analyzer := TFileAnalyzer.Create;
  Analyzer.OnProgress := OnProgress;
  Analyzer.OnFindPhrase2 := OnFindPhrase;
  Frame := AFrame;

  Analyzer.CaseSensitive := Frame.CaseSensitive;
  Text := Frame.Text;
  Analyzer.UseMask := Frame.UseMask;

  FreeOnTerminate := True;
  Resume;
end;

destructor TFileAnalyzeTh.Destroy;
begin

  Analyzer.Free;
  inherited;
end;

procedure TFileAnalyzeTh.Execute;
var
  n: Integer;
begin
  inherited;

  try
    while not Terminated do
    try
      EnterCriticalSection(Frame.CS);
      try
        if Frame.FileList.Count = 0 then Terminate;
        if Terminated then Break;
        fcls := TFileClass(Frame.FileList[0]);
        Frame.FileList.Delete(0);
      finally
        LeaveCriticalSection(Frame.CS);
      end;

      try
        Analyzer.FileName := fcls.Name;
        if Terminated then Break;
        Analyzer.AnalyzeFile(Text);
        if Analyzer.TotalSize <> 0 then
          fcls.TextPercent := 100 * Analyzer.TextCharCount / Analyzer.TotalSize;
        Synchronize(SyncAddFile);
      except
        Continue;
      end;

    finally
     // if Assigned(fcls) then
      //  if (Length(fcls.Points) = 0) then FreeAndNil(fcls);
    end;
  finally
    EnterCriticalSection(Frame.CS);
    try
      for n := 0 to Frame.AnalyzeThList.Count-1 do
        if Frame.AnalyzeThList[n] = Self then
        begin
          Frame.AnalyzeThList.Delete(n);
          Break;
        end;
      if Frame.AnalyzeThList.Count = 0 then Frame.tmrSearchDone.Enabled := True;
    finally
      LeaveCriticalSection(Frame.CS);
    end;
    Terminate;
  end;

end;



procedure TfraFileList.tmrSearchDoneTimer(Sender: TObject);
begin
 // Self.Caption := FormatDateTime('ss.zzz', Now - AnalyzeStartTime);
  tmrSearchDone.Enabled := False;
  tmrProgress.Enabled := False;
  btnAbort.Visible := False;
  frmMain.pBar.Visible := False;
  frmMain.sBar.SimpleText := 'Znalezionych plików: ' + IntToStr(lv.Items.Count);
end;


procedure TFileAnalyzeTh.SyncAddFile;
begin
  if Length(fcls.Points) > 0 then
    Frame.AddItem(fcls, Length(fcls.Points));
end;



procedure TFileAnalyzeTh.OnFindPhrase(Position, LineNr, SelFrom, SelTo: Integer;
  txt: string);
var
  p: ^TPhrasePos;
begin
  SetLength(fcls.Points, Length(fcls.Points)+1);
  p := @fcls.Points[High(fcls.Points)];

  p.Pos := Position;
  p.Line := LineNr;
  p.StrFrom := SelFrom;
  p.StrTo := SelTo;
  p.txt := txt;
end;

procedure TFileAnalyzeTh.OnProgress(Processed, Current, Total: Int64);
begin
  EnterCriticalSection(Frame.CS_Progress);
  try
    Frame.TotalProgress := Frame.Totalprogress + Processed;
    Frame.SpeedProgress := Frame.SpeedProgress + Processed;
  finally
    LeaveCriticalSection(Frame.CS_Progress);
  end;
end;

{ TFileClass }


destructor TFileClass.Destroy;
begin
  SetLength(Points, 0);
  inherited;
end;



procedure TfraFileList.tmrProgressTimer(Sender: TObject);
var
  v, pr: int64;
  d: Double;
begin
  try
    EnterCriticalSection(CS_Progress);
    try
      d := 100 * TotalProgress / TotalFileSize;
      pr :=  TotalProgress;

      v := MilliSecondsBetween(Now, ProgressTime);

      if v > 100 then
      begin
        ProgressTime := Now;
        SpeedValue := (1000 * SpeedProgress / v + SpeedProgress) / 2;
        SpeedProgress := 0;
      end;

    finally
      LeaveCriticalSection(CS_Progress);
    end;

    frmMain.pBar.Position := Round(d * 10);

    frmMain.sBar.SimpleText := 'Postêp: ' + FormatFloat('0.00',d) + '% ('
      + FileSize(pr, '0.0') +  ' / ' + FileSize(TotalFileSize, '0.0') +  ' )'
      + ', prêdkoœæ: ' + FileSize(Round(SpeedValue), '0.0') + '/s';;

  except
  end;
end;

procedure TfraFileList.lvDblClick(Sender: TObject);
begin
  if Phrase then miShow.Click else miShowFile.Click;
end;

{procedure TfraFileList.FormResize(Sender: TObject);
begin
  pBar.Left := ClientWidth - pBar.Width - 32;
end;  }

procedure TfraFileList.btnNextPhraseClick(Sender: TObject);
begin
  SetPhrase(PhraseNr+1);
end;

procedure TfraFileList.btnPrevPhraseClick(Sender: TObject);
begin
  SetPhrase(PhraseNr-1);
end;

procedure TfraFileList.lvKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if lv.Selected = nil then Exit;

  if (Key = VK_Right) and btnNextPhrase.Enabled then btnNextPhrase.Click
  else
  if (Key = VK_Left) and btnPrevPhrase.Enabled then btnPrevPhrase.Click
  else
  if Key = VK_Delete then
    miDelete.Click;

  //FormaKeyDown(Sender, Key, Shift);

end;

procedure TfraFileList.miShowClick(Sender: TObject);
begin
  if lv.Selected = nil then Exit;
  ShowFile(TFileClass(lv.Selected.Data), Self);
end;

procedure TfraFileList.miShowFileClick(Sender: TObject);
begin
  if lv.Selected = nil then Exit;
  imgIconClick(nil);
end;

procedure TfraFileList.miDeleteClick(Sender: TObject);
begin
  if lv.Selected = nil then exit;
    if FileOperation(Handle, foDelete, TFileClass(lv.Selected.Data).Name) then
      if not FileExists(TFileClass(lv.Selected.Data).Name) then lv.Selected.Delete;
end;


procedure TfraFileList.miExportClick(Sender: TObject);
var
  stl: TStringList;
  m, n: Integer;
  s: string;
begin
  if not dlgSave.Execute then Exit;
  stl := TStringList.Create;
  try
    for m := 0 to lv.Items.Count-1 do
    begin
      s := '"' + lv.Items[m].Caption + '"';
      for n := 0 to lv.Items[m].SubItems.Count-1 do
        s := s + ';"' + lv.Items[m].SubItems[n] + '"';
      stl.Add(s);
    end;
    stl.SaveToFile(dlgSave.FileName);
  finally
    stl.Free;
  end;
end;

procedure TfraFileList.pmMainPopup(Sender: TObject);
begin
  if Phrase then miShow.Default := True else miShowFile.Default := True;

  miExport.Visible := lv.Items.Count > 0;
  miShow.Visible := lv.Selected <> nil;
  miShowFile.Visible := lv.Selected <> nil;
  miDelete.Visible := lv.Selected <> nil;

end;

destructor TfraFileList.Destroy;
begin
  IsDestroying := True;

  if btnAbort.Visible then
  begin
    btnAbort.Click;

    while btnAbort.Visible do
    begin
      Sleep(1);
      Application.ProcessMessages;
    end;
  end;
  
  FreeList(FileClassList);
  lv.WindowProc := FListViewWndProc; // restore window proc
  FListViewWndProc := nil;

  DeleteCriticalSection(CS);
  DeleteCriticalSection(CS_Progress);
  FileList.Free;
  AnalyzeThList.Free;

  frmMain.pnlFiles.Visible  := frmMain.Pages.PageCount > 0;
  frmMain.pnlBackground.Visible := not frmMain.pnlFiles.Visible;


  inherited;
end;

procedure TfraFileList.miCloseTabClick(Sender: TObject);
begin
  frmMain.btnCloseTab.Click;
end;

end.
