unit frm_Preview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExControls, JvEditorCommon, JvEditor, JvUnicodeEditor, JclWideStrings,
  ComCtrls, JvExComCtrls, JvStatusBar, StdCtrls, ExtCtrls, fra_FileList,
  JvUpDown, JvComCtrls, JvLabel, JvExStdCtrls, JvScrollBar, Forma,
  JvExExtCtrls, JvExtComponent, JvPanel, Menus;

type
  TfrmPreview = class(TForma)
    sBar: TJvStatusBar;
    edt: TJvEditor;
    pnlNavigate: TPanel;
    btnNextFile: TJvUpDown;
    lblPos: TJvLabel;
    tbLine: TJvScrollBar;
    pnlWait: TJvPanel;
    tmrOpen: TTimer;
    PopupMenu1: TPopupMenu;
    Zapiszorygianly1: TMenuItem;
    Zapiszjakotekst1: TMenuItem;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure edtCaretChanged(Sender: TObject; LastCaretX,
      LastCaretY: Integer);
    procedure edtPaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure FormResize(Sender: TObject);
    procedure tmrOpenTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure edtGetLineAttr(Sender: TObject; var Line: String;
      Index: Integer; var Attrs: TLineAttrs);
    procedure btnNextFileClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLineChange(Sender: TObject);
    procedure FormaShow(Sender: TObject);
    procedure Zapiszjakotekst1Click(Sender: TObject);
    procedure Zapiszorygianly1Click(Sender: TObject);
  private
//    LLock: Boolean;
  public
    fcls: TFileClass;
    fName: string;
    fraList: TfraFileList;
  end;

procedure ShowFile(afcls: TFileClass; AFrame: TfraFileList);

implementation

uses unt_Common;
{$R *.dfm}

function GetTemp: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := IncludeTrailingPathDelimiter(StrPas(tempFolder));
end;

procedure ShowFile(afcls: TFileClass; AFrame: TfraFileList);
begin
  if not FileExists(afcls.Name) then Exit;
  with TfrmPreview.Create(AFrame) do
  begin
    fcls := afcls;
    fraList := AFrame;
  end;
end;

procedure TfrmPreview.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  pnlNavigate.DoubleBuffered := True;
  Show;
end;

procedure TfrmPreview.edtCaretChanged(Sender: TObject; LastCaretX,
  LastCaretY: Integer);
begin
  if IsClosing then Exit;
  sBAr.SimpleText := Format('Wiersz: %d, Kolumna: %d', [edt.CaretY+1, edt.CaretX+1]);
end;

procedure TfrmPreview.edtPaintGutter(Sender: TObject; Canvas: TCanvas);
var
  v, n: Integer;
begin
  if IsClosing then Exit;

  Canvas.Font.Size := 10;
  Canvas.Font.Color := clBlack;
  Canvas.Font.Style := [];

  v := Canvas.TextHeight('0') + 1;

  for n := 1 to edt.VisibleRowCount+1 do
    Canvas.TextOut(46 - Canvas.TextWidth(IntToStr(edt.TopRow+n)),
    (n-1)*v,  IntToStr(edt.TopRow+n));

end;

procedure TfrmPreview.FormResize(Sender: TObject);
begin
  edt.Refresh;
  pnlWait.Left := ClientWidth div 2 - pnlWait.Width div 2;
  pnlWait.Top := ClientHeight div 2 - pnlWait.Height div 2;
end;

procedure TfrmPreview.tmrOpenTimer(Sender: TObject);
var
  F1, F2: file;
  Buffer: array[1..4096] of Char;
  NumRead: Integer;
  n, CharPos: Integer;
  bb: Boolean;
begin
  tmrOpen.Enabled := False;
  CharPos := 1;
  Try
    try
      fName := GetTemp + ExtractFileName(fcls.Name) + '.~tmp';
      if FileExists(fName) then DeleteFile(fName);

      Caption := fcls.Name;

      edt.Clear;
      edt.Lines.Clear;
      edt.SetCaret(0, 0);

      if fcls.Size > 1024000 then
      begin
        pnlWait.Visible := True;
        Application.ProcessMessages;
      end;

      AssignFile(F1, fcls.Name);
      Reset(F1, 1);
      try
        FileMode := 0;
        AssignFile(F2, fName);
        try
          Rewrite(F2, 1);
          repeat
            BlockRead(F1, Buffer, SizeOf(Buffer), NumRead);
            for n := Low(Buffer) to High(Buffer) do
            begin
              if Buffer[n] in [#0, #9] then Buffer[n] := #32;

              if CharPos >= 1024 then
                Buffer[n] := #10;

              bb := False;
              if (n < NumRead) then
                if (Buffer[n] = #13) and (Buffer[n+1] <> #10) then bb := True;
              if Buffer[n] = #10 then bb := True;

              if bb then CharPos := 1 else inc(CharPos);

            end;

            BlockWrite(F2, Buffer, NumRead);
          until NumRead = 0;
        finally
          CloseFile(F2);
        end;
      finally
        CloseFile(F1);
      end;

      edt.Lines.LoadFromFile(fName);

      if Length(fcls.Points) > 0 then
      begin
        tbLine.Position := 1;
        tbLine.Max := Length(fcls.Points);
        if tbLine.Max < 20 then tbLine.PageSize := 1 else tbLine.PageSize := 0;

      end
      else
        pnlNavigate.Visible := False;

      edt.LineInformations.Clear;
      edt.LineInformations.BreakpointColor := clYellow;
      edt.LineInformations.BreakpointTextColor := clBlack;
      for n := 0 to High(fcls.Points) do
        edt.LineInformations.SelectStyle[fcls.Points[n].Line-1] := lssBreakpoint;

    finally
      if FileExists(fName) then DeleteFile(fName);
      pnlWait.Visible := False;
    end;

    tbLineChange(nil);
  except
    on E: Exception do
    begin
      Close;
      raise;
    end;
  end;
end;

procedure TfrmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmPreview.FormDestroy(Sender: TObject);
begin
  if FileExists(fName) then DeleteFile(fName);
end;

procedure TfrmPreview.edtGetLineAttr(Sender: TObject; var Line: String;
  Index: Integer; var Attrs: TLineAttrs);
var
  m, n: Integer;
begin
  if IsClosing then Exit;

  for m := Low(fcls.Points) to High(fcls.Points) do
  begin
    if fcls.Points[m].Line <> Index+1 then Continue;
    for n := fcls.Points[m].StrFrom to fcls.Points[m].StrTo do
      if n <= Length(Line) then
        Attrs[n].Style := [fsBold, fsUnderLine];
  end;

end;

procedure TfrmPreview.btnNextFileClick(Sender: TObject; Button: TUDBtnType);
var
  i: Integer;
begin
  i := fraList.lv.ItemIndex;

  if Button = btNext then inc(i)
  else
  if Button = btPrev then dec(i);

  if i < 0 then i := fraList.lv.Items.Count-1;

  if i > fraList.lv.Items.Count-1 then i := 0;

  fraList.lv.ItemIndex := i;
  fcls := TFileClass(fraList.lv.Selected.Data);

  tmrOpen.Enabled := True;

end;

procedure TfrmPreview.tbLineChange(Sender: TObject);
begin
  if not pnlNavigate.Visible then Exit;

  lblPos.Caption := Format('%d/%d', [tbLine.Position, tbLine.Max]);

  edt.CaretY := fcls.Points[tbLine.Position-1].Line-1;
  edt.CaretX := fcls.Points[tbLine.Position-1].StrTo;

  edt.CaretX := fcls.Points[tbLine.Position-1].StrFrom-1;

  if Sender <> nil then edt.SetFocus;
end;

procedure TfrmPreview.FormaShow(Sender: TObject);
begin
  tmrOpen.Enabled := True;
end;

procedure TfrmPreview.Zapiszjakotekst1Click(Sender: TObject);
begin
  inherited;
  dlgSave.FileName := ChangeFileExt(ExtractFileName(fcls.Name), '.txt');
  dlgSave.Filter := 'Pliki tekstowe|*.txt';
  if not dlgSave.Execute then Exit;
  edt.Lines.SaveToFile(dlgSave.FileName);
end;

procedure TfrmPreview.Zapiszorygianly1Click(Sender: TObject);
begin
  inherited;
  dlgSave.FileName := ExtractFileName(fcls.Name);
  dlgSave.Filter := '';
  if not dlgSave.Execute then Exit;
  FileOperation(Handle, foCopy, fcls.Name, dlgSave.FileName);
end;

end.
