unit unt_FileAnalyzer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExMask, JvToolEdit, JvExControls,
  JvEditorCommon, JvEditor, StrUtils, IniFiles, ExtCtrls, ComCtrls;

const
  BuffSize = 8192;

type
  TOnProgressEvent = procedure(Processed, Current, Total: Int64) of object;
  TOnFindPhraseEvent = procedure(Position, LineNr, SelFrom, SelTo: Integer) of object;
  TOnFindPhrase2Event = procedure(Position, LineNr, SelFrom, SelTo: Integer; txt: string) of object;

  TMaskPart = record
    Part:     string[30];
    PartFrom: Integer;
    PartTo:   Integer;
    Length:   Integer;
    Mode:     Char;
  end;

  TFileAnalyzer = class
  private


    str: string;
    //res: string;

    LineNr: Integer;
    Buffer: array[1..BuffSize] of Char; // bufor odczytu pliku
    Buff: array[1..BuffSize*2] of Char; // bufor lini - upper case
    BuffC: array[1..BuffSize*2] of Char; // bufor zawierajacy jedna linie
    BuffSize: Integer;

    Masks: array of array of TMaskPart;

    procedure ProcessLine;
  public
    Abort: Boolean;
    FileName: string;
    UseMask: Boolean;
    TotalSize: Int64;
    Progress: Int64;
    TextCharCount: Int64;
    CaseSensitive: Boolean;
    OnProgress: TOnProgressEvent;
    OnFindPhrase: TOnFindPhraseEvent;
    OnFindPhrase2: TOnFindPhrase2Event;
    procedure AnalyzeFile(mask: string);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TFileAnalyzer }


procedure TFileAnalyzer.AnalyzeFile(mask: string);
var
  F: file;
  NumRead: Integer;
  m, n: Integer;
  CharPos: Integer;
  bb: Boolean;
  stl: TStringList;
  stc: TStringList;
begin
  Abort := False;
  stl := TStringList.Create;
  stc := TStringList.Create;
  try
    if CaseSensitive then str := mask else str := UpperCase(mask);

    if UseMask then
    begin
      stl.Text := AnsiReplaceStr(str, '|', #13#10);
      SetLength(Masks, stl.Count);

      for m := 0 to stl.Count-1 do
      begin
        stc.Text := AnsiReplaceStr(stl[m], '*', #13#10 + '*');
        stc.Text := AnsiReplaceStr(stc.Text, '^', #13#10 + '^');
        stc.Text := AnsiReplaceStr(stc.Text, '_', #13#10 + '_');

        for n := stc.Count-1 downto 0 do
          if stc[n] = '' then stc.Delete(n);

        SetLength(Masks[m], stc.Count);

        for n := 0 to stc.Count-1 do
        begin
          if n = 0 then
          begin
            Masks[m][n].Part := stc[n];
            Masks[m][n].Mode := #0;
          end
          else
          begin
            Masks[m][n].Mode := stc[n][1];
            Masks[m][n].Part := Copy(stc[n], 2, Length(stc[n])-1);
          end;

          Masks[m][n].Length := Length(Masks[m][n].Part);
        end;
      end;
    end
    else
    begin
       SetLength(Masks, 1);
       SetLength(Masks[0], 1);
       Masks[0][0].Part := str;
       Masks[0][0].Length := Length(str);
    end;

   { if Pos('umasks.dcu', LowerCase(FileName)) > 0 then
      Sleep(0); }
    AssignFile(F, FileName);
    FileMode := 0;
    Reset(F, 1);

    CharPos := 1;
    LineNr := 0;
    Progress := 0;
    TextCharCount := 0;
   // TotalSize := FileSize(edtFileName.Text);

    TotalSize := FileSize(F);

    repeat
      if Abort then Break;
      BlockRead(F, Buffer, SizeOf(Buffer), NumRead);

      for m := 1 to NumRead do
      begin
        Inc(Progress);
        bb := False;

        if CharPos >= 1024 then
          Buffer[m] := #10;

        if Buffer[m] in [#9, #10, #13, #32..#127] then inc(TextCharCount);

        if (m < NumRead) then
          if (Buffer[m] = #13) and (Buffer[m+1] <> #10) then bb := True;
        if Buffer[m] = #10 then bb := True;

        if bb then
        begin
          BuffSize := CharPos-1;
          ProcessLine;
          CharPos := 1;
        end
        else
        begin
          if CharPos > Length(Buff) then CharPos := 1;
          buff[CharPos] := Buffer[m];
          inc(CharPos);
        end;
      end;

      if Assigned(OnProgress) then
        OnProgress(NumRead, Progress, TotalSize);

    until NumRead = 0;

    CloseFile(F) ;
  finally
    for n := Low(Masks) to High(Masks) do
      SetLength(Masks[n], 0);
    SetLength(Masks, 0);

    stc.Free;
    stl.Free;

  end;

 // Caption := 'predkosc: ' + FileSize( Round(TotalSize / (tc/1000))) + '/s';

 // memo1.Lines.Text := Trim(ResultBuff);

 { mmo.Lines.Text := 'Fraza: "' + str + '"' + #13#10
    + 'Znaleziono ' + IntToStr(ResCnt) + ' fraz' + #13#10
    + 'Czas: ' + IntToStr(tc) + #13#10
    + 'Linie: ' + IntToStr(LineCnt) + #13#10
    //+ 'Ansi: ' + IntToStr(AnsiCharCnt) + ', nie Ansi: ' + IntToStr(NonAnsiCharCnt) + #13#10
    + res;

    pbar.Position := 1000;    }

end;

constructor TFileAnalyzer.Create;
begin

end;

destructor TFileAnalyzer.Destroy;
begin

  inherited;
end;

procedure TFileAnalyzer.ProcessLine;
var
  PartFrom, PartTo: Integer;
  pm: ^TMaskPart;

  function FindPart: Boolean;
  var
    m: integer; //nr znaku maski
    n: Integer; // nr znaku tekstu

    LFound: Boolean;
  begin
    Result := False;
    m := 1;

    PartFrom := PartTo;

    n := PartFrom;
    while n < BuffSize do
    begin
      inc(n);

      if UseMask then
      begin
        LFound := (Buff[n] = pm.Part[m]) or (pm.Part[m] in ['?', '#', '$', '@']);
        if LFound and (pm.Part[m] in ['#', '$', '@']) then
          if Buff[n] in [#32..#126] then
          begin
            if (pm.Part[m] = '#') and (Buff[n] = #32) then LFound := False
            else
            if (pm.Part[m] = '$') and (not (Buff[n] in [#48..#57])) then LFound := False
            else
            if (pm.Part[m] = '%') and (not (Buff[n] in [#65..#90, #97..#122])) then LFound := False
            else
            if (pm.Part[m] = '@') and(not (Buff[n] in
              [#33..#47, #58..#64, #91..#96, #123..#126]))
            then LFound := False
          end
          else
            LFound := False;
      end
      else
        LFound := Buff[n] = pm.Part[m];

      if LFound then inc(m)
      else
      begin
        if (m > 1) then Dec(n);
        m := 1;
      end;

      if m = 2 then PartFrom := n;

      if m > pm.Length then
      begin
        Result := True;
        PartTo := n;
        Break;
      end
    end;
  end;

var
  m, n, i: Integer;
  StrFrom, StrTo: Integer;
  bb, LFound: Boolean;
begin
  Inc(LineNr);

  if BuffSize= 0 then Exit;

  for n := 1 to BuffSize do
    if Buff[n] <> #0 then BuffC[n] := Buff[n] else BuffC[n] := #32;


  if not CaseSensitive then
    CharUpperBuff(@Buff[1], BuffSize);


  for m := Low(Masks) to High(Masks) do
  begin
    LFound := True;

    StrTo := 0;
    PartFrom := 0;
    PartTo := 0;


    while LFound do
    begin
      StrFrom := 0;

      if UseMask then
        for n := Low(Masks[m]) to High(Masks[m]) do
        begin
          pm := @masks[m][n];

          if pm.Part <> '' then
          begin
            bb := FindPart;

            if bb and (n > 0) then
              if pm.Mode <> '*' then   // jesli lacznikiem nie jest *
              begin
                if pm.Mode = '^' then
                  for i := StrTo+1 to PartFrom-1 do
                    if not (Buff[i] in [#48..#57, #65..#90, #97..#122]) then
                    begin
                      bb := False;
                      Break;
                    end;

                if pm.Mode = '_' then
                  for i := StrTo+1 to PartFrom-1 do
                    if not (Buff[i] in [#9, #32, '_']) then  //SPACJA lub TAB
                    begin
                      bb := False;
                      Break;
                    end;
              end;

            if not bb then
            begin
              LFound := False;
              Break;
            end
            else
            begin
              if StrFrom = 0 then StrFrom := PartFrom;
              if PartTo > StrTo then StrTo := PartTo;
            end;
          end;
        end
      else
      begin
        pm := @masks[m][0];
        if not FindPart then
        begin
          LFound := False;
          Break;
        end
        else
        begin
          if StrFrom = 0 then StrFrom := PartFrom;
          if PartTo > StrTo then StrTo := PartTo;
        end;
      end;


      if LFound then
      begin
        if Assigned(OnFindPhrase) then
           OnFindPhrase(Progress, LineNr, StrFrom, StrTo);

        if Assigned(OnFindPhrase2) then
          OnFindPhrase2(Progress, LineNr, StrFrom, StrTo,
            AnsiReplaceStr(Copy(BuffC, 1, BuffSize), #0, #32));

      end;




     { begin




         res := res + IntToStr(ResCnt) + '. ' + #9 + IntToStr(LineCnt) + ':' + #9 +  '"'
            + Copy(BuffC, StrFrom, StrTo - StrFrom + 1) +'"' + #9 + ' -> '
            + Copy(BuffC, 1, BuffSize) + #13#10;

      end;    }



    end;


  end;

end;

end.




