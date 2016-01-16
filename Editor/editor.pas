unit Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  AALHighlighter, Types, contnrs, LCLType, ExtCtrls, AALTypes, UnitParser,
  Dialogs, Graphics, StdCtrls, Buttons, ComCtrls, strutils, CodeFormatter,
  ToolTip, ListRecords, SynEditTypes, Math;

type

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    CodeExplorerPanel: TPanel;
    CodeExplorerHead: TPanel;
    CodeExplorerImages: TImageList;
    Label2: TLabel;
    SearchButton: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    CodeEditor: TSynEdit;
    Completion: TSynCompletion;
    SearchEdit: TEdit;
    Label1: TLabel;
    SearchBar: TPanel;
    ReplaceEdit: TEdit;
    SelectHighlightTimer: TTimer;
    CheckSelTimer: TTimer;
    CloseSearchButton: TSpeedButton;
    CloseCodeExplorerButton: TSpeedButton;
    ToolTipTimer: TTimer;
    CodeExplorer: TTreeView;
    procedure CheckSelTimerTimer(Sender: TObject);
    procedure CloseCodeExplorerButtonClick(Sender: TObject);
    procedure CloseSearchButtonClick(Sender: TObject);
    procedure CodeEditorChange(Sender: TObject);
    procedure CodeEditorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CodeEditorMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure CodeEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CodeExplorerDblClick(Sender: TObject);
    procedure CompletionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    procedure CompletionSearchPosition(var APosition: integer);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure ReplaceEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SearchEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SelectHighlightTimerTimer(Sender: TObject);
    procedure ToolTipTimerTimer(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FOpenEditor: TOpenEditorEvent;
    topl: integer;
    FToolTip: TEditorToolTip;
    currFunc: string;
    currInfo: string;
    FOnParserFinished: TNotifyEvent;
    moveright: boolean;
    currWord: string;
    FOnChange: TNotifyEvent;
    Highlight: TAALSynHighlight;
    FFunctions: TFuncList;
    FRequiredFiles: TStringList;
    FVars: TVarList;
    FStdFunc: TFuncList;
    FFileName: string;
    FKeyWords: TStringList;
    FDefRanges: TObjectList;
    Parser: TUnitParser;
    procedure CompleteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    function GetCurrWord: string;
    function GetFont: TFont;
    procedure SetFont(f: TFont);
    procedure SetRanges(l: TObjectList);
    procedure SetFunc(l: TFuncList);
    procedure SetVar(l: TVarList);
    function GetAtCursor(x, y: integer): string;
    procedure ParserHasFinished(Sender: TObject);
    { private declarations }
  public
    procedure MoveHorz(i: IntPtr);
    procedure MoveVert(i: IntPtr);
    procedure ShowSearch;
    procedure SetFocus; override;
    procedure CodeJump(p: TPoint);
    procedure StartFormatter;
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property FunctionList: TFuncList read FFunctions write SetFunc;
    property VariableList: TVarList read FVars write SetVar;
    property FileName: string read FFileName write FFilename;
    property DefRanges: TObjectList read FDefRanges write SetRanges;
    property Font: TFont read GetFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnParserFinished: TNotifyEvent read FOnParserFinished
      write FOnParserFinished;
    property RequiredFiles: TStringList read FRequiredFiles;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    { public declarations }
  end;

implementation

{$R *.lfm}

procedure TEditorFrame.MoveHorz(i: IntPtr);
var
  p: TPoint;
begin
  p := CodeEditor.LogicalCaretXY;
  p.x := p.x + i;
  while Length(CodeEditor.Lines[p.y - 1]) < p.x - 1 do
    CodeEditor.Lines[p.y - 1] := CodeEditor.Lines[p.y - 1] + ' ';
  CodeEditor.LogicalCaretXY := p;
end;

procedure TEditorFrame.MoveVert(i: IntPtr);
var
  p: TPoint;
begin
  p := CodeEditor.LogicalCaretXY;
  p.y := p.y + i;
  while CodeEditor.Lines.Count < p.y do
    CodeEditor.TextBetweenPoints[Point(
      Length(CodeEditor.Lines[CodeEditor.Lines.Count - 1]), CodeEditor.Lines.Count),
      Point(Length(CodeEditor.Lines[CodeEditor.Lines.Count - 1]),
      CodeEditor.Lines.Count)] := '#13';
  CodeEditor.LogicalCaretXY := p;
end;

function TEditorFrame.GetFont: TFont;
begin
  Result := CodeEditor.Font;
end;

procedure TEditorFrame.SetFont(f: TFont);
begin
  CodeEditor.Font := f;
end;

procedure TEditorFrame.SetRanges(l: TObjectList);
var
  i: integer;
begin
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges[i].Free;
  FDefRanges.Clear;
  for i := 0 to l.Count - 1 do
    FDefRanges.Add(l[i]);
end;


procedure TEditorFrame.ShowSearch;
begin
  SearchBar.Show;
  SearchEdit.SetFocus;
end;

constructor TEditorFrame.Create(TheOwner: TComponent);

  procedure LoadFuncList;
  var
    sl1, sl2: TStringList;
    i: integer;
  begin
    sl1 := TStringList.Create;
    sl2 := TStringList.Create;
    try
      sl1.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
        'Funcs.lst');
      for i := 0 to sl1.Count - 1 do
        if sl1[i][1] = ';' then
          sl2.Add(Copy(sl1[i], 2, Length(sl1[i])))
        else
        begin
          FStdFunc.Add(FuncInfo(sl1[i], -1, sl2.Text));
          sl2.Clear;
        end;
    finally
      sl1.Free;
      sl2.Free;
    end;
  end;

begin
  inherited;
  currInfo := '';
  Completion.OnKeyDown := @CompleteKeyDown;
  CodeEditor.Lines.Add('');
  FOnChange := nil;
  FToolTip := TEditorToolTip.Create(self);
  FToolTip.Parent := CodeEditor;
  moveright := True;
  Parser := TUnitParser.Create(True);
  Highlight := TAALSynHighlight.Create(nil);
  Highlight.LoadConfig(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'HL');
  CodeEditor.Highlighter := Highlight;
  FFunctions := TFuncList.Create;
  FVars := TVarList.Create;
  FStdFunc := TFuncList.Create;
  FKeyWords := TStringList.Create;
  FDefRanges := TObjectList.Create(False);
  FRequiredFiles := TStringList.Create;
  LoadFuncList;
  FKeyWords.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'Keywords.lst');
  UpdateTimerTimer(nil);
  currWord := '';
end;

procedure TEditorFrame.SetFocus;
begin
  inherited;
  CodeEditor.SetFocus;
end;

procedure TEditorFrame.ParserHasFinished(Sender: TObject);
var
  i: integer;
  FE, VE, IE: boolean;
  FS, VS: integer;
  InE: array of boolean;
  p: TTreeNode;
begin
  CodeExplorer.BeginUpdate;
  try
    FE := CodeExplorer.Items.FindNodeWithText('Funktionen').Expanded;
    IE := CodeExplorer.Items.FindNodeWithText('Includes').Expanded;
    VE := CodeExplorer.Items.FindNodeWithText('Variablen').Expanded;
    SetLength(InE, FRequiredFiles.Count);
    FillChar(InE[0], SizeOf(boolean) * FRequiredFiles.Count, $00);
    for i := 0 to FRequiredFiles.Count - 1 do
      if Assigned(CodeExplorer.Items.FindNodeWithText(FRequiredFiles[i])) then
        InE[i] := CodeExplorer.Items.FindNodeWithText(FRequiredFiles[i]).Expanded;
    i := 0;
    while CodeExplorer.Items.Count > 3 do
      if CodeExplorer.Items[i].ImageIndex <> 0 then
        CodeExplorer.Items.Delete(CodeExplorer.Items[i])
      else
        Inc(i);
    for i := 0 to FRequiredFiles.Count - 1 do
      with CodeExplorer.Items.AddChild(CodeExplorer.Items.FindNodeWithText('Includes'),
          FRequiredFiles[i]) do
      begin
        ImageIndex := 1;
        SelectedIndex := 1;
        Data := Pointer(i);
      end;
    for i := 0 to FFunctions.Count - 1 do
      with CodeExplorer.Items.AddChild(CodeExplorer.Items.FindNodeWithText('Funktionen'),
          FFunctions[i].Name) do
      begin
        ImageIndex := 2;
        SelectedIndex := 2;
        Data := Pointer(i);
      end;
    for i := 0 to FVars.Count - 1 do
      with CodeExplorer.Items.AddChild(CodeExplorer.Items.FindNodeWithText('Variablen'),
          FVars[i].Name) do
      begin
        ImageIndex := 3;
        SelectedIndex := 3;
        Data := Pointer(i);
      end;
    FS := FFunctions.Count;
    VS := FVars.Count;
    if Assigned(FOnParserFinished) then
      FOnParserFinished(Self);
    for i := FS to FFunctions.Count - 1 do
    begin
      p := CodeExplorer.Items.FindNodeWithText(CreateRelativePath(
        FFunctions[i].FileName, ExtractFilePath(FFileName), True));
      if Assigned(p) then
        with CodeExplorer.Items.AddChild(p, FFunctions[i].Name) do
        begin
          ImageIndex := 2;
          SelectedIndex := 2;
          Data := Pointer(i);
        end;
    end;
    for i := VS to FVars.Count - 1 do
    begin
      p := CodeExplorer.Items.FindNodeWithText(CreateRelativePath(
        FVars[i].FileName, ExtractFilePath(FFileName), True));
      if Assigned(p) then
        with CodeExplorer.Items.AddChild(p, FVars[i].Name) do
        begin
          ImageIndex := 3;
          SelectedIndex := 3;
          Data := Pointer(i);
        end;
    end;
    CodeExplorer.Items.FindNodeWithText('Funktionen').Expanded := FE;
    CodeExplorer.Items.FindNodeWithText('Includes').Expanded := IE;
    CodeExplorer.Items.FindNodeWithText('Variablen').Expanded := VE;
    for i := 0 to FRequiredFiles.Count - 1 do
      if Assigned(CodeExplorer.Items.FindNodeWithText(FRequiredFiles[i])) then
        CodeExplorer.Items.FindNodeWithText(FRequiredFiles[i]).Expanded := InE[i];
  finally
    CodeExplorer.EndUpdate;
  end;
end;

procedure TEditorFrame.CompletionExecute(Sender: TObject);

  function StringsContain(s: TStrings; str: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to s.Count - 1 do
      if LowerCase(str) = LowerCase(s[i]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  i, x: integer;
begin
  Completion.ItemList.Clear;
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.AddStrings(FKeyWords);
    for i := 0 to FStdFunc.Count - 1 do
      Completion.ItemList.Add(FStdFunc[i].Name);
    for i := 0 to FFunctions.Count - 1 do
      Completion.ItemList.Add(FFunctions[i].Name);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    for i := 0 to FVars.Count - 1 do
      if ((FVars[i].Line <= CodeEditor.LogicalCaretXY.y - 1) or
        (FVars[i].FileName <> '')) and
        (Pos(LowerCase(Completion.CurrentString), LowerCase(FVars[i].Name)) = 1) then
        Completion.ItemList.Add(FVars[i].Name);
    for x := 0 to FDefRanges.Count - 1 do
      if (CodeEditor.LogicalCaretXY.y - 1 >= (FDefRanges[x] as TDefRange).StartLine) and
        (CodeEditor.LogicalCaretXY.y - 1 < (FDefRanges[x] as TDefRange).EndLine) then
        for i := 0 to (FDefRanges[x] as TDefRange).Vars.Count - 1 do
          if ((FDefRanges[x] as TDefRange).Vars[i].Line <=
            CodeEditor.LogicalCaretXY.y - 1) and
            (Pos(LowerCase(Completion.CurrentString), LowerCase(
            (FDefRanges[x] as TDefRange).Vars[i].Name)) = 1) then
            Completion.ItemList.Add((FDefRanges[x] as TDefRange).Vars[i].Name);
    if not StringsContain(Completion.ItemList, Completion.CurrentString) then
      Completion.ItemList.Add(Completion.CurrentString);
  end
  else
  begin
    for i := 0 to FKeyWords.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FKeyWords[i])) = 1 then
        Completion.ItemList.Add(FKeyWords[i]);
    for i := 0 to FStdFunc.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FStdFunc[i].Name)) = 1 then
        Completion.ItemList.Add(FStdFunc[i].Name);
    for i := 0 to FFunctions.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FFunctions[i].Name)) = 1 then
        Completion.ItemList.Add(FFunctions[i].Name);
    Completion.ItemList.Add(Completion.CurrentString);
  end;
  Completion.ItemList.Add('');
  Completion.Position := 0;
end;

procedure TEditorFrame.Save(p: string = '');
begin
  if (p = '') and (FFileName <> '') then
    p := FFileName;
  if (p <> '') then
  begin
    CodeEditor.Lines.SaveToFile(p);
    CodeEditor.MarkTextAsSaved;
    FFileName := p;
  end;
end;

procedure TEditorFrame.Load(p: string = '');
begin
  if (p = '') and (FFileName <> '') then
    p := FFileName;
  if (p <> '') then
  begin
    CodeEditor.Lines.LoadFromFile(p);
    FFileName := p;
    UpdateTimerTimer(nil);
  end;
end;

procedure TEditorFrame.CompleteKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = 8) and (Length(Completion.CurrentString) > 0) and
    (Completion.CurrentString[1] = '$') then
    Completion.Deactivate
  else
  {if (key in [17, 18]) then
    Completion.Deactivate
  else }if key = 9 then
    key := 13;

end;

procedure TEditorFrame.CompletionSearchPosition(var APosition: integer);

  function StringsContain(s: TStrings; str: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to s.Count - 1 do
      if LowerCase(str) = LowerCase(s[i]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  i, x: integer;
begin
  Completion.ItemList.Clear;
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.AddStrings(FKeyWords);
    for i := 0 to FStdFunc.Count - 1 do
      Completion.ItemList.Add(FStdFunc[i].Name);
    for i := 0 to FFunctions.Count - 1 do
      Completion.ItemList.Add(FFunctions[i].Name);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    for i := 0 to FVars.Count - 1 do
      if ((FVars[i].Line <= CodeEditor.LogicalCaretXY.y - 1) or
        (FVars[i].FileName <> '')) and
        (Pos(LowerCase(Completion.CurrentString), LowerCase(FVars[i].Name)) = 1) then
        Completion.ItemList.Add(FVars[i].Name);
    for x := 0 to FDefRanges.Count - 1 do
      if (CodeEditor.LogicalCaretXY.y - 1 >= (FDefRanges[x] as TDefRange).StartLine) and
        (CodeEditor.LogicalCaretXY.y - 1 < (FDefRanges[x] as TDefRange).EndLine) then
        for i := 0 to (FDefRanges[x] as TDefRange).Vars.Count - 1 do
          if ((FDefRanges[x] as TDefRange).Vars[i].Line <=
            CodeEditor.LogicalCaretXY.y - 1) and
            (Pos(LowerCase(Completion.CurrentString), LowerCase(
            (FDefRanges[x] as TDefRange).Vars[i].Name)) = 1) then
            Completion.ItemList.Add((FDefRanges[x] as TDefRange).Vars[i].Name);
    if not StringsContain(Completion.ItemList, Completion.CurrentString) then
      Completion.ItemList.Add(Completion.CurrentString);
  end
  else
  begin
    for i := 0 to FKeyWords.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FKeyWords[i])) = 1 then
        Completion.ItemList.Add(FKeyWords[i]);
    for i := 0 to FStdFunc.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FStdFunc[i].Name)) = 1 then
        Completion.ItemList.Add(FStdFunc[i].Name);
    for i := 0 to FFunctions.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FFunctions[i].Name)) = 1 then
        Completion.ItemList.Add(FFunctions[i].Name);
    Completion.ItemList.Add(Completion.CurrentString);
  end;
  Completion.ItemList.Add('');
  Completion.Position := min(Max(Completion.Position, 0), Completion.ItemList.Count - 1);
end;

procedure TEditorFrame.ReplaceAllButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace, ssoReplaceAll],
    Point(0, 0)) = 0 then
    ShowMessage('Keine Ergebnisse gefunden');
end;

procedure TEditorFrame.ReplaceButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace],
    CodeEditor.BlockEnd) = 0 then
    if MessageDlg('Dateiende Erreicht',
      'Die Suche hat das Dateiende erreicht, von Dateianfang erneut suchen?',
      mtConfirmation, mbYesNo, 'Suche') = mrYes then
      if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace],
        Point(0, 0)) = 0 then
        ShowMessage('Keine Ergebnisse gefunden');
end;

procedure TEditorFrame.SearchButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [], CodeEditor.BlockEnd) = 0 then
    if MessageDlg('Dateiende Erreicht',
      'Die Suche hat das Dateiende erreicht, von Dateianfang erneut suchen?',
      mtConfirmation, mbYesNo, 'Suche') = mrYes then
      if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [], Point(0, 0)) = 0 then
        ShowMessage('Keine Ergebnisse gefunden');
end;

procedure TEditorFrame.ReplaceEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
    ReplaceButtonClick(nil);
end;

procedure TEditorFrame.SearchEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
    SearchButtonClick(nil);
end;

procedure TEditorFrame.SelectHighlightTimerTimer(Sender: TObject);
begin
  SelectHighlightTimer.Enabled := False;
  Highlight.SelectedText := currWord;
  CodeEditor.Invalidate;
end;

procedure TEditorFrame.ToolTipTimerTimer(Sender: TObject);
begin
  ToolTipTimer.Enabled := False;
  FToolTip.Info := currInfo;
  FToolTip.Func := currFunc;
  FToolTip.ShowAt(CodeEditor.CaretXPix, CodeEditor.CaretYPix, CodeEditor.LineHeight);
  FToolTip.BringToFront;
end;

procedure TEditorFrame.UpdateTimerTimer(Sender: TObject);
begin
  if Trim(CodeEditor.Lines.Text) = '' then
    Exit;
  if Parser.Finished or Parser.Suspended then
  begin
    Parser.Free;
    Parser := TUnitParser.Create(True);
    Parser.OnFinished := @ParserHasFinished;
    Parser.Text := CodeEditor.Lines.Text;
    Parser.Funcs := FFunctions;
    Parser.Vars := FVars;
    Parser.RequiredFiles := FRequiredFiles;
    Parser.Ranges := FDefRanges;
    Parser.Start;
  end;
end;

procedure TEditorFrame.CompletionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  p: integer;
  ln, rpval: string;
begin
  ln := CodeEditor.Lines[SourceStart.y - 1];
  moveright := False;
  if Length(Value) = 0 then
    Value := Completion.CurrentString
  else if (Value[1] = '$') then
  begin
    if Value[Length(Value)] = ']' then
      Application.QueueAsyncCall(@MoveHorz, -1)
    else if isEnd(ln, 'dim') and AnsiEndsStr(Value, Trim(ln)) then
    begin
      Value := Value + '[]';
      Application.QueueAsyncCall(@MoveHorz, -1);
    end
    else if Completion.CurrentString = Trim(ln) then
      Value := Value + ' = '
    else if (SourceEnd.x = SourceStart.x + Length(Completion.CurrentString)) and
      not isEnd(ln, 'global') then
      Value := Value + ' ';
  end
  else
  begin
    p := Pos('(', Value);
    if p > 0 then
    begin
      if Value[p + 1] <> ')' then
        Application.QueueAsyncCall(@MoveHorz, -1);
      SetLength(Value, Pos('(', Value));
      Value := Value + ')';
    end
    else if isEnd(ln, 'func') and AnsiEndsStr(Value, TrimRight(ln)) then
    begin
      Value := Value + '()';
      Application.QueueAsyncCall(@MoveHorz, -1);
    end
    else if (pos(Completion.CurrentString, ln) > 0) and
      (not ((pos(Completion.CurrentString, ln) + Length(Completion.CurrentString) <=
      length(ln)) and (ln[pos(Completion.CurrentString, ln) +
      Length(Completion.CurrentString)] in [#0..#32]))) then
      Value := Value + ' ';
  end;
  if (Length(Value) > 0) and not (Value[1] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) then
    Value := Copy(Value, 2, Length(Value) - 1);
  (*rpval := Value;
  Value := Copy(ln, SourceStart.x, PosEx(Completion.CurrentString, ln, SourceStart.x))+
    Value+
    Copy(ln, PosEx(Completion.CurrentString, ln, SourceStart.x)+Length(Completion.CurrentString)+1,
    SourceEnd.x-PosEx(Completion.CurrentString, ln, SourceStart.x)+Length(Completion.CurrentString));
  Application.QueueAsyncCall(@MoveHorz, -(Length(Value) -
    (Pos(rpval, Value) + Length(rpval) - 1)));*)
end;

procedure TEditorFrame.CodeEditorKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);

  function GotClosed(i: integer; sTok, eTok: string): boolean;
  var
    counter, c: integer;
  begin
    while (i >= 0) do
      if isEnd(CodeEditor.Lines[i], 'func') or isEnd(CodeEditor.Lines[i], 'endfunc') then
      begin
        Inc(i);
        Break;
      end
      else
        Dec(i);
    c := CodeEditor.Lines.Count;
    counter := 0;
    while (i < c) and (not (isEnd(LowerCase(CodeEditor.Lines[i]), 'endfunc'))) and
      (not (isEnd(LowerCase(CodeEditor.Lines[i]), 'func'))) do
    begin
      if isEnd(LowerCase(CodeEditor.Lines[i]), sTok) then
        Inc(counter)
      else if isEnd(LowerCase(CodeEditor.Lines[i]), eTok) then
        Dec(counter);
      Inc(i);
    end;
    Result := counter <= 0;
  end;

var
  ln, pref, tmp: string;
  i, x, l: integer;
  b: boolean;
  p: TPoint;
begin
  if (Key = 13) and moveright then
  begin
    ln := LowerCase(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 2]);
    i := 1;
    l := Length(ln);
    while (i <= l) and (ln[i] in [#0..#32]) do
      Inc(i);
    pref := Copy(ln, 1, i - 1);
    i := CodeEditor.LogicalCaretXY.y - 2;
    if isEnd(ln, 'while') then
    begin
      if not GotClosed(i, 'while', 'wend') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'WEnd';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'for') then
    begin
      if not GotClosed(i, 'for', 'next') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'Next';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'if') then
    begin
      if not GotClosed(i, 'if', 'endif') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndIf';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'func') then
    begin
      b := True;
      for x := i + 1 to CodeEditor.Lines.Count - 1 do
        if isEnd(CodeEditor.Lines[x], 'func') then
          break
        else if isEnd(CodeEditor.Lines[x], 'endfunc') then
        begin
          b := False;
          break;
        end;
      if b then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndFunc';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end;
  end
  else if (Key = Ord('D')) and (ssCtrl in Shift) then
    StartFormatter
  else if (Key = Ord('F')) and (ssCtrl in Shift) then
    ShowSearch;
  moveright := True;
  if key in [Ord('A')..Ord('Z'), Ord('0')..Ord('9')] then
  begin
    tmp := GetCurrWord;
    p := Point(CodeEditor.CaretXPix, CodeEditor.CaretYPix + CodeEditor.LineHeight);
    p := CodeEditor.ClientToScreen(p);
    if (Length(tmp) > 1) and (tmp[1] = '$') then
      Completion.Execute(GetCurrWord, p);
  end;
end;


function TEditorFrame.GetAtCursor(x, y: integer): string;
var
  p: TPoint;
  s: integer;
  i: integer;
  len: integer;
  slen: integer;
  ln: string;
begin
  Result := '';
  p := CodeEditor.PixelsToLogicalPos(Point(x, y));
  ln := CodeEditor.Lines[p.y - 1];
  if ln = '' then
    Exit;
  slen := Length(ln);
  i := p.x - 1;
  len := 0;

  if i < 1 then
    i := 1;
  if (i < slen) and (ln[i + 1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']) and
    ((i > 0) or (ln[i] in [#0..#32])) then
    Inc(i);

  while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
    Dec(i);

  if (i > 0) and (ln[i] = '$') then
  begin
    Inc(len);
    s := i;
    Inc(i);
  end
  else
  begin
    Inc(i);
    s := i;
  end;

  while (i <= slen) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
  begin
    Inc(i);
    Inc(len);
  end;
  Result := Copy(ln, s, len);
end;

procedure TEditorFrame.CodeEditorMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);

  function GetCurrFunc(sel: string; out f: TFuncInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
      if LowerCase(Copy(FFunctions[i].Name, 1, Pos('(', FFunctions[i].Name) - 1)) =
        LowerCase(sel) then
      begin
        Result := True;
        f := FFunctions[i];
        if f.FileName = '' then
          exit;
      end;
  end;

  function GetCurrVar(sel: string; out v: TVarInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FVars.Count - 1 do
    begin
      if isEnd(FVars[i].Name, sel) then
      begin
        Result := True;
        v := FVars[i];
        if v.FileName = '' then
          exit;
      end;
    end;
  end;

var
  l, pos, i, n: integer;
  sel: string;
  f: TFuncInfo;
  v: TVarInfo;
begin
  if ssCtrl in Shift then
  begin
    if (CodeEditor.Lines.Count < CodeEditor.LinesInWindow) and
      (y > CodeEditor.Lines.Count * CodeEditor.LineHeight) then
    begin
      Highlight.JumpItem := SelectedItem(-1, 0);
      CodeEditor.Invalidate;
      Exit;
    end;
    l := CodeEditor.PixelsToLogicalPos(Point(x, y)).y - 1;
    pos := CodeEditor.PixelsToLogicalPos(Point(x, y)).x - 1;
    sel := GetAtCursor(x, y);
    if GetCurrFunc(sel, f) or GetCurrVar(sel, v) then
    begin
      Highlight.JumpItem := SelectedItem(l, pos);
      CodeEditor.Invalidate;
      Exit;
    end
    else
      for n := 0 to FDefRanges.Count - 1 do
        for i := 0 to (FDefRanges[n] as TDefRange).Vars.Count - 1 do
          if (l >= (FDefRanges[n] as TDefRange).StartLine) and
            (l <= (FDefRanges[n] as TDefRange).EndLine) and
            ((FDefRanges[n] as TDefRange).Vars[i].Name = sel) then
          begin
            if (FDefRanges[n] as TDefRange).Vars[i].Line > l then
              Continue;
            Highlight.JumpItem := SelectedItem(l, pos);
            CodeEditor.Invalidate;
            Exit;
          end;
    Highlight.JumpItem := SelectedItem(-1, 0);
    CodeEditor.Invalidate;
  end;
end;

procedure TEditorFrame.CodeJump(p: TPoint);
begin
  CodeEditor.LogicalCaretXY := p;
  CodeEditor.TopLine := p.y;
  CodeEditor.SetFocus;
end;

procedure TEditorFrame.CodeEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

  function GetCurrFunc(sel: string; out f: TFuncInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
      if LowerCase(Copy(FFunctions[i].Name, 1, Pos('(', FFunctions[i].Name) - 1)) =
        LowerCase(sel) then
      begin
        Result := True;
        f := FFunctions[i];
        if f.FileName = '' then
          exit;
      end;
  end;

  function GetCurrVar(sel: string; out v: TVarInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FVars.Count - 1 do
    begin
      if isEnd(FVars[i].Name, sel) then
      begin
        Result := True;
        v := FVars[i];
        if v.FileName = '' then
          exit;
      end;
    end;
  end;

var
  sel: string;
  i, n, l: integer;
  v: TVarInfo;
  f: TFuncInfo;
begin
  Highlight.JumpItem := SelectedItem(-1, 0);
  CodeEditor.Invalidate;
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    CodeEditor.LogicalCaretXY := CodeEditor.PixelsToLogicalPos(Point(x, y));
    sel := GetCurrWord;
    if GetCurrFunc(sel, f) then
    begin
      if (f.FileName = '') then
        CodeJump(Point(Pos(f.Name, CodeEditor.Lines[f.Line]), f.Line + 1))
      else if Assigned(FOpenEditor) then
        FOpenEditor(f.FileName, Point(1, f.Line + 1));
    end
    else if GetCurrVar(sel, v) then
    begin
      if (v.FileName = '') then
      begin
        if pos('[', v.Name) > 0 then
          CodeJump(Point(Pos(Copy(v.Name, 1, Pos('[', v.Name) - 1),
            CodeEditor.Lines[v.Line]), v.Line + 1))
        else
          CodeJump(Point(v.Pos, v.Line + 1));
      end
      else if Assigned(FOpenEditor) then
        FOpenEditor(v.FileName, Point(v.Pos + 1, v.Line + 1));
    end
    else
    begin
      l := CodeEditor.LogicalCaretXY.y - 1;
      for n := 0 to FDefRanges.Count - 1 do
        for i := 0 to (FDefRanges[n] as TDefRange).Vars.Count - 1 do
          if (l >= (FDefRanges[n] as TDefRange).StartLine) and
            (l <= (FDefRanges[n] as TDefRange).EndLine) and
            ((FDefRanges[n] as TDefRange).Vars[i].Name = sel) then
          begin
            if (FDefRanges[n] as TDefRange).Vars[i].Line > l then
              Continue;
            CodeJump(Point((FDefRanges[n] as TDefRange).Vars[i].Pos,
              (FDefRanges[n] as TDefRange).Vars[i].Line + 1));
          end;
    end;
  end;

end;

procedure TEditorFrame.CodeExplorerDblClick(Sender: TObject);
begin
  if not Assigned(CodeExplorer.Selected) then
    Exit;
  case CodeExplorer.Selected.ImageIndex of
    1: if Assigned(FOpenEditor) then
        OpenEditor(CreateAbsolutePath(FRequiredFiles[IntPtr(CodeExplorer.Selected.Data)],
          ExtractFilePath(FFileName)), Point(0, 0));
    2:
      if CodeExplorer.Selected.Parent.ImageIndex = 0 then
        CodeJump(Point(1, FFunctions[IntPtr(CodeExplorer.Selected.Data)].Line + 1))
      else if Assigned(FOpenEditor) then
        OpenEditor(CreateAbsolutePath(
          FRequiredFiles[IntPtr(CodeExplorer.Selected.Parent.Data)],
          ExtractFilePath(FFileName)),
          Point(1, FFunctions[IntPtr(CodeExplorer.Selected.Data)].Line + 1));
    3:
      if CodeExplorer.Selected.Parent.ImageIndex = 0 then
        CodeJump(Point(FVars[IntPtr(CodeExplorer.Selected.Data)].Pos,
          FVars[IntPtr(CodeExplorer.Selected.Data)].Line + 1))
      else if Assigned(FOpenEditor) then
        OpenEditor(CreateAbsolutePath(
          FRequiredFiles[IntPtr(CodeExplorer.Selected.Parent.Data)],
          ExtractFilePath(FFileName)),
          Point(FVars[IntPtr(CodeExplorer.Selected.Data)].Pos,
          FVars[IntPtr(CodeExplorer.Selected.Data)].Line + 1));
  end;
end;

procedure TEditorFrame.CodeEditorChange(Sender: TObject);
begin
  Highlight.JumpItem := SelectedItem(-1, 0);
  UpdateTimerTimer(nil);
  if Assigned(FOnChange) then
    FOnChange(Self);
  CodeEditor.Invalidate;
end;


procedure TEditorFrame.StartFormatter;
var
  c: TCodeFormatter;
  i: integer;
begin
  if MessageDlg('Code Formatieren', 'Codeformatter:'#10#13'Wirklich formatieren?',
    mtConfirmation, mbYesNo, 'Confirmation') = mrYes then
  begin
    c := TCodeFormatter.Create;
    try
      c.Lines.Clear;
      c.Lines.AddStrings(CodeEditor.Lines);
      c.Format;
      for i := 0 to CodeEditor.Lines.Count - 1 do
        if CodeEditor.Lines[i] <> c.Lines[i] then
        begin
          CodeEditor.TextBetweenPoints[Point(0, i + 1),
            Point(Length(CodeEditor.Lines[i]) + 1, i + 1)] := c.Lines[i];
        end;
    finally
      c.Free;
    end;

  end;
end;

procedure TEditorFrame.CheckSelTimerTimer(Sender: TObject);

  function isParam(out n: integer; out s: string): boolean;
  var
    i, len: integer;
    ln: string;
    d: integer;
  begin
    Result := False;
    n := 0;
    d := 1;
    i := CodeEditor.LogicalCaretXY.x - 1;
    ln := CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1];
    while (i > 0) and (i <= Length(ln)) and (d > 0) do
    begin
      if ln[i] = ')' then
        Inc(d)
      else if ln[i] = '(' then
        Dec(d);
      if (d = 1) and (ln[i] = ',') then
        Inc(n);
      Dec(i);
    end;
    if d = 0 then
    begin
      len := 0;
      while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '#']) do
      begin
        Dec(i);
        Inc(len);
      end;
      if len > 0 then
        s := Copy(ln, i + 1, len);
      Result := True;
    end;
  end;

  function isStdFunc(s: string; out x: integer): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FStdFunc.Count - 1 do
    begin
      if AnsiStartsText(s + '(', FStdFunc[i].Name) then
      begin
        Result := True;
        x := i;
        Exit;
      end;
    end;
  end;

  function isInFunc(s: string; out x: integer): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
    begin
      if AnsiStartsText(s + '(', FFunctions[i].Name) then
      begin
        Result := True;
        x := i;
        Exit;
      end;
    end;
  end;

var
  s, m, inf: string;
  i, n: integer;
  tmp: string;
begin
  if (CodeEditor.TopLine <> Topl) then
  begin
    if FToolTip.Visible then
    begin
      FToolTip.Hide;
      CodeEditor.Invalidate;
      currFunc := '';
    end;
    topl := CodeEditor.TopLine;
  end;
  tmp := lowercase(GetCurrWord());
  if tmp <> currWord then
  begin
    currWord := tmp;
    Highlight.SelectedText := '';
    CodeEditor.Invalidate;
    //Reset
    SelectHighlightTimer.Enabled := False;
    SelectHighlightTimer.Enabled := True;
  end;
  m := '';
  inf := '';
  if isParam(n, s) then
  begin
    if isStdFunc(s, i) then
    begin
      m := FStdFunc[i].Name;
      inf := FStdFunc[i].Info;
    end
    else if isInFunc(s, i) then
    begin
      m := FFunctions[i].Name;
      inf := FFunctions[i].Info;
    end;
  end
  else
  begin
    n := -1;
    if isStdFunc(tmp, i) then
    begin
      m := FStdFunc[i].Name;
      inf := FStdFunc[i].Info;
    end
    else if isInFunc(tmp, i) then
    begin
      m := FFunctions[i].Name;
      inf := FFunctions[i].Info;
    end;
  end;
  if isEnd(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1], 'func') then
    m := '';
  FToolTip.SelectedParam := n;
  if Length(m) > 0 then
  begin
    if LowerCase(m) <> LowerCase(currFunc) then
    begin
      currInfo := inf;
      currFunc := m;
      FToolTip.Hide;
      ToolTipTimer.Enabled := False;
      ToolTipTimer.Enabled := True;
    end;
  end
  else
  begin
    currFunc := '';
    FToolTip.Hide;
  end;
end;

procedure TEditorFrame.CloseCodeExplorerButtonClick(Sender: TObject);
begin
  CodeExplorerPanel.Hide;
end;

procedure TEditorFrame.CloseSearchButtonClick(Sender: TObject);
begin
  SearchBar.Hide;
  CodeEditor.SetFocus;
end;

function TEditorFrame.GetCurrWord(): string;
var
  s: integer;
  i: integer;
  len: integer;
  slen: integer;
  ln: string;
begin
  Result := '';
  ln := CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1];
  if ln = '' then
    Exit;
  slen := Length(ln);
  i := CodeEditor.LogicalCaretXY.x - 1;
  len := 0;

  if i < 1 then
    i := 1;
  if (i < slen) and (ln[i + 1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$', '#']) and
    ((i > 0) or (ln[i] in [#0..#32])) then
    Inc(i);

  while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
    Dec(i);

  if (i > 0) and (ln[i] in ['$', '#']) then
  begin
    Inc(len);
    s := i;
    Inc(i);
  end
  else
  begin
    Inc(i);
    s := i;
  end;

  while (i <= slen) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
  begin
    Inc(i);
    Inc(len);
  end;
  Result := Copy(ln, s, len);

end;

procedure TEditorFrame.SetFunc(l: TFuncList);
begin
  FFunctions.Clear;
  FFunctions.Assign(l);
end;

procedure TEditorFrame.SetVar(l: TVarList);
begin
  FVars.Clear;
  FVars.Assign(l);
end;

destructor TEditorFrame.Destroy;
var
  i: integer;
begin
  FToolTip.Free;
  if not (Parser.Finished) then
    Parser.Terminate;
  Parser.Free;
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges.Items[i].Free;
  FRequiredFiles.Free;
  FDefRanges.Free;
  FFunctions.Free;
  FVars.Free;
  FStdFunc.Free;
  FKeyWords.Free;
  CodeEditor.Highlighter := nil;
  Highlight.Free;
  inherited;
end;

end.
