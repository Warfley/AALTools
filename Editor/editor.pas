unit Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  AALHighlighter, Types, contnrs, LCLType, ExtCtrls, AALTypes, UnitParser,
  Dialogs, Graphics, strutils;

type

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    CodeEditor: TSynEdit;
    Completion: TSynCompletion;
    procedure CodeEditorChange(Sender: TObject);
    procedure CodeEditorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CompletionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    procedure CompletionSearchPosition(var APosition: integer);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    Highlight: TAALSynHighlight;
    FFunctions: TStringList;
    FVars: TStringList;
    FStdFunc: TStringList;
    FFileName: string;
    FKeyWords: TStringList;
    FDefRanges: TObjectList;
    Parser: TUnitParser;
    function GetFont: TFont;
    procedure SetFont(f: TFont);
    procedure SetRanges(l: TObjectList);
    procedure SetFunc(l: TStringList);
    procedure SetVar(l: TStringList);
    procedure MoveHorz(i: IntPtr);
    procedure MoveVert(i: IntPtr);
    { private declarations }
  public
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property FunctionList: TStringList read FFunctions write SetFunc;
    property VariableList: TStringList read FVars write SetVar;
    property FileName: string read FFileName write FFilename;
    property DefRanges: TObjectList read FDefRanges write SetRanges;
    property Font: TFont read GetFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
  CodeEditor.LogicalCaretXY := p;
end;

procedure TEditorFrame.MoveVert(i: IntPtr);
var
  p: TPoint;
begin
  p := CodeEditor.LogicalCaretXY;
  p.y := p.y + i;
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

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited;
  CodeEditor.Lines.Add('');
  FOnChange := nil;
  Parser := TUnitParser.Create(True);
  Highlight := TAALSynHighlight.Create(nil);
  Highlight.LoadConfig(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'HL');
  CodeEditor.Highlighter := Highlight;
  FFunctions := TStringList.Create;
  FVars := TStringList.Create;
  FStdFunc := TStringList.Create;
  FKeyWords := TStringList.Create;
  FDefRanges := TObjectList.Create(False);
  FStdFunc.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'Funcs.lst');
  FKeyWords.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'Keywords.lst');
  UpdateTimerTimer(nil);
end;

procedure TEditorFrame.CompletionExecute(Sender: TObject);
var
  i, x: integer;
begin
  Completion.ItemList.Clear;
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.AddStrings(FStdFunc);
    Completion.ItemList.AddStrings(FFunctions);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    for i := 0 to FVars.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FVars[i])) = 1 then
        Completion.ItemList.Add(FVars[i]);
    for x := 0 to FDefRanges.Count - 1 do
      if (CodeEditor.BlockBegin.y >= (FDefRanges[x] as TDefRange).StartLine) and
        (CodeEditor.BlockBegin.y <= (FDefRanges[x] as TDefRange).EndLine) then
        for i := 0 to (FDefRanges[x] as TDefRange).Vars.Count - 1 do
          if Pos(LowerCase(Completion.CurrentString), LowerCase(
            (FDefRanges[x] as TDefRange).Vars[i])) = 1 then
            Completion.ItemList.Add((FDefRanges[x] as TDefRange).Vars[i]);
  end
  else
  begin
    for i := 0 to FKeyWords.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FKeyWords[i])) = 1 then
        Completion.ItemList.Add(FKeyWords[i]);
    for i := 0 to FStdFunc.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FStdFunc[i])) = 1 then
        Completion.ItemList.Add(FStdFunc[i]);
    for i := 0 to FFunctions.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FFunctions[i])) = 1 then
        Completion.ItemList.Add(FFunctions[i]);
  end;
  Completion.ItemList.Add(Completion.CurrentString);
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

procedure TEditorFrame.CompletionSearchPosition(var APosition: integer);
var
  i, x: integer;
begin
  Completion.ItemList.Clear;
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.AddStrings(FKeyWords);
    Completion.ItemList.AddStrings(FStdFunc);
    Completion.ItemList.AddStrings(FFunctions);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    for i := 0 to FVars.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FVars[i])) = 1 then
        Completion.ItemList.Add(FVars[i]);
    for x := 0 to FDefRanges.Count - 1 do
      if (CodeEditor.LogicalCaretXY.y >= (FDefRanges[x] as TDefRange).StartLine) and
        (CodeEditor.LogicalCaretXY.y <= (FDefRanges[x] as TDefRange).EndLine) then
        for i := 0 to (FDefRanges[x] as TDefRange).Vars.Count - 1 do
          if Pos(LowerCase(Completion.CurrentString), LowerCase(
            (FDefRanges[x] as TDefRange).Vars[i])) = 1 then
            Completion.ItemList.Add((FDefRanges[x] as TDefRange).Vars[i]);
  end
  else
  begin
    for i := 0 to FKeyWords.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FKeyWords[i])) = 1 then
        Completion.ItemList.Add(FKeyWords[i]);
    for i := 0 to FStdFunc.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FStdFunc[i])) = 1 then
        Completion.ItemList.Add(FStdFunc[i]);
    for i := 0 to FFunctions.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FFunctions[i])) = 1 then
        Completion.ItemList.Add(FFunctions[i]);
  end;
  Completion.ItemList.Add(Completion.CurrentString);
end;

procedure TEditorFrame.UpdateTimerTimer(Sender: TObject);
begin
  if Parser.Finished or Parser.Suspended then
  begin
    Parser.Free;
    Parser := TUnitParser.Create(True);
    Parser.Text := CodeEditor.Lines.Text;
    Parser.Funcs := FFunctions;
    Parser.Vars := FVars;
    Parser.Ranges := FDefRanges;
    Parser.Start;
  end;
end;

procedure TEditorFrame.CompletionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  p: integer;
begin
  if Length(Value) = 0 then
    Exit;
  if not (Value[1] = '$') then
  begin
    p := Pos('(', Value);
    if p > 0 then
    begin
      if Value[p + 1] <> ')' then
        Application.QueueAsyncCall(@MoveHorz, -1);
      SetLength(Value, Pos('(', Value));
      Value := Value + ')';
    end
    else
      Value := Value + ' ';
  end;
end;

procedure TEditorFrame.CodeEditorKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);

  function isEnd(s, endTok: string): boolean;
  var
    l, l2: integer;
  begin
    s := Trim(s);
    l := Length(endTok);
    l2 := Length(s);
    Result := False;
    if l2 < l then
    begin
      Exit;
    end
    else
    if (l2 > l) and (AnsiStartsText(endTok, s) and (s[l + 1] in [#0..#32])) then
    begin
      Result := True;
      Exit;
    end
    else
    if LowerCase(s) = endTok then
    begin
      Result := True;
      Exit;
    end;
  end;

  function GotClosed(i: integer; sTok, eTok: string): boolean;
  var
    counter, c: integer;
  begin
    while (i >= 0) do
      if isEnd(CodeEditor.Lines[i], 'func') then
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
  ln, pref: string;
  i, x, l: integer;
  b: boolean;
begin
  if Key = 13 then
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
        CodeEditor.Lines.Insert(i + 2, pref + 'WEnd');
      end;
        Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'for') then
    begin
      if not GotClosed(i, 'for', 'next') then
      begin
        CodeEditor.Lines.Insert(i + 2, pref + 'Next');
      end;
        Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'if') then
    begin
      if not GotClosed(i, 'if', 'endif') then
      begin
        CodeEditor.Lines.Insert(i + 2, pref + 'EndIf');
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
        CodeEditor.Lines.Insert(i + 2, pref + 'EndFunc');
      end;
        Application.QueueAsyncCall(@MoveHorz, 2);
    end;
    UpdateTimerTimer(nil);
  end;
end;

procedure TEditorFrame.CodeEditorChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorFrame.SetFunc(l: TStringList);
begin
  FFunctions.Clear;
  FFunctions.AddStrings(l);
end;

procedure TEditorFrame.SetVar(l: TStringList);
begin
  FVars.Clear;
  FVars.AddStrings(l);
end;

destructor TEditorFrame.Destroy;
var
  i: integer;
begin
  Parser.Free;
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges.Items[i].Free;
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
