unit Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  AALHighlighter, Types, contnrs, LCLType, ExtCtrls, AALTypes, UnitParser,
  Dialogs, Graphics;

type

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    CodeEditor: TSynEdit;
    Completion: TSynCompletion;
    procedure CodeEditorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CompletionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    procedure CompletionSearchPosition(var APosition: integer);
    procedure UpdateTimerTimer(Sender: TObject);
  private
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
    { public declarations }
  end;

implementation

{$R *.lfm}

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
end;

procedure TEditorFrame.CompletionExecute(Sender: TObject);
var
  i, x: integer;
begin
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.Clear;
    Completion.ItemList.AddStrings(FStdFunc);
    Completion.ItemList.AddStrings(FFunctions);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    Completion.ItemList.Clear;
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
    Completion.ItemList.Clear;
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
  if Length(Completion.CurrentString) = 0 then
  begin
    Completion.ItemList.Clear;
    Completion.ItemList.AddStrings(FKeyWords);
    Completion.ItemList.AddStrings(FStdFunc);
    Completion.ItemList.AddStrings(FFunctions);
  end
  else if Completion.CurrentString[1] = '$' then
  begin
    Completion.ItemList.Clear;
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
    Completion.ItemList.Clear;
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
  if not (Value[1] = '$') then
  begin
    p := Pos('(', Value);
    if p > 0 then
      SetLength(Value, Pos('(', Value))
    else
      Value := Value + ' ';
  end;
end;

procedure TEditorFrame.CodeEditorKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
    UpdateTimerTimer(nil);
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
  if not Parser.Finished then
    Parser.Suspend;
  Parser.Free;
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges.Items[i].Free;
  FDefRanges.Free;
  FFunctions.Free;
  FVars.Free;
  FStdFunc.Clear;
  CodeEditor.Highlighter:=nil;
  Highlight.Free;
  inherited;
end;

end.
