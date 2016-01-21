unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ComCtrls, Buttons, ExtCtrls, PairSplitter, Project, IDEStartupScreen,
  ProjectInspector, EditorManagerFrame, AALTypes, FormEditor, Editor,
  AALFileInfo, strutils, CompilerOptions, AALCompiler;

type

  { TMainForm }
  PEnterFuncInfo = ^TEnterFuncInfo;

  TEnterfuncInfo = record
    FileName: string;
    Pos: TPoint;
  end;

  PCreateFuncInfo = ^TCreateFuncInfo;

  TCreateFuncInfo = record
    FileName, Func: string;
  end;

  TMainForm = class(TForm)
    AALIDEProps: TApplicationProperties;
    TextEditorOptionsItem: TMenuItem;
    OutputBox: TListBox;
    SaveAllBtn: TSpeedButton;
    CloseAllBtn: TSpeedButton;
    CloseEditorBtn: TSpeedButton;
    SelectModeBox: TComboBox;
    EditorManager1: TEditorManager;
    MainFormMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    CloseFileItem: TMenuItem;
    CloseAllItem: TMenuItem;
    EditMenuItem: TMenuItem;
    FormatMenuItem: TMenuItem;
    CompDebMenuItem: TMenuItem;
    CompRelMenuItem: TMenuItem;
    ConfigMenuItem: TMenuItem;
    CompOptionMenuItem: TMenuItem;
    ToolbarSplit1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    RenReleaseMenuItem: TMenuItem;
    RunDebugMenuItem: TMenuItem;
    RunMenuItem: TMenuItem;
    OpenAALFileDialog: TOpenDialog;
    ProjectInspector1: TProjectInspector;
    SaveAsItem: TMenuItem;
    SaveAALFileDialog: TSaveDialog;
    SaveFileItem: TMenuItem;
    SaveAllItem: TMenuItem;
    MenuSplitItem5: TMenuItem;
    MenuSplitItem4: TMenuItem;
    MenuSplitItem3: TMenuItem;
    SearchMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    AddUnitBtn: TSpeedButton;
    AddFormBtn: TSpeedButton;
    SaveAsBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    NewProjBtn: TSpeedButton;
    RunBtn: TSpeedButton;
    StopBtn: TSpeedButton;
    ToolBar1: TToolBar;
    UndoMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    MenuSplitItem2: TMenuItem;
    MenuSplitItem1: TMenuItem;
    NewFormItem: TMenuItem;
    NewFileItem: TMenuItem;
    NewMenuItem: TMenuItem;
    NewProjectItem: TMenuItem;
    procedure CloseAllItemClick(Sender: TObject);
    procedure CloseFileItemClick(Sender: TObject);
    procedure CompDebMenuItemClick(Sender: TObject);
    procedure CompOptionMenuItemClick(Sender: TObject);
    procedure CompRelMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewFileItemClick(Sender: TObject);
    procedure NewFormItemClick(Sender: TObject);
    procedure NewProjectItemClick(Sender: TObject);
    procedure RenReleaseMenuItemClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure RunDebugMenuItemClick(Sender: TObject);
    procedure SaveAllItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveFileItemClick(Sender: TObject);
    procedure KillEditor(s: string);
    procedure StopBtnClick(Sender: TObject);
    procedure ToolBar1Paint(Sender: TObject);
  private
    FFirstLoad: boolean;
    FCompiler: TAALCompiler;
    FFormIsClosing: boolean;
    FCurrentProject: TAALProject;
    FLastOpend: TStringList;
    FFileData: TAALFileManager;
    { private declarations }
    procedure EditorParserFinished(Sender: TObject);
    procedure ShowStartupScreen(Data: IntPtr);
    procedure OpenFile(Filename: string; Pos: TPoint);
    function EnterFunction(FileName, FuncName: string; Params: string;
      CreateIfMissing: boolean): string;
    procedure AddInclude(FileName, IncludeFile: string);
    function CheckInclude(FileName, IncludeFile: string): boolean;
    procedure EditorClosing(Sender: TObject; Editor: integer; var Proceed: boolean);
    procedure EditorCreated(Sender: TObject; Editor: integer);
    procedure EditorChanged(Sender: TObject);
    procedure UpdateProject(Data: IntPtr);
    procedure EnterFunc(Data: IntPtr);
    procedure CreateFunc(Data: IntPtr);
    procedure ChangeMainForm(FileName: string);
    function ShowCompilerOptions: boolean;
    procedure PrintText(Sender: TObject; FileName: string; Output: string);
    procedure FinishedComp(Sender: TObject);
    procedure FinishedRun(Sender: TObject);
    procedure CompileError(Sender: TObject);
  public
    property CurrentProject: TAALProject read FCurrentProject;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.PrintText(Sender: TObject; FileName: string; Output: string);
begin
  OutputBox.Items.Add(FCurrentProject.GetRelPath(FileName) + ': ' + Output);
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
end;

procedure TMainForm.FinishedComp(Sender: TObject);
begin
  OutputBox.Items.Add('Kompilieren abgeschlossen');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
end;

procedure TMainForm.FinishedRun(Sender: TObject);
begin
  OutputBox.Items.Add('Ausführung beendet');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

    procedure TMainForm.CompileError(Sender: TObject);
    begin
  OutputBox.Items.Add('Compiler Fehler, ausführung beendet');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
    end;

function TMainForm.ShowCompilerOptions: boolean;
begin
  Result := False;
  CompilerOptionsForm.CDebugFileEdit.FileName := FCompiler.CompilerDebugPath;
  CompilerOptionsForm.CReleaseFileEdit.FileName := FCompiler.CompilerReleasePath;
  CompilerOptionsForm.CLogEdit.Text := FCompiler.CompilerOutputPath;
  CompilerOptionsForm.COutputBox.Checked := FCompiler.PrintCompilerOutput;
  CompilerOptionsForm.CAdvOutputBox.Checked := FCompiler.AdvancedCompilerOutput;
  CompilerOptionsForm.IDebugFileEdit.FileName := FCompiler.InterpreterDebugPath;
  CompilerOptionsForm.IReleaseFileEdit.FileName := FCompiler.InterpreterReleasePath;
  CompilerOptionsForm.ILogEdit.Text := FCompiler.InterpreterOutputPath;
  CompilerOptionsForm.IOutputBox.Checked := FCompiler.PrintInterpreaterOutput;
  if CompilerOptionsForm.ShowModal = mrOk then
  begin
    FCompiler.CompilerDebugPath := CompilerOptionsForm.CDebugFileEdit.FileName;
    FCompiler.CompilerReleasePath := CompilerOptionsForm.CReleaseFileEdit.FileName;
    FCompiler.CompilerOutputPath := CompilerOptionsForm.CLogEdit.Text;
    FCompiler.PrintCompilerOutput := CompilerOptionsForm.COutputBox.Checked;
    FCompiler.AdvancedCompilerOutput := CompilerOptionsForm.CAdvOutputBox.Checked;
    FCompiler.InterpreterDebugPath := CompilerOptionsForm.IDebugFileEdit.FileName;
    FCompiler.InterpreterReleasePath := CompilerOptionsForm.IReleaseFileEdit.FileName;
    FCompiler.InterpreterOutputPath := CompilerOptionsForm.ILogEdit.Text;
    FCompiler.PrintInterpreaterOutput := CompilerOptionsForm.IOutputBox.Checked;
    FCompiler.WriteConf(IncludeTrailingPathDelimiter(
      ExtractFilePath(ParamStr(0))) + 'compiler.cfg');
    Result := True;
  end;
end;

procedure TMainForm.OpenFile(Filename: string; Pos: TPoint);
begin
  if not FilenameIsAbsolute(Filename) then
    FileName := FCurrentProject.GetAbsPath(Filename);
  EditorManager1.OpenEditor(Filename, Pos);
end;

procedure TMainForm.ShowStartupScreen(Data: IntPtr);

  procedure StringsDelete(s: TStrings; str: string);
  var
    i: integer;
  begin
    for i := 0 to s.Count - 1 do
      if s[i] = str then
      begin
        s.Delete(i);
        Break;
      end;
  end;

var
  i: integer;
begin
  Self.Hide;
  if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'compiler.cfg') then
    FCompiler.ReadConf(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'compiler.cfg')
  else if not ShowCompilerOptions then
  begin
    Close;
    Exit;
  end;
  StartupScreen.LastOpend := FLastOpend;
  if FFirstLoad and (Paramcount > 0) and FileExists(ParamStr(1)) and
    (LowerCase(ExtractFileExt(ParamStr(1))) = '.aalproj') then
    StartupScreen.SelectedPath := ParamStr(1)
  else
    StartupScreen.ShowModal;
  FFirstLoad := False;
  if FileExists(StartupScreen.SelectedPath) then
  begin
    StringsDelete(FLastOpend, StartupScreen.SelectedPath);
    FLastOpend.Insert(0, StartupScreen.SelectedPath);
    FCurrentProject.ReadFromFile(StartupScreen.SelectedPath);
    FCurrentProject.CheckInclude := @CheckInclude;
    FCurrentProject.AddInclude := @AddInclude;
    ProjectInspector1.Project := FCurrentProject;
    OpenAALFileDialog.InitialDir := FCurrentProject.ProjectDir;
    SaveAALFileDialog.InitialDir := FCurrentProject.ProjectDir;
    Self.Show;
    for i := 0 to FCurrentProject.OpendFiles.Count - 1 do
      if FileExists(FCurrentProject.GetAbsPath(
        FCurrentProject.OpendFiles[i].Name)) then
        EditorManager1.OpenEditor(FCurrentProject.GetAbsPath(
          FCurrentProject.OpendFiles[i].Name),
          Point(FCurrentProject.OpendFiles[i].Pos, FCurrentProject.OpendFiles[i].Line));
    EditorManager1.EditorIndex := FCurrentProject.FocusedFile;
    ProjectInspector1.OpenEditor := @OpenFile;
    ProjectInspector1.CloseEditor := @KillEditor;
    EditorManager1.OnEditorClose := @EditorClosing;
    EditorManager1.OnEditorChanged := @EditorChanged;
    EditorManager1.OnEditorCreated := @EditorCreated;
  end
  else
    Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  mr: TModalResult;
begin
  FFormIsClosing := True;
  EditorManager1.OnEditorChanged := nil;
  EditorManager1.OnEditorCreated := nil;
  CloseAllItemClick(Sender);
  if EditorManager1.Count > 0 then
    CloseAction := caNone
  else
  if FCurrentProject.Changed then
  begin
    mr := MessageDlg('Projekt sichern',
      'Das Projekt hat sich seit dem Letzten öffnen geändert'#10#13'Projekt sichern?',
      mtConfirmation, mbYesNoCancel, 'Sichern');
    case mr of
      mrYes: FCurrentProject.Save;
      mrAbort: CloseAction := caNone;
    end;
  end;
  if CloseAction = caNone then
  begin
    EditorManager1.OnEditorChanged := @EditorChanged;
    EditorManager1.OnEditorCreated := @EditorCreated;
    FFormIsClosing := False;
  end;
  FLastOpend.SaveToFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
end;

procedure TMainForm.CloseFileItemClick(Sender: TObject);
begin
  EditorManager1.CloseEditor(EditorManager1.EditorIndex);
end;

procedure TMainForm.CompDebMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(SaveAllItem);
  OutputBox.Clear;
  FCompiler.Compile(FCurrentProject, cmDebug);
end;

procedure TMainForm.CompOptionMenuItemClick(Sender: TObject);
begin
  ShowCompilerOptions;
end;

procedure TMainForm.CompRelMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(SaveAllItem);
  OutputBox.Clear;
  FCompiler.Compile(FCurrentProject, TCompilerMode.cmRelease);
end;

procedure TMainForm.CloseAllItemClick(Sender: TObject);
var
  i: integer;
begin
  i := EditorManager1.Count + 1;
  while (EditorManager1.Count > 0) and (i > EditorManager1.Count) do
  begin
    i := EditorManager1.Count;
    EditorManager1.CloseEditor(EditorManager1.EditorIndex);
    EditorManager1.Invalidate;
  end;
end;

procedure TMainForm.EnterFunc(Data: IntPtr);
begin
  EditorManager1.OpenEditor(PEnterFuncInfo(Data)^.FileName,
    PEnterFuncInfo(Data)^.Pos);
  Dispose(PEnterFuncInfo(Data));
end;


procedure TMainForm.CreateFunc(Data: IntPtr);
var
  e: TEditorFrame;
begin
  with PCreateFuncInfo(Data)^ do
  begin
    e := EditorManager1.OpenEditor(FileName, Point(0, 0)) as TEditorFrame;
    e.CodeEditor.TextBetweenPoints[Point(
      Length(e.CodeEditor.Lines[e.CodeEditor.Lines.Count - 1]) + 1,
      e.CodeEditor.Lines.Count),
      Point(Length(e.CodeEditor.Lines[e.CodeEditor.Lines.Count - 1]) +
      1, e.CodeEditor.Lines.Count)] := #13#13 + 'Func ' + Func + #13#13 + 'EndFunc';
    e.CodeEditor.LogicalCaretXY := Point(2, e.CodeEditor.Lines.Count - 1);
    Application.QueueAsyncCall(@e.MoveHorz, 2);
  end;
  Dispose(PCreateFuncInfo(Data));
end;

function TMainForm.EnterFunction(FileName, FuncName: string; Params: string;
  CreateIfMissing: boolean): string;
var
  i: integer;
  e: TEditorFrame;
  d: PEnterFuncInfo;
  c: PCreateFuncInfo;
begin
  if FFileData.FileIndex[FileName] = -1 then
  begin
    FFileData.LoadFile(FileName);
    exit;
  end;
  with FFileData[FFileData.FileIndex[FileName]] do
    for i := 0 to Functions.Count - 1 do
      if pos(LowerCase(FuncName), LowerCase(Functions[i].Name)) = 1 then
      begin
        new(d);
        d^.FileName := FileName;
        d^.Pos := Point(1, Functions[i].Line + 2);
        Application.QueueAsyncCall(@EnterFunc, PtrInt(d));
        Exit;
      end;
  if CreateIfMissing then
  begin
    new(c);
    c^.FileName := FileName;
    c^.Func := FuncName + '(' + Params + ')';
    Application.QueueAsyncCall(@CreateFunc, IntPtr(c));
  end;
end;

procedure TMainForm.AddInclude(FileName, IncludeFile: string);
var
  e: TEditorFrame;
  sl: TStringList;
begin
  if FilenameIsAbsolute(IncludeFile) then
    IncludeFile := CreateRelativePath(IncludeFile, ExtractFilePath(FileName), True);
  e := EditorManager1.TextEditor[FileName];
  if Assigned(e) then
    e.CodeEditor.TextBetweenPoints[Point(1, 1), Point(1, 1)] :=
      Format('#include("%s")'#13, [IncludeFile])
  else if FileExists(FileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FileName);
      sl.Insert(0, Format('#include("%s")', [IncludeFile]));
      sl.SaveToFile(FileName);
    finally
      sl.Free;
    end;
  end;
end;

function TMainForm.CheckInclude(FileName, IncludeFile: string): boolean;

  function ExtractBetween(const Value, A, B: string): string;
  var
    aPos, bPos: integer;
  begin
    Result := '';
    aPos := Pos(A, Value);
    if aPos > 0 then
    begin
      aPos := aPos + Length(A);
      bPos := PosEx(B, Value, aPos);
      if bPos > 0 then
      begin
        Result := Copy(Value, aPos, bPos - aPos);
      end;
    end;
  end;

var
  i: integer;
  e: TEditorFrame;
  sl: TStringList;
  fname: string;
begin
  e := EditorManager1.TextEditor[FileName];
  Result := False;
  ;
  if Assigned(e) then
  begin
    i := 0;
    while i < e.CodeEditor.Lines.Count do
    begin
      if isEnd(e.CodeEditor.Lines[i], '#include') then
      begin
        fname := ExtractBetween(e.CodeEditor.Lines[i], '"', '"');
        Result := IncludeFile = fname;
        if not FilenameIsAbsolute(fname) then
          fname := CreateAbsolutePath(fname, ExtractFilePath(FileName));
        Result := Result or (IncludeFile = fname);
        if not (FileExists(fname) or (EditorManager1.Editor[fname] >= 0)) then
        begin
          if i = e.CodeEditor.Lines.Count - 1 then
            e.CodeEditor.TextBetweenPoints[Point(1, i + 1),
              Point(Length(e.CodeEditor.Lines[i]) + 1, i + 1)] := ''
          else
            e.CodeEditor.TextBetweenPoints[Point(1, i + 1), Point(1, i + 2)] := '';
          Continue;
        end;
      end;
      Inc(i);
    end;
  end
  else if FileExists(FileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FileName);
      i := 0;
      while i < sl.Count - 1 do
      begin
        if isEnd(sl[i], '#include') then
        begin
          fname := ExtractBetween(sl[i], '"', '"');
          Result := IncludeFile = fname;
          if not FilenameIsAbsolute(fname) then
            fname := CreateAbsolutePath(fname, ExtractFilePath(FileName));
          Result := Result or (IncludeFile = fname);
          if not (FileExists(fname) or (EditorManager1.Editor[fname] >= 0)) then
          begin
            sl.Delete(i);
            Continue;
          end;
        end;
        Inc(i);
      end;
      sl.SaveToFile(FileName);
    finally
      sl.Free;
    end;
  end;
end;

procedure TMainForm.EditorClosing(Sender: TObject; Editor: integer;
  var Proceed: boolean);
var
  res: TModalResult;
begin
  if EditorManager1.Editors[Editor].Parent.Caption[1] = '*' then
  begin
    EditorManager1.EditorIndex := Editor;
    EditorManager1.Invalidate;
    res := MessageDlg('Datei wurde Verändert',
      'Datei wurde Verändert'#10#13'Vor dem schließen Sichern?',
      mtConfirmation, mbYesNoCancel, 'Schließen?');
    case res of
      mrYes: SaveFileItemClick(SaveFileItem);
      mrNo: Exit;
      mrCancel: Proceed := False;
    end;
  end;
  if Proceed and not FFormIsClosing then
    Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.EditorCreated(Sender: TObject; Editor: integer);
begin
  Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.EditorChanged(Sender: TObject);
begin
  Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.UpdateProject(Data: IntPtr);
var
  i: integer;
begin
  FCurrentProject.OpendFiles.Clear;
  for i := 0 to EditorManager1.Count - 1 do
    FCurrentProject.OpendFiles.Add(
      OpendFileInfo(FCurrentProject.GetRelPath(EditorManager1.EditorFiles[i]),
      EditorManager1.EditorCaret[i].Y, EditorManager1.EditorCaret[i].X));
  FCurrentProject.FocusedFile := EditorManager1.EditorIndex;
  FCurrentProject.Changed := True;
end;

procedure TMainForm.KillEditor(s: string);
var
  i: integer;
begin
  for i := 0 to EditorManager1.Count - 1 do
    if EditorManager1.EditorFiles[i] = s then
    begin
      EditorManager1.Editors[i].Free;
      EditorManager1.EditorControl.Pages[i].Free;
      Break;
    end;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FCompiler.Stop;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMainForm.ToolBar1Paint(Sender: TObject);
begin
  ToolBar1.Canvas.Pen.Style := psSolid;
  ToolBar1.Canvas.Pen.Color := $00DEDEDE;
  ToolBar1.Canvas.Pen.Width := 2;
  ToolBar1.Canvas.MoveTo(-1, ToolBar1.Height - 1);
  ToolBar1.Canvas.LineTo(ToolBar1.Width, ToolBar1.Height - 1);
end;

procedure TMainForm.EditorParserFinished(Sender: TObject);
var
  i, n, idx, f: integer;
  req: string;
  e: TFormEditFrame;
begin
  if Sender is TFormEditFrame then
  begin
    idx := FFileData.FileIndex[(Sender as TFormEditFrame).FileName];
    if idx = -1 then
      idx := FFileData.CreateFile((Sender as TFormEditFrame).FileName);
    (Sender as TFormEditFrame).AddToVarlist(FFileData[idx].Variables);
    idx := FFileData.FileIndex[ChangeFileExt(
      (Sender as TFormEditFrame).FileName, 'aal1')];
    if idx = -1 then
      idx := FFileData.LoadFile(ChangeFileExt(
        (Sender as TFormEditFrame).FileName, 'aal1'));
  end
  else if Sender is TEditorFrame then
  begin
    idx := FFileData.FileIndex[(Sender as TEditorFrame).FileName];
    if idx = -1 then
      idx := FFileData.CreateFile((Sender as TEditorFrame).FileName);
    FFileData[idx].Functions := (Sender as TEditorFrame).FunctionList;
    FFileData[idx].RequiredFiles := (Sender as TEditorFrame).RequiredFiles;
    FFileData[idx].Variables := (Sender as TEditorFrame).VariableList;
    for i := 0 to FFileData[idx].RequiredFiles.Count - 1 do
    begin
      req := FFileData[idx].RequiredFiles[i];
      if not FilenameIsAbsolute(req) then
        req := CreateAbsolutePath(FFileData[idx].RequiredFiles[i],
          ExtractFilePath((Sender as TEditorFrame).FileName) + PathDelim);
      f := FFileData.FileIndex[req];
      if f >= 0 then
      begin
        for n := 0 to FFileData[f].Variables.Count - 1 do
          (Sender as TEditorFrame).VariableList.Add(FFileData[f].Variables[n]);
        for n := 0 to FFileData[f].Functions.Count - 1 do
          (Sender as TEditorFrame).FunctionList.Add(FFileData[f].Functions[n]);
      end
      else
        FFileData.LoadFile(req);
    end;
    e := EditorManager1.FormEditor[ChangeFileExt(
      (Sender as TEditorFrame).FileName, '.afm')];
    if Assigned(e) then
    begin
      with Sender as TEditorFrame do
      begin
        e.FuncList.Clear;
        for i := 0 to FunctionList.Count - 1 do
          e.FuncList.Add(Copy(FunctionList[i].Name, 1,
            Pos('(', FunctionList[i].Name) - 1));
      end;
    end;
  end;
end;

procedure TMainForm.ChangeMainForm(FileName: string);

  function ExtractBetween(const Value, A, B: string): string;
  var
    aPos, bPos: integer;
  begin
    Result := '';
    aPos := Pos(A, Value);
    if aPos > 0 then
    begin
      aPos := aPos + Length(A);
      bPos := PosEx(B, Value, aPos);
      if bPos > 0 then
      begin
        Result := Copy(Value, aPos, bPos - aPos);
      end;
    end;
  end;

var
  sl: TStringList;
  e: TEditorFrame;
  i: integer;
begin
  if FilenameIsAbsolute(FileName) then
    FileName := FCurrentProject.GetRelPath(FileName);
  e := EditorManager1.TextEditor[FCurrentProject.MainFile];
  if Assigned(e) then
  begin
    for i := 0 to e.CodeEditor.Lines.Count - 1 do
      if isEnd(e.CodeEditor.Lines[i], '#include') then
        if ExtractBetween(e.CodeEditor.Lines[i], '"', '"') =
          ChangeFileExt(FCurrentProject.MainForm, '.aal1') then
          e.CodeEditor.TextBetweenPoints[Point(1, i + 1),
            Point(Length(e.CodeEditor.Lines[i]) + 1, i + 1)] :=
            Format('#include("%s")', [FileName]);
  end
  else if FileExists(FCurrentProject.MainFile) then
  begin
    sl:=TStringList.Create;
    try
      sl.LoadFromFile(FCurrentProject.MainFile);
      for i := 0 to sl.Count - 1 do
      if isEnd(sl[i], '#include') then
        if ExtractBetween(sl[i], '"', '"') =
          ChangeFileExt(FCurrentProject.MainForm, '.aal1') then
          sl[i] := Format('#include("%s")', [FileName]);
    finally
      sl.Free;
    end;
  end;
  FCurrentProject.MainForm:=FCurrentProject.GetRelPath(FileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FFirstLoad := True;
  FCompiler := TAALCompiler.Create;
  FCompiler.OnOutput := @PrintText;
  FCompiler.OnFinishedCompiling := @FinishedComp;
  FCompiler.OnFinishedRunning := @FinishedRun;
  FCompiler.OnCompileError:=@CompileError;
  FFormIsClosing := False;
  FCurrentProject := TAALProject.Create;
  EditorManager1.EnterFunc := @EnterFunction;
  FFileData := TAALFileManager.Create;
  EditorManager1.OnParserFinished := @EditorParserFinished;
  EditorManager1.IDEOpenFile := @OpenFile;
  ProjectInspector1.ChangeMainForm := @ChangeMainForm;
  FLastOpend := TStringList.Create;
  FLastOpend.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
  i := 0;
  while i < FLastOpend.Count do
    if FileExists(FLastOpend[i]) then
      Inc(i)
    else
      FLastOpend.Delete(i);
  Application.ShowMainForm := False;
  Application.QueueAsyncCall(@ShowStartupScreen, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLastOpend.Free;
  FFileData.Free;
  FCurrentProject.Free;
  FCompiler.Free;
end;

procedure TMainForm.NewFileItemClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  i := 1;
  while (EditorManager1.Editor[IncludeTrailingPathDelimiter(
      FCurrentProject.ProjectDir) + 'AALUnit' + IntToStr(i) + '.aal1'] >= 0) or
    (FileExists(IncludeTrailingPathDelimiter(FCurrentProject.ProjectDir) +
      'AALUnit' + IntToStr(i) + '.aal1')) do
    Inc(i);
  s := IncludeTrailingPathDelimiter(FCurrentProject.ProjectDir) +
    'AALUnit' + IntToStr(i) + '.aal1';
  EditorManager1.OpenEditor(s, Point(0, 0)).Parent.Caption := '*' + ExtractFileName(s);
  FCurrentProject.AddFile(s);
end;

procedure TMainForm.NewFormItemClick(Sender: TObject);
var
  fName: string;
  sl: TStringList;
  i: integer;
begin
  i := 1;
  while FileExists(FCurrentProject.GetAbsPath('Form' + IntToStr(i) + '.afm')) do
    Inc(i);
  fName := FCurrentProject.GetAbsPath('Form' + IntToStr(i));
  sl := TStringList.Create;
  try
    sl.Text := '#include("' + 'Form' + IntToStr(i) + '.afm")';
    sl.SaveToFile(fName + '.aal1');
    sl.Clear;
    sl.Text := Format('$%s = CreateWindow("%s"), %d, %d, %d, %d, %d',
      ['Form' + IntToStr(i), 'Form' + IntToStr(i), 150, 150, 300, 200, 0]);
    sl.SaveToFile(fName + '.afm');
  finally
    sl.Free;
  end;
  FCurrentProject.AddFile(fName + '.aal1');
  FCurrentProject.AddFile(fName + '.afm');
  EditorManager1.OpenEditor(fName + '.aal1', Point(0, 0));
  EditorManager1.OpenEditor(fName + '.afm', Point(0, 0));
end;

procedure TMainForm.NewProjectItemClick(Sender: TObject);
var
  c: TCloseAction;
begin
  c := caNone;
  FormClose(Self, c);
  if EditorManager1.Count > 0 then
    exit;
  FCurrentProject.Clear;
  Application.QueueAsyncCall(@ShowStartupScreen, 0);
end;

procedure TMainForm.RenReleaseMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(SaveAllItem);
  OutputBox.Items.Clear;
  OutputBox.Items.Add('Kompiliere: ' + FCurrentProject.Name + ' Modus: ' +
    IfThen(SelectModeBox.ItemIndex = 0, 'Debug', 'Release'));
  FCompiler.CompileAndRun(FCurrentProject, TCompilerMode.cmRelease);
  RunBtn.Enabled := False;
  StopBtn.Enabled := True;
end;

procedure TMainForm.RunBtnClick(Sender: TObject);
begin
  SaveAllItemClick(SaveAllItem);
  OutputBox.Items.Clear;
  OutputBox.Items.Add('Kompiliere: ' + FCurrentProject.Name + ' Modus: ' +
    IfThen(SelectModeBox.ItemIndex = 0, 'Debug', 'Release'));
  FCompiler.CompileAndRun(FCurrentProject, TCompilerMode(SelectModeBox.ItemIndex));
  RunBtn.Enabled := False;
  StopBtn.Enabled := True;
end;

procedure TMainForm.RunDebugMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(SaveAllItem);
  OutputBox.Items.Clear;
  OutputBox.Items.Add('Kompiliere: ' + FCurrentProject.Name + ' Modus: ' +
    IfThen(SelectModeBox.ItemIndex = 0, 'Debug', 'Release'));
  FCompiler.CompileAndRun(FCurrentProject, cmDebug);
  RunBtn.Enabled := False;
  StopBtn.Enabled := True;
end;

procedure TMainForm.SaveAllItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to EditorManager1.Count - 1 do
    EditorManager1.EditorSave(i);
  FCurrentProject.Save;
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
var
  ext, oldFile, newfile: string;
  i: integer;
begin
  oldFile := EditorManager1.EditorFiles[EditorManager1.EditorIndex];
  ext := ExtractFileExt(oldFile);
  if ext = '.afm' then
    SaveAALFileDialog.Filter := 'AAL Formular|*.afm'
  else if ext = '.aal1' then
    SaveAALFileDialog.Filter := 'AAL Quellcode Datei|*.aal1'
  else if ext = '.apr' then
    SaveAALFileDialog.Filter := 'AAL Hauptdatei|*.apr';
  SaveAALFileDialog.FileName := ExtractFileName(oldFile);
  if SaveAALFileDialog.Execute then
  begin
    newfile := SaveAALFileDialog.FileName;
    if FileExists(oldFile) then
      DeleteFile(oldFile);
    FFileData.UnloadFile(FFileData.FileIndex[EditorManager1.EditorFiles[
      EditorManager1.EditorIndex]]);
    EditorManager1.EditorSave(EditorManager1.EditorIndex, SaveAALFileDialog.FileName);
    for i := 0 to FCurrentProject.Files.Count - 1 do
      if FCurrentProject.FilePath[i] = oldFile then
      begin
        FCurrentProject.FilePath[i] := SaveAALFileDialog.FileName;
        Break;
      end;
    if (oldFile = FCurrentProject.MainFile) then
      FCurrentProject.MainFile := SaveAALFileDialog.FileName;
    if ext = '.aal1' then
    begin
      oldFile := ChangeFileExt(oldFile, '.afm');
      if FileExists(oldFile) then
      begin
        // Add New Include
        AddInclude(SaveAALFileDialog.FileName,
          ChangeFileExt(SaveAALFileDialog.FileName, '.afm'));
        // Change Filename for new Form
        SaveAALFileDialog.FileName := ChangeFileExt(SaveAALFileDialog.FileName, '.afm');
        // Save form file with new name
        if EditorManager1.Editor[oldFile] >= 0 then
          EditorManager1.EditorSave(EditorManager1.Editor[oldFile],
            SaveAALFileDialog.FileName)
        else
        begin
          CopyFile(oldFile, SaveAALFileDialog.FileName);
        end;
        DeleteFile(oldFile);
        // Delete old Include
        CheckInclude(newfile, oldFile);
        if FCurrentProject.GetRelPath(oldFile) = FCurrentProject.MainForm then
        begin
          CheckInclude(FCurrentProject.MainFile, '');
          AddInclude(FCurrentProject.MainFile, newfile);
          FCurrentProject.MainForm :=
            FCurrentProject.GetRelPath(SaveAALFileDialog.FileName);
        end;
        // Change file in Project
        for i := 0 to FCurrentProject.Files.Count - 1 do
          if FCurrentProject.FilePath[i] = oldFile then
          begin
            FCurrentProject.FilePath[i] := SaveAALFileDialog.FileName;
            Break;
          end;
      end;
    end
    else if ext = '.afm' then
    begin
      oldFile := ChangeFileExt(oldFile, '.aal1');
      if FileExists(oldFile) then
      begin
        // Change Filename for new Form
        SaveAALFileDialog.FileName := ChangeFileExt(SaveAALFileDialog.FileName, '.aal1');
        // Save form file with new name
        if EditorManager1.Editor[oldFile] >= 0 then
          EditorManager1.EditorSave(EditorManager1.Editor[oldFile],
            SaveAALFileDialog.FileName)
        else
        begin
          CopyFile(oldFile, SaveAALFileDialog.FileName);
        end;
        DeleteFile(oldFile);
        // Add New Include
        AddInclude(SaveAALFileDialog.FileName,
          ChangeFileExt(SaveAALFileDialog.FileName, '.afm'));
        // Delete old Include
        CheckInclude(SaveAALFileDialog.FileName, oldFile);
        if FCurrentProject.GetRelPath(ChangeFileExt(oldFile, '.afm')) =
          FCurrentProject.MainForm then
        begin
          CheckInclude(FCurrentProject.MainFile, '');
          AddInclude(FCurrentProject.MainFile, SaveAALFileDialog.FileName);
          FCurrentProject.MainForm :=
            FCurrentProject.GetRelPath(newfile);
        end;
        // Change file in Project
        for i := 0 to FCurrentProject.Files.Count - 1 do
          if FCurrentProject.FilePath[i] = oldFile then
          begin
            FCurrentProject.FilePath[i] := SaveAALFileDialog.FileName;
            Break;
          end;
      end;
    end
    else if ext = '.apr' then
    begin
      DeleteFile(FCurrentProject.ProjectDir + FCurrentProject.Name + '.aalproj');
      FCurrentProject.WriteToFile(FCurrentProject.ProjectDir +
        ChangeFileExt(ExtractFileName(SaveAALFileDialog.FileName), '.aalproj'));
      FLastOpend.Insert(0, FCurrentProject.ProjectDir + FCurrentProject.Name +
        '.aalproj');
    end;
    FFileData.LoadFile(SaveAALFileDialog.FileName);
  end;
end;

procedure TMainForm.SaveFileItemClick(Sender: TObject);
begin
  if FileExists(EditorManager1.EditorFiles[EditorManager1.EditorIndex]) then
    EditorManager1.EditorSave(EditorManager1.EditorIndex)
  else
    SaveAsItemClick(Sender);
end;

end.
