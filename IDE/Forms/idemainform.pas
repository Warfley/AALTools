unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Project, IDEStartupScreen, ProjectInspector, EditorManagerFrame,
  AALTypes, FormEditor, Editor, AALFileInfo;

type

  { TMainForm }

  TMainForm = class(TForm)
    EditorManager1: TEditorManager;
    MainFormMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    CloseFileItem: TMenuItem;
    CloseAllItem: TMenuItem;
    EditMenuItem: TMenuItem;
    FormatMenuItem: TMenuItem;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewFileItemClick(Sender: TObject);
    procedure NewProjectItemClick(Sender: TObject);
    procedure SaveAllItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveFileItemClick(Sender: TObject);
    procedure KillEditor(s: string);
  private
    FCurrentProject: TAALProject;
    FLastOpend: TStringList;
    FFileData: TAALFileManager;
    { private declarations }
    procedure EditorParserFinished(Sender: TObject);
    procedure ShowStartupScreen(Data: IntPtr);
    procedure OpenFile(Filename: string; Pos: TPoint);
    function EnterFunction(FileName, FuncName: string; Params: TStringList;
      CreateIfMissing: boolean): string;
    procedure AddInclude(FileName, IncludeFile: string);
    function CheckInclude(FileName, IncludeFile: string): boolean;
    procedure EditorClosing(Sender: TObject; Editor: integer; var Proceed: boolean);
    procedure EditorCreated(Sender: TObject; Editor: integer);
    procedure EditorChanged(Sender: TObject);
    procedure UpdateProject(Data: IntPtr);
  public
    property CurrentProject: TAALProject read FCurrentProject;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.OpenFile(Filename: string; Pos: TPoint);
begin
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
  StartupScreen.LastOpend := FLastOpend;
  StartupScreen.ShowModal;
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
  EditorManager1.OnEditorChanged := nil;
  EditorManager1.OnEditorClose := nil;
  EditorManager1.OnEditorCreated := nil;
  CloseAllItemClick(Sender);
  if EditorManager1.Count > 0 then
    CloseAction := caNone;
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
    EditorManager1.OnEditorClose := @EditorClosing;
    EditorManager1.OnEditorChanged := @EditorChanged;
    EditorManager1.OnEditorCreated := @EditorCreated;
  end;
  FLastOpend.SaveToFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
end;

procedure TMainForm.CloseFileItemClick(Sender: TObject);
begin
  EditorManager1.CloseEditor(EditorManager1.EditorIndex);
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

function TMainForm.EnterFunction(FileName, FuncName: string;
  Params: TStringList; CreateIfMissing: boolean): string;
begin
  //TODO
end;

procedure TMainForm.AddInclude(FileName, IncludeFile: string);
begin
  //TODO
end;

function TMainForm.CheckInclude(FileName, IncludeFile: string): boolean;
begin
  //TODO
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
  if Proceed then
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

procedure TMainForm.EditorParserFinished(Sender: TObject);
begin
  if Sender is TFormEditFrame then
  begin
    //TODO
  end
  else if Sender is TEditorFrame then
  begin
    //TODO
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FCurrentProject := TAALProject.Create;
  EditorManager1.EnterFunc := @EnterFunction;
  FFileData := TAALFileManager.Create;
  EditorManager1.OnParserFinished := @EditorParserFinished;
  EditorManager1.IDEOpenFile := @OpenFile;
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
  ext, oldFile: string;
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
    if FileExists(oldFile) then
      DeleteFile(oldFile);
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
        DeleteFile(oldFile);
        // Delete old Include
        CheckInclude(SaveAALFileDialog.FileName, oldFile);
        // Change Filename for new Form
        SaveAALFileDialog.FileName := ChangeFileExt(SaveAALFileDialog.FileName, '.afm');
        // Add New Include
        AddInclude(SaveAALFileDialog.FileName, SaveAALFileDialog.FileName);
        // Save form file with new name
        EditorManager1.EditorSave(EditorManager1.Editor[oldFile],
          SaveAALFileDialog.FileName);
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
