unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Project, IDEStartupScreen, ProjectInspector, EditorManagerFrame;

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
    SaveAsItem: TMenuItem;
    SaveFileItem: TMenuItem;
    SaveAllItem: TMenuItem;
    MenuSplitItem5: TMenuItem;
    MenuSplitItem4: TMenuItem;
    MenuSplitItem3: TMenuItem;
    ProjectInspector1: TProjectInspector;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewProjectItemClick(Sender: TObject);
    procedure SaveFileItemClick(Sender: TObject);
  private
    FCurrentProject: TAALProject;
    FLastOpend: TStringList;
    { private declarations }
    procedure ShowStartupScreen(Data: IntPtr);
    procedure OpenFile(Filename: string; Pos: TPoint);
    function EnterFunction(FileName, FuncName: string; Params: TStringList;
      CreateIfMissing: boolean): string;
    procedure AddInclude(FileName, IncludeFile: string);
    function CheckInclude(FileName, IncludeFile: string): boolean;
    procedure EditorClosing(Sender: TObject; Editor: integer; var Proceed: boolean);
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
  EditorManager1.OpenEditor(Filename, Pos.y, Pos.x);
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
    Self.Show;
    for i := 0 to FCurrentProject.OpendFiles.Count - 1 do
      EditorManager1.OpenEditor(FCurrentProject.GetAbsPath(
        FCurrentProject.OpendFiles[i].Name),
        FCurrentProject.OpendFiles[i].Line, FCurrentProject.OpendFiles[i].Pos);
    EditorManager1.EditorIndex := FCurrentProject.FocusedFile;
    ProjectInspector1.OpenEditor := @OpenFile;
  end
  else
    Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FLastOpend.SaveToFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
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
      'Datei wurde Verändert'#10#13'Vor dem schließen Sichern?', mtConfirmation,
      mbYesNoCancel, 'Schließen?');
    case res of
      mrYes: SaveFileItemClick(SaveFileItem);
      mrNo: Exit;
      mrCancel: Proceed := False;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FCurrentProject := TAALProject.Create;
  FLastOpend := TStringList.Create;
  EditorManager1.OnEditorClose := @EditorClosing;
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
  FCurrentProject.Free;
end;

procedure TMainForm.NewProjectItemClick(Sender: TObject);
begin

end;

procedure TMainForm.SaveFileItemClick(Sender: TObject);
begin

end;

end.
