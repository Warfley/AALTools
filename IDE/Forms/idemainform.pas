unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Project, IDEStartupScreen, ProjectInspector;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainFormMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    CloseFileItem: TMenuItem;
    CloseAllItem: TMenuItem;
    EditMenuItem: TMenuItem;
    FormatMenuItem: TMenuItem;
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
  private
    FCurrentProject: TAALProject;
    FLastOpend: TStringList;
    { private declarations }
    procedure ShowStartupScreen(Data: IntPtr);
  public
    property CurrentProject: TAALProject read FCurrentProject;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ShowStartupScreen(Data: IntPtr);
procedure StringsDelete(s: TStrings; str: String);
var i: Integer;
begin
  for i:=0 to s.Count-1 do
    if s[i]=str then
    begin
      s.Delete(i);
    end;
end;

begin
  Self.Hide;
  StartupScreen.LastOpend:=FLastOpend;
  StartupScreen.ShowModal;
  if FileExists(StartupScreen.SelectedPath) then
  begin
    StringsDelete(FLastOpend, StartupScreen.SelectedPath);
    FLastOpend.Insert(0, StartupScreen.SelectedPath);
    FCurrentProject.ReadFromFile(StartupScreen.SelectedPath);
    ProjectInspector1.Project:=FCurrentProject;
    Self.Show;
  end
  else
    Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FLastOpend.SaveToFile(ExtractFilePath(ParamStr(0))+'LastOpend.txt');
end;

procedure TMainForm.FormCreate(Sender: TObject);
var i: Integer;
begin
  FCurrentProject := TAALProject.Create;
  FLastOpend := TStringList.Create;
  FLastOpend.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
  i:=0;
  while i<FLastOpend.Count do
    if FileExists(FLastOpend[i]) then
      inc(i)
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
  //if FCurrentProject.Changed And (MessageDlg() = mrYes)
end;

end.

