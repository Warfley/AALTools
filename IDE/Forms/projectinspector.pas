unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, Project, Dialogs, AALTypes;

type

  { TProjectInspector }

  TProjectInspector = class(TFrame)
    AddButton: TButton;
    RenameButton: TButton;
    DeleteButton: TButton;
    FileIconList: TImageList;
    FileInfoPanel: TPanel;
    Label1: TLabel;
    PathEdit: TLabeledEdit;
    ProjectFileTreeView: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure ProjectFileTreeViewClick(Sender: TObject);
    procedure ProjectFileTreeViewDblClick(Sender: TObject);
  private
    FProject: TAALProject;
    FOpenEditor: TOpenEditorEvent;
    procedure SetProject(p: TAALProject);
    procedure ProjChanged(Sender: TObject);
    { private declarations }
  public
    property Project: TAALProject read FProject write SetProject;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    { public declarations }
  end;

implementation

{$R *.lfm}

procedure TProjectInspector.ProjectFileTreeViewClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) then
  begin
    PathEdit.Visible := True;
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      PathEdit.Text := FProject.MainFile
    else if ProjectFileTreeView.Selected.Data = Pointer(-2) then
      PathEdit.Text := FProject.ProjectDir
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
      PathEdit.Text := FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)]
    else
      PathEdit.Visible := True;
  end;

end;

procedure TProjectInspector.ProjectFileTreeViewDblClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) And Assigned(FOpenEditor) then
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      FOpenEditor(FProject.MainFile, Point(-1, -1))
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
      FOpenEditor(FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)], Point(-1, -1))
end;

procedure TProjectInspector.SetProject(p: TAALProject);
begin
  FProject := p;
  FProject.OnChange := @ProjChanged;
  ProjChanged(FProject);
end;

procedure TProjectInspector.ProjChanged(Sender: TObject);

  function CreateDirNode(p: string): TTreeNode;
  var
    s: string;
    i: integer;
  begin
    s := ExtractFileName(ExcludeTrailingPathDelimiter(p));
    if s = '' then
      Result := ProjectFileTreeView.Items[0]
    else
    begin
      Result := CreateDirNode(ExtractFilePath(ExcludeTrailingPathDelimiter(p)));
      for i := 0 to Result.Count - 1 do
        if Result.Items[i].Text = s then
        begin
          Result := Result.Items[i];
          Exit;
        end;
      Result := ProjectFileTreeView.Items.AddChild(Result, s);
      Result.ImageIndex := 1;
      Result.SelectedIndex := 1;
      Result.Data := Pointer(-3);
    end;
  end;

var
  i: integer;
  s: string;
  p: TTreeNode;
  ext: string;
  tmp: TTreeNode;
begin
  ProjectFileTreeView.Items.Clear;
  tmp := ProjectFileTreeView.Items.Add(nil, FProject.Name);
  tmp.ImageIndex := 4;
  tmp.SelectedIndex := 4;
  tmp.Data := Pointer(-2);
  s := FProject.GetMainFileRel;
  p := CreateDirNode(ExtractFilePath(s));
  tmp := ProjectFileTreeView.Items.AddChild(p, ExtractFileName(s));
  tmp.ImageIndex := 2;
  tmp.SelectedIndex := 2;
  tmp.Data := Pointer(-1);
  for i := 0 to FProject.Files.Count - 1 do
  begin
    if not FileExists(FProject.FilePath[i]) then
      Continue;
    s := FProject.Files[i];
    p := CreateDirNode(ExtractFilePath(s));
    ext := ExtractFileExt(s);
    tmp := ProjectFileTreeView.Items.AddChild(p, ExtractFileName(s));
    tmp.Data := Pointer(i);
    if ext = '.aal1' then
    begin
      tmp.ImageIndex := 2;
      tmp.SelectedIndex := 2;
    end
    else if ext = '.afm' then
    begin
      tmp.ImageIndex := 3;
      tmp.SelectedIndex := 3;
    end
    else
    begin
      tmp.ImageIndex := 0;
      tmp.SelectedIndex := 0;
    end;
  end;
  ProjectFileTreeView.FullExpand;
end;

end.
