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
    OpenAALFileDialog: TOpenDialog;
    SetMainFormButton: TButton;
    RenameButton: TButton;
    DeleteButton: TButton;
    FileIconList: TImageList;
    FileInfoPanel: TPanel;
    Label1: TLabel;
    PathEdit: TLabeledEdit;
    ProjectFileTreeView: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure ProjectFileTreeViewClick(Sender: TObject);
    procedure ProjectFileTreeViewDblClick(Sender: TObject);
    procedure SetMainFormButtonClick(Sender: TObject);
  private
    FProject: TAALProject;
    FOpenEditor: TOpenEditorEvent;
    FCloseEditor: TCloseEditorEvent;
    procedure SetProject(p: TAALProject);
    procedure ProjChanged(Sender: TObject);
    { private declarations }
  public
    property Project: TAALProject read FProject write SetProject;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property CloseEditor: TCloseEditorEvent read FCloseEditor write FCloseEditor;
    { public declarations }
  end;

implementation

{$R *.lfm}

procedure TProjectInspector.ProjectFileTreeViewClick(Sender: TObject);
begin
  DeleteButton.Enabled := False;
  if Assigned(ProjectFileTreeView.Selected) then
  begin
    PathEdit.Visible := True;
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      PathEdit.Text := FProject.MainFile
    else if ProjectFileTreeView.Selected.Data = Pointer(-2) then
      PathEdit.Text := FProject.ProjectDir
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
    begin
      DeleteButton.Enabled := True;
      PathEdit.Text := FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)];
    end
    else
      PathEdit.Visible := True;
    SetMainFormButton.Visible :=
      ExtractFileExt(ProjectFileTreeView.Selected.Text) = '.afm';
    SetMainFormButton.Enabled :=
      (IntPtr(ProjectFileTreeView.Selected.Data) >= 0) and not
      (FProject.MainForm = FProject.Files[IntPtr(ProjectFileTreeView.Selected.Data)]);
  end;

end;

procedure TProjectInspector.FrameResize(Sender: TObject);
begin
  FileInfoPanel.Height := (ClientHeight - Label1.Height) div 3;
end;

procedure TProjectInspector.AddButtonClick(Sender: TObject);
var
  i: integer;
  hasForm: boolean;
begin
  if OpenAALFileDialog.Execute then
  begin
    hasForm := (ExtractFileExt(OpenAALFileDialog.FileName) = '.aal1') and
      FileExists(ChangeFileExt(OpenAALFileDialog.FileName, '.afm'));
    if Pos(FProject.ProjectDir, OpenAALFileDialog.FileName) < 1 then
      if MessageDlg('Datei nicht im Projektverzeichnis', 'Die gewählte Datei ' +
        'befindet sich nicht im Projektverzeichnis'#10#13 +
        'Wollen sie die Datei in das Projektverzeichnis kopieren?',
        mtConfirmation, mbYesNo, 'Kopieren') = mrYes then
      begin
        if hasForm then
          CopyFile(ChangeFileExt(OpenAALFileDialog.FileName, '.afm'),
            IncludeTrailingPathDelimiter(FProject.ProjectDir) +
            ChangeFileExt(ExtractFileName(OpenAALFileDialog.FileName), '.afm'));
        CopyFile(OpenAALFileDialog.FileName,
          IncludeTrailingPathDelimiter(FProject.ProjectDir) +
          ExtractFileName(OpenAALFileDialog.FileName));
        OpenAALFileDialog.FileName :=
          IncludeTrailingPathDelimiter(FProject.ProjectDir) +
          ExtractFileName(OpenAALFileDialog.FileName);
      end;
    if Assigned(FOpenEditor) then
      FOpenEditor(OpenAALFileDialog.FileName, Point(0, 0));
    for i := 0 to FProject.Files.Count - 1 do
      if FProject.FilePath[i] = OpenAALFileDialog.FileName then
        exit;
    FProject.AddFile(OpenAALFileDialog.FileName);
    if hasForm then
    begin
      OpenAALFileDialog.FileName := ChangeFileExt(OpenAALFileDialog.FileName, '.afm');
      if Assigned(FOpenEditor) then
        FOpenEditor(OpenAALFileDialog.FileName, Point(0, 0));
      for i := 0 to FProject.Files.Count - 1 do
        if FProject.FilePath[i] = OpenAALFileDialog.FileName then
          exit;
      FProject.AddFile(OpenAALFileDialog.FileName);
    end;
  end;
end;

procedure TProjectInspector.DeleteButtonClick(Sender: TObject);
var
  fm, p: string;
begin
  if MessageDlg('Wirklich löschen', 'Diese Datei wirklich löschen',
    mtConfirmation, mbYesNo, 'Bestätigen') = mrYes then
  begin
    fm := '';
    p := FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)];
    if ExtractFileExt(p) = '.aal1' then
      fm := ChangeFileExt(p, '.afm')
    else if ExtractFileExt(p) = '.afm' then
      fm := ChangeFileExt(p, '.aal1');
    if FileExists(fm) then
    begin
      DeleteFile(fm);
      if Assigned(FCloseEditor) then
        FCloseEditor(fm);
      FProject.DeleteFile(fm);
    end;
    DeleteFile(p);
    if Assigned(FCloseEditor) then
      FCloseEditor(p);
    FProject.DeleteFile(p);
  end;
end;

procedure TProjectInspector.ProjectFileTreeViewDblClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) and Assigned(FOpenEditor) then
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      FOpenEditor(FProject.MainFile, Point(0, 0))
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
      FOpenEditor(FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)],
        Point(0, 0));
end;

procedure TProjectInspector.SetMainFormButtonClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) and
    (ExtractFileExt(ProjectFileTreeView.Selected.Text) = '.afm') then
  begin
    FProject.MainForm := FProject.Files[IntPtr(ProjectFileTreeView.Selected.Data)];
    SetMainFormButton.Enabled := False;
  end;
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
