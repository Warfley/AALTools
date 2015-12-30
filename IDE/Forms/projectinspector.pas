unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, Project;

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
  private
    FProject: TAALProject;
    procedure SetProject(p: TAALProject);
    procedure ProjChanged(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

procedure TProjectInspector.SetProject(p: TAALProject);
begin
  FProject := p;
  FProject.OnChange := @ProjChanged;
end;

procedure TProjectInspector.ProjChanged(Sender: TObject);
begin

end;

end.

