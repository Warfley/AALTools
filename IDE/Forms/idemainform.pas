unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Project,
  IDEStartupScreen;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentProject: TAALProject;
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
    begin
      Self.Hide;
      StartupScreen.ShowModal;
      Self.Show;
    end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.ShowMainForm:=False;
  Application.QueueAsyncCall(@ShowStartupScreen, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCurrentProject) then
    FCurrentProject.Free;
end;

end.

