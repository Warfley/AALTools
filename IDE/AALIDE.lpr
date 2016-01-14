program AALIDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, IDEMainForm, Project, IDEStartupScreen, ProjectInspector,
  AALTypes, Editor, FormEditor, EditorManagerFrame, AALFileInfo, 
FormEditComponents, AALCompiler
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStartupScreen, StartupScreen);
  Application.Run;
end.

