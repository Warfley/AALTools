unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Editor;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    EditorFrame1: TEditorFrame;
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    EditorFrame1.Load(OpenDialog1.FileName);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if FileExists(OpenDialog1.FileName) then
      EditorFrame1.Load(OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  if EditorFrame1.FileName = '' then
    MenuItem4Click(nil)
  else
    EditorFrame1.Save();
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
      DeleteFile(SaveDialog1.FileName);
    EditorFrame1.Save(SaveDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  FontDialog1.Font := EditorFrame1.Font;
  if FontDialog1.Execute then
    EditorFrame1.Font := FontDialog1.Font;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  ColorDialog1.Color := EditorFrame1.CodeEditor.Color;
  if ColorDialog1.Execute then
    EditorFrame1.CodeEditor.Color := ColorDialog1.Color;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  ColorDialog1.Color := EditorFrame1.CodeEditor.Gutter.Color;
  if ColorDialog1.Execute then
    EditorFrame1.CodeEditor.Gutter.Color := ColorDialog1.Color;
end;

end.

