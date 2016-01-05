unit EditorManagerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Editor, FormEditor;

type
  TCloseEditorEvent = procedure(Sender: TObject; Editor: integer;
    var Proceed: boolean) of object;
  TEditorNotifyEvent = procedure(Sender: TObject; Editor: integer) of object;

  { TEditorManager }

  TEditorManager = class(TFrame)
    EditorControl: TPageControl;
    procedure EditorControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { Fields }
    FOnEditorClose: TCloseEditorEvent;
    FOnEditorCreated: TEditorNotifyEvent;
    { Functions & Procedures }
    function FindTextEditor(FileName: string): TEditorFrame;
    function FindFormEditor(FileName: string): TFormEditFrame;
    function FindEditor(FileName: string): integer;
    function GetEditor(i: integer): TFrame;
    function GetCurrentEditor: TFrame;
    procedure SetCurrentEditor(f: TFrame);
    function GetIndex: integer;
    procedure SetIndex(i: integer);
    procedure CreateEditor(FName: string; Line, Pos: integer);
    procedure EditorChanged(Sender: TObject);
  public
    function OpenEditor(FileName: string; Line, Pos: integer): TFrame;
    procedure CloseEditor(i: integer);
    { Properties }
    property TextEditor[FileName: string]: TEditorFrame read FindTextEditor;
    property FormEditor[FileName: string]: TFormEditFrame read FindFormEditor;
    property Editor[FileName: string]: integer read FindEditor;
    property Editors[i: integer]: TFrame read GetEditor;
    property CurrentEditor: TFrame read GetCurrentEditor write SetCurrentEditor;
    property EditorIndex: integer read GetIndex write SetIndex;

    { Events }
    property OnEditorClose: TCloseEditorEvent read FOnEditorClose write FOnEditorClose;
    property OnEditorCreated: TEditorNotifyEvent
      read FOnEditorCreated write FOnEditorCreated;
  end;

implementation

{$R *.lfm}

{ TEditorManager }

procedure TEditorManager.EditorControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbMiddle then
    CloseEditor(EditorControl.TabIndexAtClientPos(Point(X, Y)));
end;

function TEditorManager.FindTextEditor(FileName: string): TEditorFrame;
var
  i: integer;
begin
  i := FindEditor(FileName);
  if (i >= 0) and (Editors[i] is TEditorFrame) then
    Result := Editors[i] as TEditorFrame
  else
    Result := nil;
end;

function TEditorManager.FindFormEditor(FileName: string): TFormEditFrame;
var
  i: integer;
begin
  i := FindEditor(FileName);
  if (i >= 0) and (Editors[i] is TFormEditFrame) then
    Result := Editors[i] as TFormEditFrame
  else
    Result := nil;
end;

function TEditorManager.FindEditor(FileName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to EditorControl.PageCount - 1 do
    if ((EditorControl.Pages[i].Components[0] is TEditorFrame) and
      ((EditorControl.Pages[i].Components[0] as TEditorFrame).FileName = FileName)) or
      ((EditorControl.Pages[i].Components[0] is TFormEditFrame) and
      ((EditorControl.Pages[i].Components[0] as TFormEditFrame).FileName =
      FileName)) then
    begin
      Result := i;
      Exit;
    end;
end;

function TEditorManager.GetEditor(i: integer): TFrame;
begin
  Result := EditorControl.Pages[i].Components[0] as TFrame;
end;

function TEditorManager.GetCurrentEditor: TFrame;
begin
  if Assigned(EditorControl.ActivePage) then
    Result := EditorControl.ActivePage.Components[0] as TFrame
  else
    Result := nil;
end;

procedure TEditorManager.SetCurrentEditor(f: TFrame);
var
  i: integer;
begin
  for i := 0 to EditorControl.PageCount - 1 do
    if EditorControl.Pages[i].Components[0] = f then
      EditorControl.PageIndex := i;
end;

function TEditorManager.GetIndex: integer;
begin
  Result := EditorControl.PageIndex;
end;

procedure TEditorManager.SetIndex(i: integer);
begin
  EditorControl.PageIndex := i;
end;

procedure TEditorManager.EditorChanged(Sender: TObject);
begin
  if not ((Sender as TFrame).Parent.Caption[1] = '*') then
    (Sender as TFrame).Parent.Caption := '*' + (Sender as TFrame).Parent.Caption;
end;

procedure TEditorManager.CreateEditor(FName: string; Line, Pos: integer);
var
  tmp: TTabSheet;
  ext: string;
begin
  tmp := EditorControl.AddTabSheet;
  tmp.Caption := ExtractFileName(FName);
  tmp.Visible := True;
  EditorControl.ActivePage := tmp;
  ext := ExtractFileExt(FName);
  if ext = '.afm' then
    with TFormEditFrame.Create(tmp) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
      OnChange := @EditorChanged;
      if FileExists(FName) then
        Load(FName)
      else
        FileName := FName;
    end
  else if (ext = '.aal1') or (ext = '.apr') then
    with TEditorFrame.Create(tmp) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
      CodeEditor.SetFocus;
      OnChange := @EditorChanged;
      if FileExists(FName) then
        Load(FName)
      else
        FileName := FName;
      if (Line > 0) and (Pos > 0) then
        CodeJump(Point(Pos, Line));
    end;
end;

function TEditorManager.OpenEditor(FileName: string; Line, Pos: integer): TFrame;
var
  Index: integer;
begin
  Index := FindEditor(FileName);
  if Index = -1 then
    CreateEditor(FileName, Line, Pos)
  else
  begin
    EditorControl.PageIndex := Index;
    if (GetCurrentEditor is TEditorFrame) and (Line > 0) and (Pos > 0) then
      (GetCurrentEditor as TEditorFrame).CodeJump(Point(Pos, Line));
  end;
  Result := GetCurrentEditor;
end;

procedure TEditorManager.CloseEditor(i: integer);
var
  Proceed: boolean;
begin
  Proceed := True;
  if Assigned(FOnEditorClose) then
    FOnEditorClose(Self, i, Proceed);
  if not Proceed then
    Exit;
  EditorControl.Pages[i].Components[0].Free;
  EditorControl.Pages[i].Free;
end;

end.
