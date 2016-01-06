unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, contnrs, AALTypes;

type
  TFormEditFrame = class(TFrame)
  private
    FFileName: string;
    FEditorControls: TObjectList;
    FOnChange: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnVarChanged: TNotifyEvent;
    { private declarations }
  public
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    { public declarations }
    property FileName: string read FFileName write FFileName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property EnterFunc: TOpenFunctionEvent read FEnterFunc write FEnterFunc;
    property OnVarChanged: TNotifyEvent read FOnVarChanged write FOnVarChanged;
  end;

implementation

{$R *.lfm}
procedure TFormEditFrame.Save(p: string = '');
begin
  if p<>'' then
  FFileName:=p;
end;

procedure TFormEditFrame.Load(p: string = '');
begin

  FFileName:=p;
end;

end.

