unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, contnrs;

type
  TFormEditFrame = class(TFrame)
  private
    FFileName: string;
    FEditorControls: TObjectList;
    FOnChange: TNotifyEvent;
    { private declarations }
  public
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    { public declarations }
    property FileName: string read FFileName write FFileName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

