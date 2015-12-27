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
    { private declarations }
  public
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    { public declarations }
    property FileName: string read FFileName write FFileName;
  end;

implementation

{$R *.lfm}
procedure TFormEditFrame.Save(p: string = '');
begin

end;

procedure TFormEditFrame.Load(p: string = '');
begin

end;

end.

