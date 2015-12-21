unit ToolTip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, Math;

type
  TEditorToolTip = class(TGraphicControl)
  private
    FFunc: string;
    FParams: TStringList;
    FInfo: string;
    FSelectedParam: integer;
    procedure SetSelectedParam(x: integer);
    function GetFunc: string;
    procedure SetFunc(func: string);
    procedure SetInfo(inf: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowAt(X, Y, LineHight: integer);
    property SelectedParam: integer read FSelectedParam write SetSelectedParam;
    property Func: string read getFunc write SetFunc;
    property Info: string read FInfo write SetInfo;
  end;

implementation


procedure TEditorToolTip.SetSelectedParam(x: integer);
begin
  FSelectedParam := x;
  Invalidate;
end;

function TEditorToolTip.GetFunc: string;
begin
  FParams.Delimiter := ',';
  FParams.StrictDelimiter := True;
  Result := FFunc + '(' + FParams.DelimitedText + ')';
end;

procedure TEditorToolTip.SetFunc(func: string);
var
  i: integer;
begin
  FFunc := Copy(func, 1, Pos('(', func) - 1);
  FParams.Clear;
  FParams.Delimiter := ',';
  FParams.StrictDelimiter := True;
  FParams.DelimitedText := Copy(func, Pos('(', func) + 1, Length(func) -
    Pos('(', func) - 1);
  for i := 0 to FParams.Count - 1 do
    FParams[i] := Trim(FParams[i]);
  Self.Width := Max(Canvas.TextWidth(GetFunc), Canvas.TextWidth(FInfo)) + 4;
  self.Height := Canvas.TextHeight(GetFunc) + Canvas.TextHeight(FInfo) + 4;
  Invalidate;
end;

procedure TEditorToolTip.SetInfo(inf: string);
begin
  FInfo := inf;
  Self.Width := Max(Canvas.TextWidth(GetFunc), Canvas.TextWidth(FInfo)) + 4;
  self.Height := Canvas.TextHeight(GetFunc) + Canvas.TextHeight(FInfo) + 4;
  Invalidate;
end;

procedure TEditorToolTip.Paint;
var
  p, i: integer;
begin
  Canvas.Brush.Color:=Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=$00333333;
  Canvas.Rectangle(0,0, Width, Height);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style:=psClear;
  Canvas.Font := Font;
  p := 2;
  Canvas.TextOut(p, 1, FFunc + '(');
  Inc(p, Canvas.TextWidth(FFunc + '('));
  for i := 0 to FParams.Count - 1 do
  begin
    if SelectedParam = i then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Canvas.TextOut(p, 1, FParams[i]);
    Inc(p, Canvas.TextWidth(FParams[i]));
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    if i <> FParams.Count - 1 then
    begin
      Canvas.TextOut(p, 1, ',');
      Inc(p, Canvas.TextWidth(','));
    end;
  end;
  Canvas.TextOut(p, 1, ')');
  Canvas.TextOut(2, Canvas.TextHeight(FFunc) + 3, FInfo);
  inherited;
end;

constructor TEditorToolTip.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TStringList.Create;
  FSelectedParam:=-1;
  Visible:=False;
end;

destructor TEditorToolTip.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TEditorToolTip.ShowAt(X, Y, LineHight: integer);
begin
  if x + Width > Parent.Width then
    x := Max(0, Parent.Width - Self.Width);
  if y + Height > Parent.Height then
    y := y - Height
    else
      y:=y+LineHight;
  Left := X;
  Top := Y;
  Visible := True;
end;

end.
