unit CodeFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCodeFormatter = class(TObject)
  private
    FLines: TStringList;
    procedure SetLines(l: TStringList);
    function isEnd(s, endTok: string): boolean;
    function FormatLine(ln: String): String;
    function GetSpaces(Depth: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Format;
    property Lines: TStringList read FLines write SetLines;
  end;

implementation

function TCodeFormatter.isEnd(s, endTok: string): boolean;
var
  l, l2: integer;
begin
  s := Trim(s);
  l := Length(endTok);
  l2 := Length(s);
  Result := False;
  if l2 < l then
  begin
    Exit;
  end
  else
  if (l2 > l) and (AnsiStartsText(endTok, s) and (s[l + 1] in [#0..#32])) then
  begin
    Result := True;
    Exit;
  end
  else
  if LowerCase(s) = endTok then
  begin
    Result := True;
    Exit;
  end;
end;

procedure TCodeFormatter.SetLines(l: TStringList);
begin
  FLines.Clear;
  FLines.AddStrings(l);
end;

constructor TCodeFormatter.Create;
begin
  FLines := TStringList.Create;
end;

destructor TCodeFormatter.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TCodeFormatter.FormatLine(ln: String): String;
begin

end;

function TCodeFormatter.GetSpaces(Depth: Integer): String;
var i: Integer;
begin
  SetLength(Result, Depth*2);
  FillChar(Result[1], Depth*2, ' ');
end;

procedure TCodeFormatter.Format;
var i: Integer;
  depth: Integer;
begin
  depth:=0;
  for i:=0 to FLines.Count-1 do
  begin
    if isEnd(FLines[i], 'endfunc') Or isEnd(FLines[i], 'wend') Or
     isEnd(FLines[i], 'next') Or  isEnd(FLines[i], 'endif') then
     dec(depth)
    else
    if isEnd(FLines[i], 'func') Or isEnd(FLines[i], 'while') Or
     isEnd(FLines[i], 'for') Or  isEnd(FLines[i], 'if') then
     inc(depth);
    if isEnd(FLines[i], 'else') then
      FLines[i]:=GetSpaces(depth-1)+FormatLine(Trim(FLines[i]))
    else
      FLines[i]:=GetSpaces(depth)+FormatLine(Trim(FLines[i]));
  end;
end;

end.

