unit AALTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ListRecords;

type
  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined, tkDoc);
  PHashInfo = ^THashInfo;

  THashInfo = record
    Key: ansistring;
    Kind: TTokenType;
  end;

  TSelectedItem = record
    Line, Pos: Integer;
  end;

  TFuncList = specialize TFPGList<TFuncInfo>;
  TVarList = specialize TFPGList<TVarInfo>;

  TRangeType = (rtFunc, rtWhile, rtIf, rtFor);
  TDefRange = class
  private
    FStart, FEnd: integer;
    FVars: TVarList;
    FRangeType: TRangeType;
  public
    constructor Create;
    destructor Destroy; override;
    property Vars: TVarList read FVars;
    property StartLine: integer read FStart write FStart;
    property RangeType: TRangeType read FRangeType write FRangeType;
    property EndLine: integer read FEnd write FEnd;
  end;

function FuncInfo(Name: String; Line: Integer; Inf: String=''): TFuncInfo;
function SelectedItem(Line, Pos: Integer): TSelectedItem;
function VarInfo(Name: String; Line, Position: Integer): TVarInfo;
function isEnd(s, endTok: string): boolean;
implementation


function isEnd(s, endTok: string): boolean;
function getFirstTok(str: String): String;
var i, len: Integer;
begin
  len:=1;
  if length(str)>1 then
  begin
    i:=2;
    while (str[i] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do
    begin
      inc(i);
      inc(len);
    end;
  end;
  Result:=Copy(str, 1, len);
end;

var
  l, l2: integer;
begin
  s := Trim(s);
  s:=getFirstTok(s);
  l := Length(endTok);
  l2 := Length(s);
  Result := False;
  if l2 < l then
  begin
    Exit;
  end
  else if LowerCase(s) = endTok then
  begin
    Result := True;
    Exit;
  end;
end;

function SelectedItem(Line, Pos: Integer): TSelectedItem;
begin
  Result.Line:=Line;
  Result.Pos:=Pos;
end;

function FuncInfo(Name: String; Line: Integer; Inf: String=''): TFuncInfo;
begin
  Result.Name:=Name;
  Result.Line:=Line;
  Result.Info:=Inf;
end;

function VarInfo(Name: String; Line, Position: Integer): TVarInfo;
begin
  Result.Name:=Name;
  Result.Line:=Line;
  Result.Pos:=Position;
end;

constructor TDefRange.Create;
begin
  FVars := TVarList.Create;
end;

destructor TDefRange.Destroy;
begin
  FVars.Free;
  inherited;
end;

end.

