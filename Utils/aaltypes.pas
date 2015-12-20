unit AALTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ListRecords;

type
  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined);
  PHashInfo = ^THashInfo;

  THashInfo = record
    Key: ansistring;
    Kind: TTokenType;
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

function FuncInfo(Name: String; Line: Integer): TFuncInfo;
function VarInfo(Name: String; Line, Position: Integer): TVarInfo;
implementation

function FuncInfo(Name: String; Line: Integer): TFuncInfo;
begin
  Result.Name:=Name;
  Result.Line:=Line;
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

