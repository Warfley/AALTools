unit AALTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined);
  PHashInfo = ^THashInfo;

  THashInfo = record
    Key: ansistring;
    Kind: TTokenType;
  end;

  TDefRange = class
  private
    FStart, FEnd: integer;
    FVars: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property Vars: TStringList read FVars;
    property StartLine: integer read FStart write FStart;
    property EndLine: integer read FEnd write FEnd;
  end;

implementation


constructor TDefRange.Create;
begin
  FVars := TStringList.Create;
end;

destructor TDefRange.Destroy;
begin
  FVars.Free;
  inherited;
end;

end.

