unit UnitParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, AALTypes, strutils, Forms;

type
  TUnitParser = class(TThread)
  private
    FText: TStringList;
    FFunc: TStringList;
    FRanges: TObjectList;
    FVars: TStringList;
    FMyFunc: TStringList;
    FMyRanges: TObjectList;
    FMYVars: TStringList;
    FCurr: TStringList;
    FWait: Boolean;
    procedure UpdateTheShit(Data: IntPtr);
    procedure SetText(s: string);
    procedure ParseLine(ln: string; vars: TStringList);
    procedure ParseRange(var i: integer; endTok: string);
  protected
    procedure Execute; override;
  public
    property Text: string write SetText;
    property Funcs: TStringList read FFunc write FFunc;
    property Ranges: TObjectList read FRanges write FRanges;
    property Vars: TStringList read FVars write FVars;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

implementation

procedure TUnitParser.SetText(s: string);
begin
  FText.Text := s;
end;

constructor TUnitParser.Create(CreateSuspended: boolean);
begin
  FMyFunc := TStringList.Create;
  FMyRanges := TObjectList.Create(False);
  FMYVars := TStringList.Create;
  FCurr := TStringList.Create;
  FText := TStringList.Create;
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
end;

destructor TUnitParser.Destroy;
begin
  FMYVars.Free;
  FMyFunc.Free;
  FMyRanges.Free;
  FCurr.Free;
  FText.Free;
  inherited Destroy;
end;

procedure TUnitParser.ParseRange(var i: integer; endTok: string);

  function isEnd(s: string): boolean;
  begin
    s := Trim(s);
    Result := False;
    if Length(s) < Length(endTok) then
    begin
      Exit;
    end
    else
    if (Length(s) > Length(endTok)) and (AnsiStartsText(endTok, s) and
      (s[Length(endTok) + 1] in [#0..#32])) then
    begin
      Result := True;
      Exit;
    end
    else
    if s = LowerCase(endTok) then
    begin
      Result := True;
      Exit;
    end;

  end;

var
  x, n: integer;
  ln: string;
  curr: TDefRange;
begin
  curr := TDefRange.Create;
  ln:=FText[i];
  curr.StartLine := i;
  while (i < FText.Count) and (not isEnd(LowerCase(ln))) do
  begin
    ParseLine(ln, curr.Vars);
    ln := FText[i];
    Inc(i);
  end;
  curr.EndLine := i;
  FMyRanges.Add(curr);
  for x := 0 to curr.Vars.Count - 1 do
    if FCurr.Find(curr.Vars[x], n) then
      FCurr.Delete(n);
end;

procedure TUnitParser.ParseLine(ln: string; vars: TStringList);
function StringsContain(s: TStrings; str: String): Boolean;
var i: Integer;
begin
  Result:=False;
  for i:=0 to s.Count-1 do
    if LowerCase(str) = LowerCase(s[i]) then
    begin
      Result:=True;
      Break;
    end;
end;
var i, s, len: Integer;
  str:String;
begin
  i:=1;
  While i<=Length(ln) do
  begin
    if ln[i] = '$' then
    begin
      s:=i;
      len:=1;
      inc(i);
      while (i<=Length(ln)) And (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
      begin
        inc(i);
        inc(len);
      end;
      str:=Copy(ln, s, len);
      if not StringsContain(FCurr, str) then
      begin
        FCurr.Add(str);
        vars.Add(str);
      end;
    end;
    inc(i);
  end;
end;

procedure TUnitParser.Execute;
var i, x, s, len: Integer;
  str, ln: String;
begin
  FCurr.Clear;
  FMyFunc.Clear;
  FMYVars.Clear;
  FMyRanges.Clear;
  i:=0;
  while i<FText.Count do
  begin
    ln:=trim(FText[i]);
    if AnsiStartsText('func', ln) then
    begin
      len:=0;
      s:=5;
      for x:=5 to Length(ln) do
        if ln[x] in [#0..#32] then
          inc(s)
        else
          Break;
      for x:=s to Length(ln) do
      begin
        inc(len);
        if ln[x] =')' then
          Break;
      end;
      str := Copy(ln, s, len);
      FMyFunc.Add(str);
      ParseRange(i, 'endfunc');
    end
    else
      ParseLine(ln, FMyVars);
    inc(i);
  end;
  FWait:=True;
  Application.QueueAsyncCall(@UpdateTheShit, 0);
  while FWait do
    Sleep(20);
end;

procedure TUnitParser.UpdateTheShit(Data: IntPtr);
var
  i: integer;
begin
  FFunc.Clear;
  FFunc.AddStrings(FMyFunc);
  FVars.Clear;
  FVars.AddStrings(FMYVars);
  for i := 0 to FRanges.Count - 1 do
    FRanges[i].Free;
  FRanges.Clear;
  for i := 0 to FMyRanges.Count - 1 do
    FRanges.Add(FMyRanges[i]);
  FWait:=False;
end;

end.
