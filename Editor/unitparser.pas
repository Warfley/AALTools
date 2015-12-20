unit UnitParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, AALTypes, strutils, Forms, Dialogs;

type
  TUnitParser = class(TThread)
  private
    FOnVarFound, FOnFuncFound, FOnFinished: TNotifyEvent;
    FText: TStringList;
    FFunc: TFuncList;
    FRanges: TObjectList;
    FVars: TVarList;
    FMyFunc: TFuncList;
    FMyRanges: TObjectList;
    FMYVars: TVarList;
    FCurr: TStringList;
    FWait: boolean;
    procedure UpdateTheShit(Data: IntPtr);
    procedure SetText(s: string);
    procedure ParseLine(ln: string; vars: TVarList; line: Integer);
    procedure ParseRange(var i: integer; endTok: string; RType: TRangeType);
  protected
    procedure Execute; override;
  public
    property Text: string write SetText;
    property Funcs: TFuncList read FFunc write FFunc;
    property Ranges: TObjectList read FRanges write FRanges;
    property Vars: TVarList read FVars write FVars;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    property OnVarFound: TNotifyEvent read FOnVarFound write FOnFuncFound;
    property OnFuncFound: TNotifyEvent read FOnFuncFound write FOnFuncFound;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
  end;

implementation


function isEnd(s, endTok: string): boolean;
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

procedure TUnitParser.SetText(s: string);
begin
  FText.Text := s;
end;

constructor TUnitParser.Create(CreateSuspended: boolean);
begin
  FMyFunc := TFuncList.Create;
  FMyRanges := TObjectList.Create(False);
  FMYVars := TVarList.Create;
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

procedure TUnitParser.ParseRange(var i: integer; endTok: string; RType: TRangeType);
function searchfor(s: TStrings; str:String; out n: Integer): Boolean;
var i: Integer;
begin
  n:=-1;
  Result:=False;
  for i:=0 to s.Count-1 do
    if LowerCase(s[i]) =  LowerCase(str) then
    begin
      n:=i;
      Result:=True;
      Exit;
    end;
end;

var
  x, n: integer;
  ln: string;
  curr: TDefRange;
begin
  if i>=FText.Count then
   Exit;
  curr := TDefRange.Create;
  curr.RangeType:=RType;
  ln := FText[i];
  curr.StartLine := i;
  ParseLine(ln, curr.Vars, i);
  inc(i);
  if i<FText.Count then
  ln := FText[i];
  while (i < FText.Count) and (not isEnd(LowerCase(ln), endTok)) and not Terminated do
  begin
    if isEnd(ln, 'if') then
      ParseRange(i, 'endif', rtIf)
    else if isEnd(ln, 'while') then
      ParseRange(i, 'wend', rtWhile)
    else if isEnd(ln, 'for') then
      ParseRange(i, 'next', rtFor)
    else
      ParseLine(ln, curr.Vars, i);
    ln := FText[i];
    Inc(i);
  end;
  curr.EndLine := i;
  FMyRanges.Add(curr);
  for x := 0 to curr.Vars.Count - 1 do
    if searchfor(FCurr, curr.Vars[x].Name, n) then
      FCurr.Delete(n);
end;

procedure TUnitParser.ParseLine(ln: string; vars: TVarList; line: Integer);

  function StringsContain(s: TStrings; str: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to s.Count - 1 do
      if LowerCase(str) = LowerCase(s[i]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  i, s, len: integer;
  str: string;
begin
  i := 1;
  while (i <= Length(ln)) and not Terminated do
  begin
    if ln[i] = '$' then
    begin
      s := i;
      len := 1;
      Inc(i);
      while (i <= Length(ln)) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
      begin
        Inc(i);
        Inc(len);
      end;
      str := Copy(ln, s, len);
      if len>1 then
      if not StringsContain(FCurr, str) then
      begin
        FCurr.Add(str);
        vars.Add(VarInfo(str, line, s));
        if Assigned(FOnVarFound) then
          Application.QueueAsyncCall(TDataEvent(FOnVarFound), PtrInt(Self));
      end;
    end;
    Inc(i);
  end;
end;

procedure TUnitParser.Execute;
var
  i, x, s, len: integer;
  str, ln: string;
begin
  FCurr.Clear;
  FMyFunc.Clear;
  FMYVars.Clear;
  FMyRanges.Clear;
  i := 0;
  while (i < FText.Count) and not Terminated do
  begin
    ln := trim(FText[i]);
    if isEnd(ln, 'func') then
    begin
      len := 0;
      s := 5;
      for x := 5 to Length(ln) do
        if ln[x] in [#0..#32] then
          Inc(s)
        else
          Break;
      for x := s to Length(ln) do
      begin
        Inc(len);
        if ln[x] = ')' then
          Break;
      end;
      str := Copy(ln, s, len);
      FMyFunc.Add(FuncInfo(str, i));
      if Assigned(FOnFuncFound) then
        Application.QueueAsyncCall(TDataEvent(FOnFuncFound), PtrInt(self));
      ParseRange(i, 'endfunc', rtFunc);
    end
    else if isEnd(ln, 'if') then
      ParseRange(i, 'endif', rtIf)
    else if isEnd(ln, 'while') then
      ParseRange(i, 'wend', rtWhile)
    else if isEnd(ln, 'for') then
      ParseRange(i, 'next', rtFor)
    else
      ParseLine(ln, FMyVars, i);
    Inc(i);
  end;
  if Terminated then Exit;
  FWait := True;
  Application.QueueAsyncCall(@UpdateTheShit, 0);
  while FWait do
    Sleep(20);
  if Assigned(FOnFinished) then
    Application.QueueAsyncCall(TDataEvent(FOnFinished), PtrInt(self));
end;

procedure TUnitParser.UpdateTheShit(Data: IntPtr);
var
  i: integer;
begin
  FFunc.Assign(FMyFunc);
  FVars.Assign(FMYVars);
  for i := 0 to FRanges.Count - 1 do
    FRanges[i].Free;
  FRanges.Clear;
  for i := 0 to FMyRanges.Count - 1 do
    FRanges.Add(FMyRanges[i]);
  FWait := False;
end;

end.
