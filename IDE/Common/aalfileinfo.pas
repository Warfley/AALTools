unit AALFileInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AALTypes, ListRecords, contnrs, UnitParser;

type

  { TAALFile }

  TAALFile = class
  private
    FFileName: string;
    FRequired: TStringList;
    FFuncs: TFuncList;
    FVars: TVarList;

    procedure SetFuncs(f: TFuncList);
    procedure SetVars(v: TVarList);
    procedure SetReq(s: TStringList);

    procedure Parsed(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;

    property Functions: TFuncList read FFuncs write SetFuncs;
    property Variables: TVarList read FVars write SetVars;
    property RequiredFiles: TStringList read FRequired write SetReq;
    property FileName: string read FFileName write FFileName;
  end;

  { TAALFileManager }

  TAALFileManager = class
  private
    FFiles: TObjectList;
    function GetFile(i: integer): TAALFile;
    function CheckFileName(n: string): boolean;
    function GetFileIndex(Name: string): integer;
    procedure SetFileOpend(Name: string; Open: boolean);
    function GetCount: integer;
  public
    function CreateFile(FName: string): integer;
    function LoadFile(FName: string): integer;
    procedure UnloadFile(i: integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;

    property Files[i: integer]: TAALFile read GetFile; default;
    property FileOpend[s: string]: boolean read CheckFileName write SetFileOpend;
    property FileIndex[Name: string]: integer read GetFileIndex;
    property Count: integer read GetCount;
  end;

implementation

{ TAALFile }

procedure TAALFile.SetFuncs(f: TFuncList);
var
  i: integer;
begin
  FFuncs.Clear;
  for i := 0 to f.Count - 1 do
    FFuncs.Add(FuncInfo(f[i].Name, f[i].Line, f[i].Info, FFileName));
end;

procedure TAALFile.SetVars(v: TVarList);
var
  i: integer;
begin
  FVars.Clear;
  for i := 0 to v.Count - 1 do
    FVars.Add(VarInfo(v[i].Name, v[i].Line, v[i].Pos, FFileName));
end;

procedure TAALFile.SetReq(s: TStringList);
begin
  FRequired.Clear;
  FRequired.AddStrings(s);
end;

procedure TAALFile.Parsed(Sender: TObject);
var
  tmpFunc: TFuncList;
  tmpVar: TVarList;
begin
  tmpFunc := TFuncList.Create;
  try
    tmpFunc.Assign(FFuncs);
    SetFuncs(tmpFunc);
  finally
    tmpFunc.Free;
  end;
  tmpVar := TVarList.Create;
  try
    tmpVar.Assign(FVars);
    SetVars(tmpVar);
  finally
    tmpVar.Free;
  end;
end;

constructor TAALFile.Create;
begin
  FFuncs := TFuncList.Create;
  FVars := TVarList.Create;
  FRequired := TStringList.Create;
end;

destructor TAALFile.Destroy;
begin
  FFuncs.Free;
  FVars.Free;
  FRequired.Free;
end;

procedure TAALFile.Load;
var
  u: TUnitParser;
  sl: TStringList;
begin
  if not FileExists(FFileName) then
    Exit;
  u := TUnitParser.Create(True);
  u.FreeOnTerminate := True;
  u.OnFinished := @Parsed;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FFileName);
    u.Text := sl.Text;
  finally
    sl.Free;
  end;
  u.Funcs := FFuncs;
  u.Vars := FVars;
  u.RequiredFiles := FRequired;
  u.Ranges := nil;
  u.Start;
end;

{ TAALFileManager }

function TAALFileManager.GetFile(i: integer): TAALFile;
begin
  Result := FFiles[i] as TAALFile;
end;

function TAALFileManager.CheckFileName(n: string): boolean;
begin
  Result := GetFileIndex(n) >= 0;
end;

function TAALFileManager.GetFileIndex(Name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FFiles.Count - 1 do
    if GetFile(i).FileName = Name then
    begin
      Result := i;
      Break;
    end;
end;

procedure TAALFileManager.SetFileOpend(Name: string; Open: boolean);
begin
  if Open and not CheckFileName(Name) then
    LoadFile(Name)
  else if Open and CheckFileName(Name) then
    UnloadFile(GetFileIndex(Name));
end;

function TAALFileManager.GetCount: integer;
begin
  Result := FFiles.Count;
end;

function TAALFileManager.CreateFile(FName: string): integer;
begin
  if GetFileIndex(FName) >= 0 then
  begin
    Result := GetFileIndex(FName);
    exit;
  end;
  Result := FFiles.Add(TAALFile.Create);
  (FFiles[Result] as TAALFile).FileName := FName;
end;

function TAALFileManager.LoadFile(FName: string): integer;
begin
  if GetFileIndex(FName) >= 0 then
  begin
    Result := GetFileIndex(FName);
    exit;
  end;
  Result := FFiles.Add(TAALFile.Create);
  with (FFiles[Result] as TAALFile) do
  begin
    FileName := FName;
    Load;
  end;
end;

procedure TAALFileManager.UnloadFile(i: integer);
begin
  if (i >= 0) and (i < FFiles.Count) then
  begin
    FFiles[i].Free;
    FFiles.Delete(i);
  end;
end;

procedure TAALFileManager.Clear;
begin
  while Count > 0 do
    UnloadFile(0);
end;

constructor TAALFileManager.Create;
begin
  FFiles := TObjectList.Create(False);
end;

destructor TAALFileManager.Destroy;
begin
  Clear;
  FFiles.Free;
end;

end.
