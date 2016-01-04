unit Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, FileUtil, Dialogs;

type
  TAALProject = class
  private
    FFiles: TStringList;
    FMainFile: string;
    FProjectDir: string;
    FChanged: boolean;
    FName: string;
    FGUIBased: boolean;
    FOpendFile: string;
    FOnChange: TNotifyEvent;
    procedure SetMainFile(f: string);
    function GetMainFile: string;
    procedure SetProjectDir(p: string);
    function GetAbsoluteFileName(i: integer): string;
    procedure SetAbsoluteFileName(i: integer; f: string);
    procedure FilesChange(Sender: TObject);
  public
    function GetMainFileRel: string;
    function AddFile(F: string): integer;
    procedure DeleteFile(f: string);
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Clear;
    procedure ReadFromFile(f: string);
    procedure WriteToFile(f: string);
    property MainFile: string read GetMainFile write SetMainFile;
    property FilePath[i: integer]: string read GetAbsoluteFileName;
    property Files: TStringList read FFiles;
    property ProjectDir: string read FProjectDir write SetProjectDir;
    property Changed: boolean read FChanged;
    property Name: string read FName write FName;
    property GUIBased: boolean read FGUIBased write FGUIBased;
    property OpendFile: string read FOpendFile write FOpendFile;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

procedure TAALProject.SetProjectDir(p: string);
var
  i: integer;
  tmp: string;
begin
  tmp := GetMainFile;
  FMainFile := CreateRelativePath(tmp, p, True);
  for i := 0 to FFiles.Count - 1 do
  begin
    tmp := GetAbsoluteFileName(i);
    FFiles[i] := CreateRelativePath(tmp, p, True);
  end;
  FProjectDir := p;
  FChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAALProject.FilesChange(Sender: TObject);
begin
  FChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAALProject.SetMainFile(f: string);
begin
  FChanged := True;
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  FMainFile := F;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TAALProject.GetMainFile: string;
begin
  if FilenameIsAbsolute(FMainFile) then
    Result := FMainFile
  else
    Result := CreateAbsolutePath(FMainFile, FProjectDir);
end;

function TAALProject.GetAbsoluteFileName(i: integer): string;
var
  P: string;
begin
  P := FFiles[i];
  if FilenameIsAbsolute(P) then
    Result := P
  else
    Result := CreateAbsolutePath(P, FProjectDir);
end;

procedure TAALProject.SetAbsoluteFileName(i: integer; f: string);
begin
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  FFiles[i] := F;
end;

function TAALProject.AddFile(F: string): integer;
begin
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  FFiles.Add(F);
end;

function TAALProject.GetMainFileRel: string;
begin
  Result := FMainFile;
end;

procedure TAALProject.Clear;
begin
  FFiles.Clear;
  FMainFile := '';
  FName := '';
  FProjectDir := '';
  FChanged := False;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAALProject.DeleteFile(f: string);
var
  i: integer;
begin
  if f <> FMainFile then
    for i := 0 to FFiles.Count - 1 do
      if FFiles[i] = f then
      begin
        FFiles.Delete(i);
        Break;
      end;
end;

constructor TAALProject.Create;
begin
  FFiles := TStringList.Create;
  FFiles.OnChange := @FilesChange;
end;

destructor TAALProject.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TAALProject.Load;
var
  ProjFile: TXMLDocument;
  FilesNode: TDOMNode;
  i: integer;
begin
  try
    FFiles.Clear;
    ReadXMLFile(ProjFile, IncludeTrailingPathDelimiter(FProjectDir) +
      FName + '.aalproj');
    FMainFile := ProjFile.DocumentElement.FindNode('MainFile').TextContent;
    FGUIBased := ProjFile.DocumentElement.FindNode('Apptype').TextContent = 'GUI';
    FOpendFile := ProjFile.DocumentElement.FindNode('FocusedFile').TextContent;
    FilesNode := ProjFile.DocumentElement.FindNode('Files');
    FFiles.BeginUpdate;
    try
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
        if FilesNode.ChildNodes.Item[i].NodeName = 'File' then
          FFiles.Add(FilesNode.ChildNodes.Item[i].TextContent);
    finally
      FFiles.EndUpdate;
    end;
  finally
    ProjFile.Free;
  end;
  FChanged := False;
end;

procedure TAALProject.Save;
var
  ProjFile: TXMLDocument;
  FilesNode, tmp, t: TDOMNode;
  i: integer;
  s: string;
begin
  ProjFile := TXMLDocument.Create;
  try
    tmp := ProjFile.CreateElement(FName);
    ProjFile.AppendChild(tmp);
    // Create Mainfile Node
    tmp := ProjFile.CreateElement('MainFile');
    ProjFile.DocumentElement.AppendChild(tmp);
    t := ProjFile.CreateTextNode(FMainFile);
    tmp.AppendChild(t);
    // Create GUI Node
    tmp := ProjFile.CreateElement('Apptype');
    ProjFile.DocumentElement.AppendChild(tmp);
    if FGUIBased then
      s := 'GUI'
    else
      s := 'CONSOLE';
    t := ProjFile.CreateTextNode(s);
    tmp.AppendChild(t);
    // Create Focused Node
    tmp := ProjFile.CreateElement('FocusedFile');
    ProjFile.DocumentElement.AppendChild(tmp);
    t := ProjFile.CreateTextNode(FOpendFile);
    tmp.AppendChild(t);
    // Createing file Nodes
    FilesNode := ProjFile.CreateElement('Files');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    if FFiles.Count = 0 then
      FilesNode.AppendChild(ProjFile.CreateTextNode(' '));
    for i := 0 to FFiles.Count - 1 do
    begin
      tmp := ProjFile.CreateElement('File');
      FilesNode.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FFiles[i]);
      tmp.AppendChild(t);
    end;
    WriteXMLFile(ProjFile, IncludeTrailingPathDelimiter(FProjectDir) +
      FName + '.aalproj');
  finally
    ProjFile.Free;
  end;
  FChanged := False;
end;

procedure TAALProject.ReadFromFile(f: string);
begin
  SetProjectDir(ExtractFilePath(f));
  FName := ExtractFileName(ExtractFileNameWithoutExt(f));
  Load;
end;

procedure TAALProject.WriteToFile(f: string);
begin
  SetProjectDir(ExtractFilePath(f));
  FName := ExtractFileName(ExtractFileNameWithoutExt(f));
  Save;
end;

end.
