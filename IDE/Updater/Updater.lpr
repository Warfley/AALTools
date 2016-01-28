program Updater;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
     {$ENDIF}   {$ENDIF}
 //{$IfDef Windows}windows, TlHelp32,{$ENDIF}
  SysUtils,
  Classes,
  fphttpclient,
  sha1,
  crt,
  process
 ;

type
  TPrintObject = class
  public
    DownloadName: string;
    procedure PrintOutput(Sender: TObject; const ContentLength, CurrentPos: int64);
  end;

const
  SUpdateURL = 'http://kehrein.org/AET/Updates/';


  procedure TPrintObject.PrintOutput(Sender: TObject;
  const ContentLength, CurrentPos: int64);
  begin
    GotoXY(Length(DownloadName) + 1, WhereY);
    ClrEol;
    Write(CurrentPos);
    Write(' Bytes geladen...');
  end;

  function GetHash(FileName: string): string; inline;
  begin
    Result := SHA1Print(SHA1File(FileName));
  end;

var
  sl: TStringList;
  po: TPrintObject;
  i: integer;
  f: TFPHTTPClient;
  p: TProcess;

{$R *.res}

(*{$IFDEF Windows}
function KillTask(ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
      Result := Integer(TerminateProcess(
                        OpenProcess(PROCESS_TERMINATE,
                                    BOOL(0),
                                    FProcessEntry32.th32ProcessID),
                                    0));
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;
{$EndIf} *)

begin
  sl := TStringList.Create;
  po := TPrintObject.Create;
  f := TFPHTTPClient.Create(nil);
  try
    (*{$IfDef WINDOWS}KillTask('AALIDE.exe');{$else}*)Sleep(1000);//{$EndIf}
    f.OnDataReceived := @po.PrintOutput;
    TextColor(LightRed);
    po.DownloadName := 'Updateinformationen werden geladen: ';
    Write('Updateinformationen werden geladen: ');
    TextColor(White);
    sl.Text := f.Get(SUpdateURL + 'Update.txt');
    TextColor(LightGreen);
    WriteLn('Fertig');
    TextColor(White);
    WriteLn('Update auf Version: ' + sl[0]);
    WriteLn(sl[1] + ' Dateien');
    for i := 2 to sl.Count - 1 do
      if sl[i] <> '' then
      begin
        f.RequestHeaders.Clear;
        TextColor(White);
        Write('Prüfe Datei ' + sl.Names[i] + '... ');
        if (not FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
          sl.Names[i])) or (GetHash(IncludeTrailingPathDelimiter(
          ExtractFilePath(ParamStr(0))) + sl.Names[i]) <> sl.ValueFromIndex[i]) then
        begin
          WriteLn('');
          TextColor(LightRed);
          po.DownloadName := Format('Datei %s wird geladen: ', [sl.Names[i]]);
          Write(Format('Datei %s wird geladen: ', [sl.Names[i]]));
          TextColor(White);
          ForceDirectories(ExtractFilePath(IncludeTrailingPathDelimiter(
            ExtractFilePath(ParamStr(0))) + sl.Names[i]));
          if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + sl.Names[i]) then
          DeleteFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + sl.Names[i]);
          f.Get(SUpdateURL + StringReplace(ExtractFilePath(sl.Names[i]), '\', '/', [rfReplaceAll])+EncodeURLElement(ExtractFileName(sl.Names[i])),
            IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + sl.Names[i]);
        end;
        TextColor(LightGreen);
        WriteLn('Fertig');
      end;
    TextColor(Yellow);
    WriteLn('Update abgeschlossen');
    Write('Mit beliebiger Taste Beenden...');
    ReadKey;
    p := TProcess.Create(nil);
    try
      p.Executable := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
        'AALIDE.exe';
      p.Execute;
    finally
      p.Free;
    end;
  finally
    sl.Free;
    po.Free;
    f.Free;
  end;
end.
