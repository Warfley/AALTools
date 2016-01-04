unit ListRecords;

{$mode Delphi}{$H+}

// Needed Second unit for DELPHI Operator overload

interface

uses
  SysUtils;

type

  TFuncInfo = record
    Name: String;
    Info: String;
    FileName: String;
    Line: Integer;
    class operator Equal (a, b: TFuncInfo) R: Boolean;
  end;

  TVarInfo = record
    Name: String;
    FileName: String;
    Line: Integer;
    Pos: Integer;
    class operator Equal (a, b: TVarInfo) R: Boolean;
  end;

implementation
    class operator TFuncInfo.Equal (a, b: TFuncInfo) R: Boolean;
    begin
      R:=(a.Name=b.Name) And (a.Line=b.Line);
    end;
    class operator TVarInfo.Equal (a, b: TVarInfo) R: Boolean;
    begin
      R:=(a.Name=b.Name) And (a.Line=b.Line) And (a.Pos=b.Pos);
    end;

end.

