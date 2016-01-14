unit AALCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Project;

type
  TCompilerMode = (cmDebug, cmRelease);
  TOutputEvent = procedure(Sender: TObject; FileName: string; Output: string);

  TAALCompiler = class
  private
    FCompilerRelease: string;
    FCompilerDebug: string;
    FInterpreaterRelease: string;
    FInterpreaterDebug: string;
    FCompilerOutput: string;
    FInterpreaterOutput: string;
    FPrintCompilerOutput: boolean;
    FAdvancedCompilerOutput: boolean;
    FPrintInterpreaterOutput: boolean;
    FOnOutput: TOutputEvent;
  public
    procedure ReadConf(Path: string);
    procedure WriteConf(Path: string);
    constructor Create; overload;
    constructor Create(ConfPath: string); overload;
    function Compile(P: TAALProject; Mode: TCompilerMode): string;
    procedure Run(Path: string; Mode: TCompilerMode);

    property CompilerReleasePath: string read FCompilerRelease write FCompilerRelease;
    property CompilerDebugPath: string read FCompilerDebug write FCompilerDebug;
    property InterpreterReleasePath: string read FInterpreaterRelease
      write FInterpreaterRelease;
    property InterpreterDebugPath: string read FInterpreaterDebug
      write FInterpreaterDebug;
    property InterpreterOutputPath: string read FInterpreaterOutput
      write FInterpreaterOutput;
    property CompilerOutputPath: string read FCompilerOutput write FCompilerOutput;
    property PrintCompilerOutput: boolean read FPrintCompilerOutput
      write FPrintCompilerOutput;
    property AdvancedCompilerOutput: boolean
      read FAdvancedCompilerOutput write FAdvancedCompilerOutput;
    property PrintInterpreaterOutput: boolean
      read FPrintInterpreaterOutput write FPrintInterpreaterOutput;
  end;

implementation

end.
