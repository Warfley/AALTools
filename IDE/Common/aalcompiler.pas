unit AALCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Project, DOM, XMLRead, XMLWrite, process;

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
    function Compile(P: TAALProject; Mode: TCompilerMode): string;
    procedure Run(Path: string; Mode: TCompilerMode);

    property CompilerReleasePath: string read FCompilerRelease write FCompilerRelease;
    property CompilerDebugPath: string read FCompilerDebug write FCompilerDebug;
    property InterpreterReleasePath: string
      read FInterpreaterRelease write FInterpreaterRelease;
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
    property OnOutput: TOutputEvent read FOnOutput write FOnOutput;
  end;

implementation

procedure TAALCompiler.ReadConf(Path: string);
var
  doc: TXMLDocument;
  tmpNode: TDOMNode;
begin
  try
    ReadXMLFile(doc, Path);
    tmpNode := doc.DocumentElement.FindNode('Compiler');
    FCompilerDebug := tmpNode.FindNode('Debug').TextContent;
    FCompilerDebug := tmpNode.FindNode('Release').TextContent;
    FCompilerOutput := tmpNode.FindNode('Output').TextContent;
    FPrintCompilerOutput := tmpNode.FindNode('PrintOutput').TextContent = 'True';
    FAdvancedCompilerOutput := tmpNode.FindNode('FullOutput').TextContent = 'True';
    tmpNode := doc.DocumentElement.FindNode('Interpret');
    FInterpreaterDebug := tmpNode.FindNode('Debug').TextContent;
    FInterpreaterRelease := tmpNode.FindNode('Release').TextContent;
    FInterpreaterOutput := tmpNode.FindNode('Output').TextContent;
    FPrintInterpreaterOutput := tmpNode.FindNode('PrintOutput').TextContent = 'True';
  finally
    doc.Free;
  end;
end;

procedure TAALCompiler.WriteConf(Path: string);
var
  doc: TXMLDocument;
  tmpNode, tmp, rootnode: TDOMNode;
begin
  doc := TXMLDocument.Create;
  try
    rootnode := doc.CreateElement('CompilerConfig');
    doc.AppendChild(rootnode);
    tmpNode := doc.CreateElement('Compiler');
    rootnode.AppendChild(tmpNode);
    // Write Compiler Debug Path
    tmp := doc.CreateElement('Debug');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerDebug));
    // Write Compiler Release Path
    tmp := doc.CreateElement('Release');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerRelease));
    // Write Compiler Output Path
    tmp := doc.CreateElement('Output');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerOutput));
    // Write Compiler Printinfo
    tmp := doc.CreateElement('PrintOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(BoolToStr(FPrintCompilerOutput, True)));
    // Write Compiler Fulloutput
    tmp := doc.CreateElement('FullOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(
      BoolToStr(FAdvancedCompilerOutput, True)));

    tmpNode := doc.CreateElement('Interpret');
    rootnode.AppendChild(tmpNode);
    // Write Compiler Debug Path
    tmp := doc.CreateElement('Debug');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterDebug));
    // Write Compiler Release Path
    tmp := doc.CreateElement('Release');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterRelease));
    // Write Compiler Output Path
    tmp := doc.CreateElement('Output');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterOutput));
    // Write Compiler Printinfo
    tmp := doc.CreateElement('PrintOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(
      BoolToStr(FPrintInterpreaterOutput, True)));
    WriteXML(doc, Path);
  finally
    doc.Free;
  end;
end;

function TAALCompiler.Compile(P: TAALProject; Mode: TCompilerMode): string;
var
  comp: TProcess;
begin

end;

procedure TAALCompiler.Run(Path: string; Mode: TCompilerMode);
begin

end;

end.
