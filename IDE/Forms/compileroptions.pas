unit CompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TCompilerOptionsForm }

  TCompilerOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IDebugFileEdit: TFileNameEdit;
    ILogEdit: TEdit;
    COutputBox: TCheckBox;
    CAdvOutputBox: TCheckBox;
    CompilerConfigBox: TGroupBox;
    IOutputBox: TCheckBox;
    CLogEdit: TEdit;
    CDebugFileEdit: TFileNameEdit;
    CReleaseFileEdit: TFileNameEdit;
    IReleaseFileEdit: TFileNameEdit;
    InterpreterConfigBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CompilerOptionsForm: TCompilerOptionsForm;

implementation

{$R *.lfm}

end.

