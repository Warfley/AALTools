unit IDEStartupScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, ComCtrls, EditBtn;

type

  { TStartupScreen }

  TStartupScreen = class(TForm)
    Button1: TButton;
    CreateBTN: TButton;
    ClosePanelButton: TButton;
    ImageList1: TImageList;
    NewProjectDirEdit: TDirectoryEdit;
    NewProjectNameEdit: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LastOpendBox: TListBox;
    NewProjectView: TListView;
    NewButton: TSpeedButton;
    OpenButton: TSpeedButton;
    LastOpendPanel: TPanel;
    OpenProjDialog: TOpenDialog;
    NewProjectPanel: TPanel;
    procedure ClosePanelButtonClick(Sender: TObject);
    procedure CreateBTNClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastOpendBoxDblClick(Sender: TObject);
    procedure LastOpendPanelPaint(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    FPath: string;
    FLastOpend: TStringList;
    procedure SetLastOpend(s: TStringList);
    { private declarations }
  public
    property LastOpend: TStringList read FLastOpend write SetLastOpend;
    property Path: string read FPath;
    { public declarations }
  end;

var
  StartupScreen: TStartupScreen;

implementation

{$R *.lfm}

{ TStartupScreen }

procedure TStartupScreen.LastOpendPanelPaint(Sender: TObject);
begin
  (Sender as TCustomControl).Canvas.Pen.Color := clHighlight;
  (Sender as TCustomControl).Canvas.Brush.Style := bsClear;
  (Sender as TCustomControl).Canvas.Pen.Style := psSolid;
  (Sender as TCustomControl).Canvas.Rectangle(0, 0, (Sender as TControl).Width,
    (Sender as TControl).Height);
end;

procedure TStartupScreen.NewButtonClick(Sender: TObject);
begin
  NewProjectPanel.Visible := True;
  LastOpendPanel.Visible := False;
end;

procedure TStartupScreen.OpenButtonClick(Sender: TObject);
begin
  if OpenProjDialog.Execute then
  begin
    ModalResult := mrYes;
    FPath := OpenProjDialog.FileName;
    Close;
  end;
end;

procedure TStartupScreen.SetLastOpend(s: TStringList);
var
  i: integer;
begin
  FLastOpend.Assign(s);
  LastOpendBox.Clear;
  for i := 0 to FLastOpend.Count - 1 do
    LastOpendBox.Items.Add(ExtractFileName(ExtractFileNameWithoutExt(FLastOpend[i])));
end;

procedure TStartupScreen.LastOpendBoxDblClick(Sender: TObject);
begin
  if LastOpendBox.ItemIndex >= 0 then
  begin
    ModalResult := mrYes;
    FPath := FLastOpend[LastOpendBox.ItemIndex];
    Close;
  end;
end;

procedure TStartupScreen.FormCreate(Sender: TObject);
begin
  FLastOpend := TStringList.Create;
end;

procedure TStartupScreen.ClosePanelButtonClick(Sender: TObject);
begin
  NewProjectPanel.Hide;
  LastOpendPanel.Show;
end;

procedure TStartupScreen.CreateBTNClick(Sender: TObject);
begin
  ClosePanelButtonClick(Sender);
  Close;
end;

procedure TStartupScreen.FormDestroy(Sender: TObject);
begin
  FLastOpend.Free;
end;

procedure TStartupScreen.FormShow(Sender: TObject);
  procedure GetSubDirs(Dir: string; slt: TStrings);
  var
    srSearch: TSearchRec;
  begin
    slt.BeginUpdate;
    try
      Dir := IncludeTrailingPathDelimiter(Dir);
      if FindFirst(Dir + '*', faDirectory, srSearch) = 0 then
        repeat
          if ((srSearch.Attr and faDirectory) = faDirectory) and
            (srSearch.Name <> '.') and
            (srSearch.Name <> '..') then
          begin
            slt.Add(srSearch.Name);
          end;
        until (FindNext(srSearch) <> 0);

    finally
      slt.EndUpdate;
      FindClose(srSearch);
    end;
  end;

var lst: TStringList;
  i: Integer;
begin
  NewProjectDirEdit.Directory :=
    GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') +
    PathDelim + 'AAL' + PathDelim + 'AALProjekt1';
  lst:=TStringList.Create;
  try
    GetSubDirs(ExtractFilePath(ParamStr(0))+'Presets', lst);
    for i:=0 to lst.Count-1 do
      with NewProjectView.Items.Add do
      begin
        ImageIndex:=0;
        Caption:=lst[i];
      end;
  finally
    lst.Free;
  end;
end;

end.
