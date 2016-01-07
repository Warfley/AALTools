unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  ExtCtrls, StdCtrls, ValEdit, ComCtrls, Grids, contnrs, AALTypes, Dialogs;

type

  { TFormEditFrame }

  TFormEditFrame = class(TFrame)
    FormCaptionLabel: TLabel;
    FormPanel: TPanel;
    ImageList1: TImageList;
    ToolSelect: TListView;
    ToolboxHeaderPanel: TPanel;
    ToolBoxPanel: TPanel;
    PropertyPages: TPageControl;
    PropertyPanel: TPanel;
    ControlProps: TTabSheet;
    ControlEvents: TTabSheet;
    EditorScrollBox: TScrollBox;
    TreeFilterEdit1: TTreeFilterEdit;
    FormControlView: TTreeView;
    PropEditor: TValueListEditor;
    EventEditor: TValueListEditor;
    procedure FormControlViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormControlViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure FormControlViewKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelPaint(Sender: TObject);
    procedure PropEditorSetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure PropertyPanelResize(Sender: TObject);
    procedure ToolboxHeaderPanelClick(Sender: TObject);
    procedure ToolboxHeaderPanelMouseEnter(Sender: TObject);
    procedure ToolboxHeaderPanelMouseLeave(Sender: TObject);
  private
    FFileName: string;
    FFormName: string;
    FFormStyle: integer;
    FFormLeft: integer;
    FFormTop: integer;
    FEditorControls: TObjectList;
    FSelected: TControl;
    FMousePoint: TPoint;
    FPanelMousePoint: TPoint;
    FSelPoint: TPoint;
    FOnChange: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnVarChanged: TNotifyEvent;
    { private declarations }
    procedure DeleteItem(n: TTreeNode);
    procedure CheckBoxClicked(Sender: TObject);
    function FindControl(s: string): integer;
    function CreateButton(P: TWinControl): TButton;
    function CreateCheckBox(P: TWinControl): TCheckBox;
    function CreateLabel(P: TWinControl): TLabel;
    function CreateEdit(P: TWinControl): TEdit;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    { public declarations }
    property FileName: string read FFileName write FFileName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property EnterFunc: TOpenFunctionEvent read FEnterFunc write FEnterFunc;
    property OnVarChanged: TNotifyEvent read FOnVarChanged write FOnVarChanged;
  end;

implementation

{$R *.lfm}


function TFormEditFrame.FindControl(s: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FEditorControls.Count - 1 do
    if LowerCase((FEditorControls[i] as TControl).Name) = LowerCase(s) then
    begin
      Result := i;
      Break;
    end;
end;

function TFormEditFrame.CreateButton(P: TWinControl): TButton;
var
  i: integer;
begin
  Result := TButton.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('Button' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Button' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 1;
        SelectedIndex := 1;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

function TFormEditFrame.CreateCheckBox(P: TWinControl): TCheckBox;
var
  i: integer;
begin
  Result := TCheckBox.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('CheckBox' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'CheckBox' + IntToStr(i);
  Result.OnChange := @CheckBoxClicked;
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 2;
        SelectedIndex := 2;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

function TFormEditFrame.CreateLabel(P: TWinControl): TLabel;
var
  i: integer;
begin
  Result := TLabel.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('Label' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Label' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 4;
        SelectedIndex := 4;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

function TFormEditFrame.CreateEdit(P: TWinControl): TEdit;
var
  i: integer;
begin
  Result := TEdit.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('Edit' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Edit' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.ReadOnly := True;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 3;
        SelectedIndex := 3;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

procedure TFormEditFrame.CheckBoxClicked(Sender: TObject);
begin
  (Sender as TCheckBox).Checked := False;
end;

constructor TFormEditFrame.Create(TheOwner: TComponent);
begin
  inherited;
  FEditorControls := TObjectList.Create(True);
  FormControlView.Items.Add(nil, 'Form1').Data := FormPanel;
  FormControlView.Items[0].ImageIndex := 0;
  FormControlView.Items[0].SelectedIndex := 0;
  FFormLeft := 0;
  FFormTop := 0;
  FFormName := 'Form1';
  FFormStyle := 0;
end;

destructor TFormEditFrame.Destroy;
begin
  FEditorControls.Free;
  inherited;
end;

procedure TFormEditFrame.FormPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if not (ssLeft in Shift) then
  begin
    FSelPoint := Point(-1, -1);
    if (Y >= (Sender as TControl).ClientHeight - 5) and
      (X >= (Sender as TControl).ClientWidth - 5) then
      (Sender as TControl).Cursor := crSizeNWSE
    else if (Y >= (Sender as TControl).ClientHeight - 5) then
      (Sender as TControl).Cursor := crSizeNS
    else if (X >= (Sender as TControl).ClientWidth - 5) then
      (Sender as TControl).Cursor := crSizeWE
    else
      (Sender as TControl).Cursor := crDefault;
  end
  else
  begin
    case (Sender as TControl).Cursor of
      crSizeNWSE:
      begin
        (Sender as TControl).Width := X;
        (Sender as TControl).Height := Y;
      end;
      crSizeNS: (Sender as TControl).Height := Y;
      crSizeWE: (Sender as TControl).Width := X;
      else
      begin
        if Sender <> FormPanel then
        begin
          (Sender as TControl).Left :=
            FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y))).X - FMousePoint.X;
          (Sender as TControl).Top :=
            FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y))).Y - FMousePoint.y;
        end;
        if ToolSelect.ItemIndex >= 0 then
        begin
          FSelPoint := FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y)));
          FormPanel.Invalidate;
        end;
      end;
    end;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFormEditFrame.FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  c: TControl;
  i: integer;
begin
  if mbLeft = Button then
    if (ToolSelect.ItemIndex >= 0) then
    begin
      case ToolSelect.ItemIndex of
        0: c := CreateButton(FormPanel);
        1: c := CreateCheckBox(FormPanel);
        2: c := CreateEdit(FormPanel);
        3: c := CreateLabel(FormPanel);
      end;

      if (FSelPoint.X - FMousePoint.x >= 0) and (FSelPoint.y - FMousePoint.Y >= 0) then
      begin
        c.Width := FSelPoint.X - FMousePoint.x;
        c.Height := FSelPoint.y - FMousePoint.Y;
        FSelPoint := Point(-1, -1);
        FormPanel.Invalidate;
      end;
      ToolSelect.ItemIndex := -1;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end
    else
      for i := 0 to FormControlView.Items.Count - 1 do
        if FormControlView.Items[i].Data = Pointer(Sender) then
          FormControlView.Select(FormControlView.Items[i]);
end;

procedure TFormEditFrame.FormPanelPaint(Sender: TObject);
begin
  if (FSelPoint.x >= 0) and (FSelPoint.y >= 0) then
  begin
    (Sender as TCustomControl).Canvas.Brush.Color := (Sender as TCustomControl).Color;
    (Sender as TCustomControl).Canvas.Clear;
    (Sender as TCustomControl).Canvas.Brush.Style := bsClear;
    (Sender as TCustomControl).Canvas.Pen.Style := psDash;
    (Sender as TCustomControl).Canvas.Pen.Color := clBlack;
    (Sender as TCustomControl).Canvas.Pen.Mode := pmNotXor;
    (Sender as TCustomControl).Canvas.Rectangle(FPanelMousePoint.X,
      FPanelMousePoint.Y, FSelPoint.x, FSelPoint.Y);
  end;
end;

procedure TFormEditFrame.PropEditorSetEditText(Sender: TObject;
  ACol, ARow: integer; const Value: string);

  function IsNumeric(s: string): boolean;
  var
    i: integer;
  begin
    Result := Length(s) > 0;
    for i := 1 to Length(s) do
      if not (s[i] in ['0'..'9']) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  c: TControl;
begin
  if not Assigned(FormControlView.Selected) then
    Exit;
  if ACol = 0 then
    exit;
  c := TControl(FormControlView.Selected.Data);
  if c = FormPanel then
  begin
    case ARow of
      0:
      begin
        FFormName := Value;
        FormControlView.Selected.Text := Value;
      end;
      1: FormCaptionLabel.Caption := Value;
      2:
      begin
        if IsNumeric(Value) then
          FFormLeft := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(FFormLeft);
      end;
      3:
      begin
        if IsNumeric(Value) then
          FFormTop := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(FFormTop);
      end;
      4:
      begin
        if IsNumeric(Value) then
          c.Width := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Width);
      end;
      5:
      begin
        if IsNumeric(Value) then
          c.Height := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Height);
      end;
      6:
      begin
        if IsNumeric(Value) then
          c.Tag := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Tag);
      end;
    end;
  end
  else
  begin
    case ARow of
      0:
      begin
        c.Name := Value;
        FormControlView.Selected.Text := Value;
      end;
      1: if c is TEdit then
          (c as TEdit).Text := Value
        else
          c.Caption := Value;
      2:
      begin
        if IsNumeric(Value) then
          c.Left := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Left);
      end;
      3:
      begin
        if IsNumeric(Value) then
          c.Top := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Top);
      end;
      4:
      begin
        if IsNumeric(Value) then
          c.Width := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Width);
      end;
      5:
      begin
        if IsNumeric(Value) then
          c.Height := StrToInt(Value)
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Height);
      end;
      6:
      begin
        if IsNumeric(Value) then
          c.Tag := StrToInt(Value) + (c.Tag div 255) * 255
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Tag mod 255);
      end;
      7:
      begin
        if IsNumeric(Value) then
          c.Tag := StrToInt(Value) * 255 + c.Tag mod 255
        else
          PropEditor.Cells[ACol, ARow] := IntToStr(c.Tag div 255);
      end;
    end;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormEditFrame.PropertyPanelResize(Sender: TObject);
begin
  PropertyPages.Height := (Sender as TControl).Height div 2;
end;

procedure TFormEditFrame.ToolboxHeaderPanelClick(Sender: TObject);
begin
  if ToolBoxPanel.Top > Parent.ClientHeight - ToolBoxPanel.Height then
    ToolBoxPanel.Top := Parent.ClientHeight - ToolBoxPanel.Height
  else
    ToolBoxPanel.Top := Parent.ClientHeight - ToolboxHeaderPanel.Height;
end;

procedure TFormEditFrame.ToolboxHeaderPanelMouseEnter(Sender: TObject);
begin
  ToolboxHeaderPanel.Color := clSilver;
end;

procedure TFormEditFrame.ToolboxHeaderPanelMouseLeave(Sender: TObject);
begin
  ToolboxHeaderPanel.Color := clBtnFace;
end;

procedure TFormEditFrame.FormPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FMousePoint := Point(X, Y);
    FPanelMousePoint := FormPanel.ScreenToClient(
      (Sender as TControl).ClientToScreen(Point(X, Y)));
  end;
end;

procedure TFormEditFrame.FormControlViewChange(Sender: TObject; Node: TTreeNode);
var
  c: TControl;
begin
  if not Assigned(Node) then
  begin
    if FormControlView.Items.Count > 0 then
      FormControlView.Select(FormControlView.Items[0]);
    exit;
  end;
  c := TControl(Node.Data);
  if c is TWinControl then
    (c as TWinControl).SetFocus;
  if c <> FormPanel then
  begin
    PropEditor.Values['Name'] := c.Name;
    if c is TEdit then
      PropEditor.Values['Text'] := (c as TEdit).Text
    else
      PropEditor.Values['Text'] := c.Caption;
    PropEditor.Values['X'] := IntToStr(c.Left);
    PropEditor.Values['Y'] := IntToStr(c.Top);
    PropEditor.Values['Width'] := IntToStr(c.Width);
    PropEditor.Values['Height'] := IntToStr(c.Height);
    PropEditor.Values['Style'] := IntToStr(c.Tag mod 255);
    PropEditor.Values['StyleEx'] := IntToStr((c.Tag div 255) mod 255);
  end
  else
  begin
    PropEditor.Values['Name'] := FFormName;
    PropEditor.Values['Text'] := FormCaptionLabel.Caption;
    PropEditor.Values['X'] := IntToStr(FFormLeft);
    PropEditor.Values['Y'] := IntToStr(FFormTop);
    PropEditor.Values['Width'] := IntToStr(c.Width);
    PropEditor.Values['Height'] := IntToStr(c.Height);
    PropEditor.Values['Style'] := IntToStr(c.Tag mod 255);
    PropEditor.Values['StyleEx'] := IntToStr((c.Tag div 255) mod 255);
  end;
end;

procedure TFormEditFrame.FormControlViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  TControl(Node.Data).Name := s;
  FormControlViewChange(Sender, Node);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormEditFrame.DeleteItem(n: TTreeNode);
var
  i: integer;
begin
  while n.HasChildren do
    DeleteItem(n.GetFirstChild);
  for i := 0 to FEditorControls.Count - 1 do
    if n.Data = Pointer(FEditorControls.Items[i]) then
    begin
      FEditorControls.Delete(i);
      FormControlView.Items.Delete(n);
      Break;
    end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormEditFrame.FormControlViewKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if (Key = 46) and Assigned(FormControlView.Selected) then
    Application.QueueAsyncCall(TDataEvent(@DeleteItem),
      IntPtr(FormControlView.Selected));
end;

procedure TFormEditFrame.Save(p: string = '');
var
  sl: TStringList;
  i: integer;
begin
  if p = '' then
    p := FFileName;
  sl := TStringList.Create;
  try
    sl.Add(Format('$%s = CreateWindow("%s", %d, %d, %d, %d, %d)',
      [FFormName, FormCaptionLabel.Caption, FFormLeft, FFormTop,
      FormPanel.Width, FormPanel.Height, FormPanel.Tag]));
    for i := 0 to FEditorControls.Count - 1 do
    begin
      if FEditorControls[i] is TButton then
        with FEditorControls[i] as TButton do
          sl.Add(Format('$%s = CreateButton($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            Tag mod 255, (Tag div 255) mod 255]))
      else if FEditorControls[i] is TEdit then
        with FEditorControls[i] as TEdit do
          sl.Add(Format('$%s = CreateInputbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            Tag mod 255, (Tag div 255) mod 255]))
      else if FEditorControls[i] is TCheckBox then
        with FEditorControls[i] as TCheckBox do
          sl.Add(Format('$%s = CreateCheckbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            Tag mod 255, (Tag div 255) mod 255]))
      else if FEditorControls[i] is TLabel then
        with FEditorControls[i] as TLabel do
          sl.Add(Format('$%s = CreateLabel($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            Tag mod 255, (Tag div 255) mod 255]));
    end;
    if p <> '' then
      sl.SaveToFile(p);
  finally
    sl.Free;
  end;
  FFileName := p;
end;

procedure TFormEditFrame.Load(p: string = '');

  function IsNumeric(s: string): boolean;
  var
    i: integer;
  begin
    Result := Length(s) > 0;
    for i := 1 to Length(s) do
      if not (s[i] in ['0'..'9']) then
      begin
        Result := False;
        Break;
      end;
  end;

  function ReadTok(s: string): string;
  var
    i: integer;
  begin
    i := 0;
    while (i <= Length(s)) and (s[i + 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(i);
    Result := Copy(s, 1, i);
  end;

var
  sl: TStringList;
  s, varName, tmp: string;
  i, len: integer;
  frm: boolean;
  c: TControl;
begin
  // TODO Rework this mess
  frm := False;
  if p = '' then
    p := FFileName;
  if not FileExists(p) then
    Exit;
  DeleteItem(FormControlView.Items[0]);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(p);
    for i := 0 to sl.Count - 1 do
    begin
      s := Trim(sl[i]);
      if Length(s) = 0 then
        Continue
      else if isEnd(s, 'setonevent') then
      begin
        //TODO
        Continue;
      end
      else if s[1] <> '$' then
        Continue;
      if pos('=', s) = 0 then
        Continue;
      len := 0;
      while (s[2 + len] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        Inc(len);
      if len = 0 then
        Continue;
      varName := Copy(s, 2, len);
      Delete(s, 1, Pos('=', s));
      s := Trim(s);
      if isEnd(s, 'CreateWindow') and not frm then
      begin
        FFormName := varName;
        if Pos('"', s) = 0 then
          Continue;
        Delete(s, 1, Pos('"', s));
        len := 0;
        while (s[1 + len] <> '"') and (len < Length(s)) do
          Inc(len);
        FormCaptionLabel.Caption := Copy(s, 1, len);
        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        FFormLeft := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        FFormTop := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        FormPanel.Width := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        FormPanel.Height := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        FormPanel.Tag := StrToInt(tmp);
        frm := True;
      end
      else
      if isEnd(s, 'CreateButton') then
      begin
        if Pos('$', s) = 0 then
          Continue;
        Delete(s, 1, Pos('$', s));
        if not (ReadTok(s) = FFormName) then
          Continue;
        c := CreateButton(FormPanel);
        if Pos('"', s) = 0 then
          Continue;
        Delete(s, 1, Pos('"', s));
        len := 0;
        while (s[1 + len] <> '"') and (len < Length(s)) do
          Inc(len);
        c.Caption := Copy(s, 1, len);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Left := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Top := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Width := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Height := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := c.Tag + StrToInt(tmp) * 255;
      end
      else
      if isEnd(s, 'CreateLabel') then
      begin
        if Pos('$', s) = 0 then
          Continue;
        Delete(s, 1, Pos('$', s));
        if not (ReadTok(s) = FFormName) then
          Continue;
        c := CreateLabel(FormPanel);
        if Pos('"', s) = 0 then
          Continue;
        Delete(s, 1, Pos('"', s));
        len := 0;
        while (s[1 + len] <> '"') and (len < Length(s)) do
          Inc(len);
        c.Caption := Copy(s, 1, len);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Left := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Top := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Width := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Height := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := c.Tag + StrToInt(tmp) * 255;
      end
      else
      if isEnd(s, 'CreateCheckbox') then
      begin
        if Pos('$', s) = 0 then
          Continue;
        Delete(s, 1, Pos('$', s));
        if not (ReadTok(s) = FFormName) then
          Continue;
        c := CreateCheckbox(FormPanel);
        if Pos('"', s) = 0 then
          Continue;
        Delete(s, 1, Pos('"', s));
        len := 0;
        while (s[1 + len] <> '"') and (len < Length(s)) do
          Inc(len);
        c.Caption := Copy(s, 1, len);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Left := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Top := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Width := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Height := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := c.Tag + StrToInt(tmp) * 255;
      end
      else
      if isEnd(s, 'CreateInputbox') then
      begin
        if Pos('$', s) = 0 then
          Continue;
        Delete(s, 1, Pos('$', s));
        if not (ReadTok(s) = FFormName) then
          Continue;
        c := CreateEdit(FormPanel);
        if Pos('"', s) = 0 then
          Continue;
        Delete(s, 1, Pos('"', s));
        len := 0;
        while (s[1 + len] <> '"') and (len < Length(s)) do
          Inc(len);
        (c as TEdit).Text := Copy(s, 1, len);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Left := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Top := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Width := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Height := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := StrToInt(tmp);

        Delete(s, 1, pos(',', s));
        tmp := ReadTok(Trim(s));
        if not IsNumeric(tmp) then
          Continue;
        c.Tag := c.Tag + StrToInt(tmp) * 255;
      end;
    end;
  finally
    sl.Free;
  end;
  FFileName := p;
end;

end.
