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
  // TODO: Save Style Information
  if p = '' then
    p := FFileName;
  sl := TStringList.Create;
  try
    sl.Add(Format('$%s = CreateWindow("%s", %d, %d, %d, %d, %d)',
      [FFormName, FormCaptionLabel.Caption, FFormLeft, FFormTop,
      FormPanel.Width, FormPanel.Height, 0]));
    for i := 0 to FEditorControls.Count - 1 do
    begin
      if FEditorControls[i] is TButton then
        with FEditorControls[i] as TButton do
          sl.Add(Format('$%s = CreateButton($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            0, 0]))
      else if FEditorControls[i] is TEdit then
        with FEditorControls[i] as TEdit do
          sl.Add(Format('$%s = CreateInputbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            0, 0]))
      else if FEditorControls[i] is TCheckBox then
        with FEditorControls[i] as TCheckBox do
          sl.Add(Format('$%s = CreateCheckbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            0, 0]))
      else if FEditorControls[i] is TLabel then
        with FEditorControls[i] as TLabel do
          sl.Add(Format('$%s = CreateLabel($%s, "%s", %d, %d, %d, %d, %d, %d)',
            [Name, FFormName, Caption, Left, Top, Width, Height,
            0, 0]));
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

  function ReadFunc(s: string; Params: TStringList): string;

    function ReadTok(s: string; out NewPos: integer): string;
    var
      len, depth: integer;
    begin
      len := 0;
      if s[1] = '"' then
      begin
        while (len + 2 < length(s)) and (s[2 + len] <> '"') do
          Inc(len);
        Result := Copy(s, 2, len);
        NewPos := len + 1;
        while not (s[NewPos] in [',', ')']) do
          Inc(NewPos);
      end
      else
      begin
        depth := 0;
        len := 0;
        while (len < length(s) - 1) and not ((depth = 0) and
            (s[1 + len] in [',', ')'])) do
        begin
          if (s[1 + len] = '(') then
            Inc(depth)
          else if (s[1 + len] = ')') then
            Dec(depth);
          Inc(len);
        end;
        Result := Copy(s, 1, len);
        NewPos := len + 1;
        while not (s[NewPos] in [',', ')']) do
          Inc(NewPos);
      end;
    end;

  var
    i: integer;
  begin
    Result := '';
    if (Pos('(', s) = 0) or (Pos(')', s) = 0) or (Params = nil) then
      exit;
    Params.Clear;
    s := Trim(s);
    i := 1;
    while (i <= length(s)) and (s[i] <> '(') do
      Inc(i);
    Result := Copy(s, 1, i - 1);
    Delete(s, 1, Pos('(', s));

    while not ((ReadTok(s, i) = ')') or (ReadTok(s, i) = '')) do
    begin
      Params.Add(ReadTok(s, i));
      while s[i] in [' ', #9, ','] do
        Inc(i);
      Delete(s, 1, i - 1);
    end;
  end;

  function ReadVar(s: string): string;
  var
    i: integer;
  begin
    i := 2;
    Result := '';
    if (Length(s) = 0) or (s[1] <> '$') then
      exit;
    while (i <= Length(s)) and (s[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(i);
    if s[i] = '[' then
      exit;
    Result := Copy(s, 2, i - 2);
  end;

var
  Lines: TStringList;
  VarName, FuncName: string;
  FuncParams: TStringList;
  i, curr: integer;
  FormFound: boolean;
  c: TControl;
begin
  curr := 1;
  DeleteItem(FormControlView.Items[0]);
  FormFound := False;
  if p = '' then
    p := FFileName;
  if not FileExists(p) then
    exit;
  Lines := TStringList.Create;
  FuncParams := TStringList.Create;
  try
    Lines.LoadFromFile(p);
    for i := 0 to Lines.Count - 1 do
    begin
      Lines[i] := Trim(Lines[i]);
      if isEnd(Lines[i], 'SetOnEvent') then
      begin
        // TODO
      end
      else
      begin
        VarName := ReadVar(Lines[i]);
        if VarName = '' then
          Continue;
        FuncName := Lines[i];
        Delete(FuncName, 1, Pos('=', FuncName));
        FuncName := LowerCase(ReadFunc(FuncName, FuncParams));
        if (FuncName = '') or (FuncParams.Count = 0) then
          Continue;
        if FuncName = 'createwindow' then
        begin
          // Syntax Check
          if FormFound or (FuncParams.Count <> 6) or not
            (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
            IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
            IsNumeric(FuncParams[5])) then
            Continue;
          // Read Data
          FFormName := VarName;
          FormControlView.Items[0].Text := FFormName;
          FormCaptionLabel.Caption := FuncParams[0];
          FFormLeft := StrToInt(FuncParams[1]);
          FFormTop := StrToInt(FuncParams[2]);
          FormPanel.Width := StrToInt(FuncParams[3]);
          FormPanel.Height := StrToInt(FuncParams[4]);
          //TODO Load Style Information
          FormFound := True;
        end
        else if FuncName = 'createbutton' then
        begin
          // Syntax Check
          if (FuncParams.Count <> 8) or not
            ((LowerCase(FuncParams[0]) = '$' + LowerCase(FFormName)) and
            IsNumeric(FuncParams[2]) and IsNumeric(FuncParams[3]) and
            IsNumeric(FuncParams[4]) and IsNumeric(FuncParams[5]) and
            IsNumeric(FuncParams[6]) and IsNumeric(FuncParams[7])) then
            Continue;
          // Read Data
          c := CreateButton(FormPanel);
          c.Name := VarName;
          FormControlView.Items[curr].Text := VarName;
          c.Caption := FuncParams[1];
          c.Left := StrToInt(FuncParams[2]);
          c.Top := StrToInt(FuncParams[3]);
          c.Width := StrToInt(FuncParams[4]);
          c.Height := StrToInt(FuncParams[5]);
          // TODO: Loading Style Information
          Inc(curr);
        end
        else if FuncName = 'createcheckbox' then
        begin
          // Syntax Check
          if (FuncParams.Count <> 8) or not
            ((LowerCase(FuncParams[0]) = '$' + LowerCase(FFormName)) and
            IsNumeric(FuncParams[2]) and IsNumeric(FuncParams[3]) and
            IsNumeric(FuncParams[4]) and IsNumeric(FuncParams[5]) and
            IsNumeric(FuncParams[6]) and IsNumeric(FuncParams[7])) then
            Continue;
          // Read Data
          c := CreateCheckBox(FormPanel);
          c.Name := VarName;
          FormControlView.Items[curr].Text := VarName;
          c.Caption := FuncParams[1];
          c.Left := StrToInt(FuncParams[2]);
          c.Top := StrToInt(FuncParams[3]);
          c.Width := StrToInt(FuncParams[4]);
          c.Height := StrToInt(FuncParams[5]);
          // TODO: Loading Style Information
          Inc(curr);
        end
        else if FuncName = 'createlabel' then
        begin
          // Syntax Check
          if (FuncParams.Count <> 8) or not
            ((LowerCase(FuncParams[0]) = '$' + LowerCase(FFormName)) and
            IsNumeric(FuncParams[2]) and IsNumeric(FuncParams[3]) and
            IsNumeric(FuncParams[4]) and IsNumeric(FuncParams[5]) and
            IsNumeric(FuncParams[6]) and IsNumeric(FuncParams[7])) then
            Continue;
          // Read Data
          c := CreateLabel(FormPanel);
          c.Name := VarName;
          FormControlView.Items[curr].Text := VarName;
          c.Caption := FuncParams[1];
          c.Left := StrToInt(FuncParams[2]);
          c.Top := StrToInt(FuncParams[3]);
          c.Width := StrToInt(FuncParams[4]);
          c.Height := StrToInt(FuncParams[5]);
          // TODO: Loading Style Information
          Inc(curr);
        end
        else if FuncName = 'createinputbox' then
        begin
          // Syntax Check
          if (FuncParams.Count <> 8) or not
            ((LowerCase(FuncParams[0]) = '$' + LowerCase(FFormName)) and
            IsNumeric(FuncParams[2]) and IsNumeric(FuncParams[3]) and
            IsNumeric(FuncParams[4]) and IsNumeric(FuncParams[5]) and
            IsNumeric(FuncParams[6]) and IsNumeric(FuncParams[7])) then
            Continue;
          // Read Data
          c := CreateEdit(FormPanel);
          c.Name := VarName;
          FormControlView.Items[curr].Text := VarName;
          (c as TEdit).Text := FuncParams[1];
          c.Left := StrToInt(FuncParams[2]);
          c.Top := StrToInt(FuncParams[3]);
          c.Width := StrToInt(FuncParams[4]);
          c.Height := StrToInt(FuncParams[5]);
          // TODO: Loading Style Information
          Inc(curr);
        end;
      end;
    end;
  finally
    FuncParams.Free;
    Lines.Free;
  end;
  Self.Parent.Caption:=ExtractFileName(p);
  FFileName := p;
end;

end.
