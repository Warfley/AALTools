unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  ExtCtrls, StdCtrls, ValEdit, ComCtrls, Grids, contnrs, AALTypes, Dialogs,
  FormEditComponents, LCLIntf, Math, GraphUtil;

type

  { TFormEditFrame }

  TFormEditFrame = class(TFrame)
    FormCaptionLabel: TLabel;
    FormPanel: TPanel;
    ImageList1: TImageList;
    EventEditor: TValueListEditor;
    PositionPickerPanel: TPanel;
    PositionPicker: TPaintBox;
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
    procedure EditorScrollBoxPaint(Sender: TObject);
    procedure EventEditorEditingDone(Sender: TObject);
    procedure EventEditorGetPickList(Sender: TObject; const KeyName: string;
      Values: TStrings);
    procedure EventEditorPickListSelect(Sender: TObject);
    procedure FormControlViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormControlViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure FormControlViewKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormPanelDblClick(Sender: TObject);
    procedure FormPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelPaint(Sender: TObject);
    procedure PositionPickerMouseEnter(Sender: TObject);
    procedure PositionPickerMouseLeave(Sender: TObject);
    procedure PositionPickerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PositionPickerPaint(Sender: TObject);
    procedure PropEditorEditingDone(Sender: TObject);
    procedure PropertyPanelResize(Sender: TObject);
    procedure ToolboxHeaderPanelClick(Sender: TObject);
    procedure ToolboxHeaderPanelMouseEnter(Sender: TObject);
    procedure ToolboxHeaderPanelMouseLeave(Sender: TObject);
    procedure PickListClick(Sender: TObject);
  private
    FFileName: string;
    FFormName: string;
    FConf: TFormEditorConfig;
    Moved: boolean;
    FLastClickTime: cardinal;
    FFormEvents: TStringList;
    FFormStyle: integer;
    FDrawLines: boolean;
    FFormLeft: integer;
    FFormTop: integer;
    FEditorControls: TObjectList;
    FMousePoint: TPoint;
    FPanelMousePoint: TPoint;
    FSelPoint: TPoint;
    FOnChange: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnVarChanged: TNotifyEvent;
    FFuncList: TStringList;
    { private declarations }
    procedure DeleteItem(n: TTreeNode);
    function FindControl(s: string): integer;
    function CreateButton(P: TWinControl): TAALButton;
    function CreateCheckBox(P: TWinControl): TAALCheckBox;
    function CreateLabel(P: TWinControl): TAALLabel;
    function CreateEdit(P: TWinControl): TAALEdit;
    procedure LoadControlData(c: TComponent);
  public
    procedure ReLoadConf;
    constructor Create(TheOwner: TComponent); override;
    procedure AddToVarlist(l: TVarList);
    destructor Destroy; override;
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    { public declarations }
    property FuncList: TStringList read FFuncList;
    property FileName: string read FFileName write FFileName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property EnterFunc: TOpenFunctionEvent read FEnterFunc write FEnterFunc;
    property OnVarChanged: TNotifyEvent read FOnVarChanged write FOnVarChanged;
  end;

implementation

{$R *.lfm}


procedure TFormEditFrame.ReLoadConf;
var
  f: file of TFormEditorConfig;
begin
  AssignFile(f, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'foms.cfg');
  try
    Reset(f);
    Read(f, FConf);
  finally
    CloseFile(f);
  end;
  with FConf do
  begin
    if OIRight then
    begin
      PropertyPanel.Align := alRight;
      PositionPickerPanel.Left :=
        ClientWidth - PropertyPanel.Width - 8 - PositionPickerPanel.Width;
      ToolBoxPanel.Left := 8;
    end
    else
    begin
      PropertyPanel.Align := alLeft;
      PositionPickerPanel.Left := ClientWidth - 8 - PositionPickerPanel.Width;
      ToolBoxPanel.Left := PropertyPanel.Width + 8;
    end;
    Color := BGCol;
    FormControlView.BackgroundColor := BGCol;
    EditorScrollBox.Color := BGCol;
    PropertyPages.Color := BGCol;
    PropertyPanel.Color := BGCol;
    PropEditor.Color := BGCol;
    EventEditor.Color := BGCol;
    TreeFilterEdit1.Color := BGCol;
    TreeFilterEdit1.Font.Color := ForeCol;
    Font.Color := ForeCol;
    ToolboxHeaderPanel.Color := TBCol;
    ToolboxHeaderPanel.Font.Color := ForeCol;
    FormControlView.Color := ForeCol;
    FormControlView.ExpandSignColor := GetHighLightColor(ForeCol);
    FormControlView.TreeLineColor := GetHighLightColor(ForeCol);
    FormControlView.SeparatorColor := GetHighLightColor(ForeCol);
    Invalidate;
  end;
end;

procedure TFormEditFrame.LoadControlData(c: TComponent);
var
  i: integer;
begin
  if c = FormPanel then
  begin
    with PropEditor do
    begin
      Clear;
      Values['Name'] := FFormName;
      Values['Text'] := FormCaptionLabel.Caption;
      Values['X'] := IntToStr(FFormLeft);
      Values['Y'] := IntToStr(FFormTop);
      Values['Width'] := IntToStr(FormPanel.Width);
      Values['Height'] := IntToStr(FormPanel.Height);
      Values['Style'] := IntToStr(FFormStyle);
    end;
    EventEditor.Clear;
    for i := 0 to FFormEvents.Count - 1 do
    begin
      EventEditor.Values[FFormEvents.Names[i]] := FFormEvents.ValueFromIndex[i];
      EventEditor.ItemProps[FFormEvents.Names[i]].EditStyle := esPickList;
    end;
  end
  else
  begin
    if c is TAALButton then
    begin
      (c as TAALButton).FillEvents(EventEditor);
      (c as TAALButton).FillProps(PropEditor);
    end
    else if c is TAALLabel then
    begin
      (c as TAALLabel).FillEvents(EventEditor);
      (c as TAALLabel).FillProps(PropEditor);
    end
    else if c is TAALCheckbox then
    begin
      (c as TAALCheckbox).FillEvents(EventEditor);
      (c as TAALCheckbox).FillProps(PropEditor);
    end
    else if c is TAALEdit then
    begin
      (c as TAALEdit).FillEvents(EventEditor);
      (c as TAALEdit).FillProps(PropEditor);
    end;
  end;
end;

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

function TFormEditFrame.CreateButton(P: TWinControl): TAALButton;
var
  i: integer;
begin
  Result := TAALButton.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('Button' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Button' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.OnDblClick := @FormPanelDblClick;
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
        FormControlView.MultiSelect := False;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

function TFormEditFrame.CreateCheckBox(P: TWinControl): TAALCheckbox;
var
  i: integer;
begin
  Result := TAALCheckbox.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('CheckBox' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'CheckBox' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnDblClick := @FormPanelDblClick;
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
        FormControlView.MultiSelect := False;
        Selected := True;
        Break;
      end;
  FEditorControls.Add(Result);
end;

procedure TFormEditFrame.PickListClick(Sender: TObject);
var
  c: cardinal;
begin
  c := GetTickCount;
  if (c - FLastClickTime < 700) and (EventEditor.ScreenToClient(Mouse.CursorPos).x <
    EventEditor.Width - 20) then
  begin
    if EventEditor.Rows[EventEditor.Row][1] = '' then
      EventEditor.Rows[EventEditor.Row][1] := '(Neu...)';
    EventEditorPickListSelect(EventEditor);
  end;
  FLastClickTime := c;
end;

function TFormEditFrame.CreateLabel(P: TWinControl): TAALLabel;
var
  i: integer;
begin
  Result := TAALLabel.Create(FormPanel);
  Result.Parent := P;
  i := 1;
  while FindControl('Label' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Label' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnDblClick := @FormPanelDblClick;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.Caption := Result.Name;
  Result.Width := Result.Canvas.TextWidth(Result.Caption);
  Result.Height := Result.Canvas.TextHeight(Result.Caption);
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
        FormControlView.MultiSelect := False;
        Selected := False;
        Break;
      end;
  FEditorControls.Add(Result);
end;

function TFormEditFrame.CreateEdit(P: TWinControl): TAALEdit;
var
  i: integer;
begin
  Result := TAALEdit.Create(FormPanel);
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
  Result.OnDblClick := @FormPanelDblClick;
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

constructor TFormEditFrame.Create(TheOwner: TComponent);
begin
  inherited;
  FormPanel.DoubleBuffered := True;
  FFuncList := TStringList.Create;
  FDrawLines := False;
  EventEditor.EditorByStyle(cbsPickList).OnClick := @PickListClick;
  EventEditor.EditorByStyle(cbsPickList).OnEnter := @PickListClick;
  FEditorControls := TObjectList.Create(True);
  FMousePoint := Point(-1, -1);
  FormControlView.Items.Add(nil, 'Form1').Data := FormPanel;
  FormControlView.Items[0].ImageIndex := 0;
  FormControlView.Items[0].SelectedIndex := 0;
  FFormLeft := 0;
  FFormTop := 0;
  FFormEvents := TStringList.Create;
  FFormEvents.Values['onClick'] := '';
  FFormName := 'Form1';
  PropEditor.ColWidths[0] := PropEditor.Width div 2;
  EventEditor.ColWidths[0] := EventEditor.Width div 2;
  FFormStyle := 0;
  ReLoadConf;
end;

destructor TFormEditFrame.Destroy;
begin
  DeleteItem(FormControlView.Items[0]);
  FEditorControls.Free;
  FFuncList.Free;
  FFormEvents.Free;
  inherited;
end;

procedure TFormEditFrame.FormPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  n: integer;
  b: boolean;
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
        (Sender as TControl).Width :=
          X div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
        (Sender as TControl).Height :=
          Y div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
        Moved := True;
      end;
      crSizeNS:
      begin
        (Sender as TControl).Height :=
          Y div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
        Moved := True;
      end;
      crSizeWE:
      begin
        (Sender as TControl).Width :=
          X div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
        Moved := True;
      end;
      else
      begin
        if Sender <> FormPanel then
        begin
          (Sender as TControl).Left :=
            (FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y))).X - FMousePoint.X) div
            (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          (Sender as TControl).Top :=
            (FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y))).Y - FMousePoint.y) div
            (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          Moved := True;
          b := False;
          if FConf.UseHelpLines then
            with Sender as TControl do
              for n := 0 to Parent.ControlCount - 1 do
              begin
                if (Left < Parent.Controls[n].Left + FConf.RasterSize) and
                  (Left > Parent.Controls[n].Left - FConf.RasterSize) then
                begin
                  Left := Parent.Controls[n].Left;
                  b := True;
                end;
                if (Top < Parent.Controls[n].Top + FConf.RasterSize) and
                  (Top > Parent.Controls[n].Top - FConf.RasterSize) then
                begin
                  Top := Parent.Controls[n].Top;
                  b := True;
                end;
                if b then
                begin
                  FDrawLines := True;
                  FormPanel.Invalidate;
                  Exit;
                end;
              end;
        end;
        if ToolSelect.ItemIndex >= 0 then
        begin
          FSelPoint := FormPanel.ScreenToClient(
            (Sender as TControl).ClientToScreen(Point(X, Y)));
          FormPanel.Invalidate;
          Moved := True;
        end;
      end;
    end;
  end;
end;

procedure TFormEditFrame.FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  c: TControl;
  i: integer;
begin
  if mbLeft = Button then
  begin
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
      if Assigned(FOnVarChanged) then
        FOnVarChanged(Self);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end
    else
      for i := 0 to FormControlView.Items.Count - 1 do
        if FormControlView.Items[i].Data = Pointer(Sender) then
          FormControlView.Select(FormControlView.Items[i]);
    if Moved and Assigned(FOnChange) then
      FOnChange(Self);
    Moved := False;
    PositionPickerPanel.Show;
    FMousePoint := Point(-1, -1);
    FDrawLines := False;
  end;
end;

procedure TFormEditFrame.AddToVarlist(l: TVarList);
var
  i: integer;
begin
  l.Clear;
  for i := 0 to FormControlView.Items.Count - 1 do
  begin
    l.Add(VarInfo('$' + FormControlView.Items[i].Text, 0, i, FFileName));
  end;
end;

procedure TFormEditFrame.FormPanelPaint(Sender: TObject);
var
  i, n: integer;
  c: TControl;
begin
  if FConf.UseRaster then
    for i := 0 to (Sender as TCustomControl).Width div FConf.RasterSize do
      for n := 0 to (Sender as TCustomControl).Height div FConf.RasterSize do
        (Sender as TCustomControl).Canvas.Pixels[i * FConf.RasterSize,
          n * FConf.RasterSize] :=
          clgray;
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
  if ((FMousePoint.x = -1) and (FMousePoint.y = -1)) or FDrawLines then
    for i := 0 to FormControlView.Items.Count - 1 do
      if FormControlView.Items[i].Selected then
      begin
        c := (Sender as TCustomControl).FindChildControl(
          TControl(FormControlView.Items[i].Data).Name);
        if Assigned(c) then
          with (Sender as TCustomControl).Canvas do
          begin
            if not FDrawLines then
            begin
              Pen.Style := psDash;
              Pen.Mode := pmCopy;
              Brush.Style := bsClear;
              Pen.Color := clBlack;
              Rectangle(c.Left - 1, c.Top - 1, c.Left + c.Width + 1, c.Top + c.Height + 1);
              Pen.Color := clHighlight;
              Pen.Style := psSolid;
            end;
            if FConf.UseHelpLines then
              for n := 0 to (Sender as TCustomControl).ControlCount - 1 do
              begin
                if (Sender as TCustomControl).Controls[n].Left = c.Left then
                  Line(c.Left, c.Top, c.Left,
                    (Sender as TCustomControl).Controls[n].Top);
                if (Sender as TCustomControl).Controls[n].top = c.Top then
                  Line(c.Left, c.Top,
                    (Sender as TCustomControl).Controls[n].Left, c.Top);
                if (Sender as TCustomControl).Controls[n].top +
                (Sender as TCustomControl).Controls[n].Height = c.Top + c.Height then
                  Line(c.Left, c.Top + c.Height,
                    (Sender as TCustomControl).Controls[n].Left,
                    c.Top + c.Height);
                if (Sender as TCustomControl).Controls[n].Left +
                (Sender as TCustomControl).Controls[n].Width = c.Left + c.Width then
                  Line(c.Left + c.Width, c.Top, c.Left + c.Width,
                    (Sender as TCustomControl).Controls[n].Top);
              end;

          end;
      end;
end;

procedure TFormEditFrame.PositionPickerMouseEnter(Sender: TObject);
begin
  PositionPickerPanel.Left := PositionPickerPanel.Left - 80;
  PositionPickerPanel.Width := PositionPickerPanel.Width + 80;
  PositionPickerPanel.Top := PositionPickerPanel.Top - 45;
  PositionPickerPanel.Height := PositionPickerPanel.Height + 45;
end;

procedure TFormEditFrame.PositionPickerMouseLeave(Sender: TObject);
begin
  PositionPickerPanel.Left := PositionPickerPanel.Left + 80;
  PositionPickerPanel.Width := PositionPickerPanel.Width - 80;
  PositionPickerPanel.Top := PositionPickerPanel.Top + 45;
  PositionPickerPanel.Height := PositionPickerPanel.Height - 45;
end;

procedure TFormEditFrame.PositionPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  fh, fw: double;
begin
  if ssLeft in Shift then
  begin
    fw := Screen.Width / PositionPicker.Width;
    fh := Screen.Height / PositionPicker.Height;
    FFormLeft := Min(max(0, trunc(X * fw) - FormPanel.Width div 2),
      Screen.Width - FormPanel.Width);
    FFormTop := Min(max(0, trunc(Y * fH) - FormPanel.Height div 2),
      Screen.Height - FormPanel.Height);
    FormControlView.Select(FormControlView.Items[0]);
    PositionPicker.Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFormEditFrame.PositionPickerPaint(Sender: TObject);
var
  fh, fw: double;
  px, py: integer;
  px2, py2: integer;
begin
  if not Assigned(FormPanel) then
    exit;
  with PositionPicker.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FConf.TBCol;
    Pen.Style := psClear;
    Rectangle(0, 0, PositionPicker.Width, PositionPicker.Height);
    fw := PositionPicker.Width / Screen.Width;
    fh := PositionPicker.Height / Screen.Height;
    px := trunc(FFormLeft * fw);
    py := trunc(FFormTop * fh);
    px2 := px + trunc(FormPanel.Width * fw);
    py2 := py + trunc(FormPanel.Height * fh);
    Brush.Color := clWindow;
    pen.Style := psSolid;
    pen.Color := clBlack;
    Rectangle(px, py, px2, py2);
  end;
end;

procedure TFormEditFrame.PropEditorEditingDone(Sender: TObject);

  function isNumeric(s: string): boolean;
  var
    c: char;
  begin
    Result := Length(s) > 0;
    for c in s do
      if not (c in ['0'..'9']) then
      begin
        Result := False;
        Break;
      end;
  end;

  function isValidName(s: string): boolean;
  var
    c: char;
  begin
    Result := Length(s) > 0;
    for c in s do
      if not (c in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  o: TObject;
  s, v: string;
begin
  if not Assigned(FormControlView.Selected) then
    Exit;
  o := TObject(FormControlView.Selected.Data);
  s := LowerCase(PropEditor.Rows[PropEditor.Row][0]);
  v := PropEditor.Rows[PropEditor.Row][1];
  if o = FormPanel then
  begin
    if (s = 'name') and isValidName(v) then
      FFormName := v
    else if s = 'text' then
      FormCaptionLabel.Caption := v
    else if (s = 'x') and isNumeric(v) then
      FFormLeft := StrToInt(v)
    else if (s = 'y') and isNumeric(v) then
      FFormTop := StrToInt(v)
    else if (s = 'width') and isNumeric(v) then
      FormPanel.Width := StrToInt(v)
    else if (s = 'height') and isNumeric(v) then
      FormPanel.Height := StrToInt(v)
    else if (s = 'style') and isNumeric(v) then
      FFormStyle := StrToInt(v);
    PositionPicker.Invalidate;
  end
  else if o is TAALButton then
    (o as TAALButton).ControlProp[s] := v
  else if o is TAALLabel then
    (o as TAALLabel).ControlProp[s] := v
  else if o is TAALCheckbox then
    (o as TAALCheckbox).ControlProp[s] := v
  else if o is TAALEdit then
    (o as TAALEdit).ControlProp[s] := v;
  FormPanel.Invalidate;
  if s = 'name' then
  begin
    FormControlView.Selected.Text := v;
    if Assigned(FOnVarChanged) then
      FOnVarChanged(Self);
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
  ToolboxHeaderPanel.Color := clWhite;
end;

procedure TFormEditFrame.ToolboxHeaderPanelMouseLeave(Sender: TObject);
begin
  ToolboxHeaderPanel.Color := FConf.TBCol;
end;

procedure TFormEditFrame.FormPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FMousePoint := Point(X, Y);
    FPanelMousePoint := FormPanel.ScreenToClient(
      (Sender as TControl).ClientToScreen(Point(X, Y)));
    if FormPanel.Cursor = crSizeNWSE then
      PositionPickerPanel.Hide;
    EditorScrollBox.Invalidate;
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
  LoadControlData(c);
  EditorScrollBox.Invalidate;
end;

procedure TFormEditFrame.EventEditorEditingDone(Sender: TObject);
var
  c: TControl;
  s, v: string;
  i: integer;
begin
  if not Assigned(FormControlView.Selected) then
    exit;
  s := EventEditor.Rows[EventEditor.Row][0];
  v := EventEditor.Values[s];
  c := TControl(FormControlView.Selected.Data);
  if c = FormPanel then
    FFormEvents.Values[s] := v
  else if c is TAALButton then
    (c as TAALButton).Event[s] := v
  else if c is TAALLabel then
    (c as TAALLabel).Event[s] := v
  else if c is TAALEdit then
    (c as TAALEdit).Event[s] := v
  else if c is TAALCheckbox then
    (c as TAALCheckbox).Event[s] := v;
  if Assigned(FOnChange) then
    FOnChange(Self);
  LoadControlData(c);
end;

procedure TFormEditFrame.EditorScrollBoxPaint(Sender: TObject);
var
  i: integer;
begin
  EditorScrollBox.Canvas.Brush.Color := (EditorScrollBox.Color);
  EditorScrollBox.Canvas.Brush.Style := bsSolid;
  EditorScrollBox.Canvas.Pen.Style := psClear;
  EditorScrollBox.Canvas.Rectangle(0, 0, EditorScrollBox.ClientWidth,
    EditorScrollBox.ClientHeight);
  if (FMousePoint.x = -1) and (FMousePoint.y = -1) then
    for i := 0 to FormControlView.Items.Count - 1 do
      if (FormControlView.Items[i].Selected) and
        (TControl(FormControlView.Items[i].Data) = FormPanel) then
      begin
        EditorScrollBox.Canvas.Brush.Style := bsClear;
        EditorScrollBox.Canvas.Pen.Style := psDash;
        EditorScrollBox.Canvas.Pen.Mode := pmNotXor;
        EditorScrollBox.Canvas.Rectangle(FormPanel.Left - 1, FormPanel.Top - 1,
          FormPanel.Left + FormPanel.Width + 1, FormPanel.Top + FormPanel.Height + 1);
      end;
end;

procedure TFormEditFrame.EventEditorGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
var
  i: integer;
begin
  Values.Add('(Kein)');
  Values.Add('(Neu...)');
  Values.AddStrings(FFuncList);
end;

procedure TFormEditFrame.EventEditorPickListSelect(Sender: TObject);
var
  s, v: string;
  i: integer;
begin
  s := EventEditor.Rows[EventEditor.Row][0];
  v := EventEditor.Rows[EventEditor.Row][1];
  if v = '(Kein)' then
  begin
    EventEditor.Values[s] := '';
    Exit;
  end;
  if v = '' then
    Exit;
  if v = '(Neu...)' then
  begin
    v := FormControlView.Selected.Text + Copy(s, 3, Length(s));
    if StringsContain(FFuncList, v) then
    begin
      i := 1;
      while StringsContain(FFuncList, v + IntToStr(i)) do
        Inc(i);
      v := v + IntToStr(i);
    end;
    EventEditor.Values[s] := v;
  end;
  if Assigned(FEnterFunc) then
    FEnterFunc(ChangeFileExt(FFileName, '.aal1'), v, '', True);
end;

procedure TFormEditFrame.FormControlViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);

  function isValidName(s: string): boolean;
  var
    c: char;
  begin
    Result := Length(s) > 0;
    for c in s do
      if not (c in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  if not isValidName(s) then
  begin
    s := Node.Text;
    Exit;
  end;
  if TControl(Node.Data) = FormPanel then
    FFormName := s
  else
    TControl(Node.Data).Name := s;
  FormControlViewChange(Sender, Node);
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnVarChanged) then
    FOnVarChanged(Self);
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
  begin
    Application.QueueAsyncCall(TDataEvent(@DeleteItem),
      IntPtr(FormControlView.Selected));
    if Assigned(FOnVarChanged) then
      FOnVarChanged(Self);
  end;
end;

procedure TFormEditFrame.FormPanelDblClick(Sender: TObject);
var
  s: string;
begin
  EventEditor.Row := 0;
  s := EventEditor.Rows[0][0];
  if EventEditor.Values[s] = '' then
    EventEditor.Values[s] := '(Neu...)';
  EventEditorPickListSelect(EventEditor);
end;

procedure TFormEditFrame.Save(p: string = '');
var
  sl: TStringList;
  i: integer;
  c: TControl;
begin
  if p = '' then
    p := FFileName;
  sl := TStringList.Create;
  try
    sl.Add(Format('$%s = CreateWindow("%s", %d, %d, %d, %d, %d)',
      [FFormName, FormCaptionLabel.Caption, FFormLeft, FFormTop,
      FormPanel.Width + 16, FormPanel.Height + 32, 0]));

    for i := 0 to FFormEvents.Count - 1 do
      if FFormEvents.ValueFromIndex[i] <> '' then
        sl.Add(Format('SetOnEvent($%s, "%s","%s")',
          [FFormName, FFormEvents.Names[i], FFormEvents.ValueFromIndex[i]]));
    for i := 0 to FEditorControls.Count - 1 do
    begin
      c := FEditorControls[i] as TControl;
      if c is TAALButton then
      begin
        sl.Add((c as TAALButton).GetAALString(FFormName));
        (c as TAALButton).AddEvents(sl);
      end
      else if c is TAALLabel then
      begin
        sl.Add((c as TAALLabel).GetAALString(FFormName));
        (c as TAALLabel).AddEvents(sl);
      end
      else if c is TAALCheckbox then
      begin
        sl.Add((c as TAALCheckbox).GetAALString(FFormName));
        (c as TAALCheckbox).AddEvents(sl);
      end
      else if c is TAALEdit then
      begin
        sl.Add((c as TAALEdit).GetAALString(FFormName));
        (c as TAALEdit).AddEvents(sl);
      end;
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
        NewPos := len + 2;
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
  idx: integer;
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
        ReadFunc(Trim(Lines[i]), FuncParams);
        if (LowerCase(FuncParams[0]) = '$' + LowerCase(FFormName)) then
          FFormEvents.Values[FuncParams[1]] := FuncParams[2]
        else
        begin
          idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
          if (idx < 0) or (idx > FEditorControls.Count - 1) then
            Continue;
          c := FEditorControls[idx] as TControl;
          if c is TAALButton then
            (c as TAALButton).Event[FuncParams[1]] := FuncParams[2]
          else if c is TAALLabel then
            (c as TAALLabel).Event[FuncParams[1]] := FuncParams[2]
          else if c is TAALEdit then
            (c as TAALEdit).Event[FuncParams[1]] := FuncParams[2]
          else if c is TAALCheckbox then
            (c as TAALCheckbox).Event[FuncParams[1]] := FuncParams[2];
        end;
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
          FormPanel.Width := StrToInt(FuncParams[3]) - 16;
          FormPanel.Height := StrToInt(FuncParams[4]) - 32;
          FFormStyle := StrToInt(FuncParams[5]);
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
          (c as TAALButton).ControlProp['Style'] := FuncParams[6];
          (c as TAALButton).ControlProp['StyleEx'] := FuncParams[7];
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
          (c as TAALCheckbox).ControlProp['Style'] := FuncParams[6];
          (c as TAALCheckbox).ControlProp['StyleEx'] := FuncParams[7];
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
          (c as TAALLabel).ControlProp['Style'] := FuncParams[6];
          (c as TAALLabel).ControlProp['StyleEx'] := FuncParams[7];
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
          (c as TAALEdit).Text := FuncParams[1];
          c.Left := StrToInt(FuncParams[2]);
          c.Top := StrToInt(FuncParams[3]);
          c.Width := StrToInt(FuncParams[4]);
          c.Height := StrToInt(FuncParams[5]);
          (c as TAALEdit).ControlProp['Style'] := FuncParams[6];
          (c as TAALEdit).ControlProp['StyleEx'] := FuncParams[7];
          Inc(curr);
        end;
      end;
    end;
  finally
    FuncParams.Free;
    Lines.Free;
  end;
  if Assigned(FOnVarChanged) then
    FOnVarChanged(Self);
  Self.Parent.Caption := ExtractFileName(p);
  FormControlView.Select(FormControlView.Items[0]);
  PositionPicker.Invalidate;
  FFileName := p;
end;

end.
