unit FormEditComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ValEdit, LCLIntf;

type
  TAALEdit = class(TCustomEdit)
  private
    FStyle: integer;
    FStyleEX: integer;
    FEvents: TStringList;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAALString(FormName: string): string;
    procedure FillProps(g: TValueListEditor);
    procedure FillEvents(g: TValueListEditor);
    procedure OpenEditor(prop: string);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property ControlProp[s: string]: string read GetProp write SetProp;
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property NumbersOnly;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property TextHint;
    property TextHintFontColor;
    property TextHintFontStyle;
    property Visible;
  end;

  TAALButton = class(TCustomButton)
  private
    FStyle: integer;
    FStyleEX: integer;
    FEvents: TStringList;
    FLastClick: cardinal;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAALString(FormName: string): string;
    procedure FillProps(g: TValueListEditor);
    procedure FillEvents(g: TValueListEditor);
    procedure OpenEditor(prop: string);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property ControlProp[s: string]: string read GetProp write SetProp;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property OnDblClick;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  TAALCheckbox = class(TCustomCheckBox)
  private
    FStyle: integer;
    FStyleEX: integer;
    FEvents: TStringList;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAALString(FormName: string): string;
    procedure FillProps(g: TValueListEditor);
    procedure FillEvents(g: TValueListEditor);
    procedure OpenEditor(prop: string);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property ControlProp[s: string]: string read GetProp write SetProp;
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property OnDblClick;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

  TAALLabel = class(TCustomControl)
  private
    FStyle: integer;
    FStyleEX: integer;
    FEvents: TStringList;
    FCaption: string;
    procedure SetCaption(s: string);
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAALString(FormName: string): string;
    procedure FillProps(g: TValueListEditor);
    procedure FillEvents(g: TValueListEditor);
    procedure OpenEditor(prop: string);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property ControlProp[s: string]: string read GetProp write SetProp;
  published
    property Caption: string read FCaption write SetCaption;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

{ Edit }
function isNumeric(s: String): Boolean;
var c: Char;
begin
  Result:=Length(s)>0;
  for c in s do
    if not (c in ['0'..'9']) then
    begin
      Result:=False;
      Break;
    end;
end;

function isValidName(s: String): Boolean;
var c: Char;
begin
  Result:=Length(s)>0;
  for c in s do
    if not (c in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
    begin
      Result:=False;
      Break;
    end;
end;

function TAALEdit.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Text
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(FStyle)
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure TAALEdit.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Text := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    FStyle := StrToInt(val)
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function TAALEdit.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure TAALEdit.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

constructor TAALEdit.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor TAALEdit.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TAALEdit.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateInputbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Text, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure TAALEdit.FillProps(g: TValueListEditor);
begin
  g.Values['Name'] := Name;
  g.Values['Text'] := Text;
  g.Values['X'] := IntToStr(Left);
  g.ItemProps['X'].EditMask := 'd';
  g.Values['Y'] := IntToStr(Top);
  g.ItemProps['Y'].EditMask := 'd';
  g.Values['Width'] := IntToStr(Width);
  g.ItemProps['Width'].EditMask := 'd';
  g.Values['Height'] := IntToStr(Height);
  g.ItemProps['Height'].EditMask := 'd';
  g.Values['Style'] := IntToStr(FStyle);
  g.ItemProps['Style'].EditMask := 'd';
  g.Values['StyleEX'] := IntToStr(FStyleEX);
  g.ItemProps['StyleEX'].EditMask := 'd';
end;

procedure TAALEdit.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

procedure TAALEdit.OpenEditor(prop: string);
begin
  prop := LowerCase(prop);
  if prop = 'style' then
    exit//Todo
  else if prop = 'styleex' then
    exit;//Todo
end;

procedure TAALEdit.AddEvents(sl: TStringList);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
    if FEvents.ValueFromIndex[i] <> '' then
      sl.Add(Format('SetOnEvent($%s, "%s","%s")',
        [Name, FEvents.Names[i], FEvents.ValueFromIndex[i]]));
end;

{ Button }

function TAALButton.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(FStyle)
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure TAALButton.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    FStyle := StrToInt(val)
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function TAALButton.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure TAALButton.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure TAALButton.Click;
var
  c: cardinal;
begin
  inherited;
  c := GetTickCount;
  if (c - FLastClick < 700) and Assigned(OnDblClick) then
    OnDblClick(Self)
  else
    FLastClick := c;
end;

constructor TAALButton.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor TAALButton.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TAALButton.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateButton($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure TAALButton.FillProps(g: TValueListEditor);
begin
  g.Values['Name'] := Name;
  g.Values['Text'] := Caption;
  g.Values['X'] := IntToStr(Left);
  g.ItemProps['X'].EditMask := 'd';
  g.Values['Y'] := IntToStr(Top);
  g.ItemProps['Y'].EditMask := 'd';
  g.Values['Width'] := IntToStr(Width);
  g.ItemProps['Width'].EditMask := 'd';
  g.Values['Height'] := IntToStr(Height);
  g.ItemProps['Height'].EditMask := 'd';
  g.Values['Style'] := IntToStr(FStyle);
  g.ItemProps['Style'].EditMask := 'd';
  g.Values['StyleEX'] := IntToStr(FStyleEX);
  g.ItemProps['StyleEX'].EditMask := 'd';
end;

procedure TAALButton.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

procedure TAALButton.OpenEditor(prop: string);
begin
  prop := LowerCase(prop);
  if prop = 'style' then
    exit//Todo
  else if prop = 'styleex' then
    exit;//Todo
end;

procedure TAALButton.AddEvents(sl: TStringList);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
    if FEvents.ValueFromIndex[i] <> '' then
      sl.Add(Format('SetOnEvent($%s, "%s","%s")',
        [Name, FEvents.Names[i], FEvents.ValueFromIndex[i]]));
end;

{ Checkbox }

function TAALCheckbox.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(FStyle)
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure TAALCheckbox.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    FStyle := StrToInt(val)
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function TAALCheckbox.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure TAALCheckbox.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure TAALCheckbox.Click;
begin
  Checked := False;
  inherited;
end;

constructor TAALCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor TAALCheckbox.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TAALCheckbox.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateCheckbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure TAALCheckbox.FillProps(g: TValueListEditor);
begin
  g.Values['Name'] := Name;
  g.Values['Text'] := Caption;
  g.Values['X'] := IntToStr(Left);
  g.ItemProps['X'].EditMask := 'd';
  g.Values['Y'] := IntToStr(Top);
  g.ItemProps['Y'].EditMask := 'd';
  g.Values['Width'] := IntToStr(Width);
  g.ItemProps['Width'].EditMask := 'd';
  g.Values['Height'] := IntToStr(Height);
  g.ItemProps['Height'].EditMask := 'd';
  g.Values['Style'] := IntToStr(FStyle);
  g.ItemProps['Style'].EditMask := 'd';
  g.Values['StyleEX'] := IntToStr(FStyleEX);
  g.ItemProps['StyleEX'].EditMask := 'd';
end;

procedure TAALCheckbox.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

procedure TAALCheckbox.OpenEditor(prop: string);
begin
  prop := LowerCase(prop);
  if prop = 'style' then
    exit//Todo
  else if prop = 'styleex' then
    exit;//Todo
end;

procedure TAALCheckbox.AddEvents(sl: TStringList);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
    if FEvents.ValueFromIndex[i] <> '' then
      sl.Add(Format('SetOnEvent($%s, "%s","%s")',
        [Name, FEvents.Names[i], FEvents.ValueFromIndex[i]]));
end;

{ Label }

function TAALLabel.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(FStyle)
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure TAALLabel.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    FStyle := StrToInt(val)
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function TAALLabel.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure TAALLabel.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure TAALLabel.Paint;
var
  p, i: integer;
  sl: TStringList;
begin
  p := 0;
  sl := TStringList.Create;
  try
    sl.Text := Caption;
    for i := 0 to sl.Count - 1 do
    begin
      Canvas.TextOut(0, p, sl[i]);
      Inc(p, Canvas.TextHeight(sl[i]));
    end;
  finally
    sl.Free;
  end;
  inherited;
end;

constructor TAALLabel.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor TAALLabel.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TAALLabel.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateLabel($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure TAALLabel.SetCaption(s: string);
begin
  Width := Canvas.TextWidth(s);
  Height := Canvas.TextHeight(s);
  FCaption := s;
  Invalidate;
end;

procedure TAALLabel.FillProps(g: TValueListEditor);
begin
  g.Values['Name'] := Name;
  g.Values['Text'] := Caption;
  g.Values['X'] := IntToStr(Left);
  g.ItemProps['X'].EditMask := 'd';
  g.Values['Y'] := IntToStr(Top);
  g.ItemProps['Y'].EditMask := 'd';
  g.Values['Width'] := IntToStr(Width);
  g.ItemProps['Width'].EditMask := 'd';
  g.Values['Height'] := IntToStr(Height);
  g.ItemProps['Height'].EditMask := 'd';
  g.Values['Style'] := IntToStr(FStyle);
  g.ItemProps['Style'].EditMask := 'd';
  g.Values['StyleEX'] := IntToStr(FStyleEX);
  g.ItemProps['StyleEX'].EditMask := 'd';
end;

procedure TAALLabel.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

procedure TAALLabel.OpenEditor(prop: string);
begin
  prop := LowerCase(prop);
  if prop = 'style' then
    exit//Todo
  else if prop = 'styleex' then
    exit;//Todo
end;

procedure TAALLabel.AddEvents(sl: TStringList);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
    if FEvents.ValueFromIndex[i] <> '' then
      sl.Add(Format('SetOnEvent($%s, "%s","%s")',
        [Name, FEvents.Names[i], FEvents.ValueFromIndex[i]]));
end;

end.
