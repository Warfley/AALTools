unit FormEditComponents;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ValEdit,
  LCLIntf;

type
  TWindowStyle = (
    WS_MAXIMIZEBOX,
    WS_MINIMIZEBOX,
    WS_SIZEBOX,
    WS_SYSMENU,
    WS_HSCROLL,
    WS_VSCROLL,
    WS_DLGFRAME,
    WS_BORDER,
    WS_MAXIMIZE,
    WS_CLIPCHILDREN,
    WS_CLIPSIBLINGS,
    WS_DISABLED,
    WS_VISIBLE,
    WS_MINIMIZE,
    WS_CHILD,
    WS_POPUP);
  TWindowStyles = set of TWindowStyle;

  TWindowExStyle = (
    WS_EX_DLGMODALFRAME,
    Filler1,
    WS_EX_NOPARENTNOTIFY,
    WS_EX_TOPMOST,
    WS_EX_ACCEPTFILES,
    WS_EX_TRANSPARENT,
    WS_EX_MDICHILD,
    WS_EX_TOOLWINDOW,
    WS_EX_WINDOWEDGE,
    WS_EX_CLIENTEDGE,
    WS_EX_CONTEXTHELP,
    Filler2,
    WS_EX_RIGHT,
    WS_EX_RTLREADING,
    WS_EX_LEFTSCROLLBAR,
    Filler3,
    WS_EX_CONTROLPARENT,
    WS_EX_STATICEDGE,
    WS_EX_APPWINDOW
    );
  TWindowExStyles = set of TWindowExStyle;

  TEditStyle = (
    ES_CENTER,
    ES_RIGHT,
    ES_MULTILINE,
    ES_UPPERCASE,
    ES_LOWERCASE,
    ES_PASSWORD,
    ES_AUTOVSCROLL,
    ES_AUTOHSCROLL,
    ES_NOHIDESEL,
    Filler4,
    ES_OEMCONVERT,
    ES_READONLY,
    ES_WANTRETURN,
    ES_NUMBER
    );
  TEditStyles = set of TEditStyle;

  TButtonStyle = (BS_DEFPUSHBUTTON,
    BS_CHECKBOX,
    BS_RADIOBUTTON,
    BS_USERBUTTON,
    Filler5,
    BS_LEFTTEXT,
    BS_ICON,
    BS_BITMAP,
    BS_LEFT,
    BS_RIGHT,
    BS_TOP,
    BS_BOTTOM,
    BS_PUSHLIKE,
    BS_MULTILINE,
    BS_NOTIFY,
    BS_FLAT
    );
  TButtonStyles = set of TButtonStyle;

  TStaticStyle = (
    SS_CENTER,
    SS_RIGHT,
    SS_BLACKRECT,
    SS_GRAYFRAME,
    SS_ETCHEDHORZ,
    Filler6,
    Filler7,
    SS_NOPREFIX,
    SS_NOTIFY,
    SS_CENTERIMAGE,
    SS_RIGHTJUST,
    SS_REALSIZEIMAGE,
    SS_SUNKEN
    );
  TStaticStyles = set of TStaticStyle;

  TPropertyChangeEvent = procedure(Sender: TObject;
    PropName, PropVal: string) of object;

  IAALComponent = interface
    ['{DE4489A7-9015-405B-8123-AF253975EBA0}']
    procedure CopyTo(c: TControl);
    procedure FillEvents(g: TValueListEditor);
    function GetAALString(FormName: string): string;

    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(prop: string): string;
    procedure SetProp(prop, val: string);
    function GetEvents: TStringList;
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    procedure AddEvents(sl: TStringList);

    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read GetEvents;
    property OnChangeProp: TPropertyChangeEvent
      read GetOnChangeProp write SetOnChangeProp;
  end;

  TAALForm = class(TCustomPanel, IAALComponent)
  private
    FStyle: cardinal;
    FEvents: TStringList;
    FLeft, FTop: integer;
    FOnChangeProp: TPropertyChangeEvent;
    FCaption: string;
    FOnChangeCaption: TNotifyEvent;
    function GetEditorTop: integer;
    function GetEditorLeft: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    function GetStyle: TWindowStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure Paint; override;
  public
    procedure SetFormPos(x, y: integer);
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function GetAALString(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property EditorTop: integer read GetEditorTop;
    property EditorLeft: integer read GetEditorLeft;
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property Text: string read FCaption write SetText;
    property Caption: string read FCaption write SetText;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property OnClick;
    property OnEnter;
    property OnExit;
    property Anchors;
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

  TAALEdit = class(TCustomEdit, IAALComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetEditStyle(val: TEditStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetEditStyle: TEditStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(prop: string): string;
    procedure SetProp(prop, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function GetAALString(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property EditStyle: TEditStyles read GetEditStyle write SetEditStyle;
    property CompleteStyle: cardinal read FStyle write FStyle;
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
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property TextHint;
    property TextHintFontColor;
    property TextHintFontStyle;
    property Visible;
  end;

  TAALButton = class(TCustomButton, IAALComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FLastClick: cardinal;
    FOnChangeProp: TPropertyChangeEvent;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    procedure Click; override;
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function GetAALString(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
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

  TAALCheckbox = class(TCustomCheckBox, IAALComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
  protected
    procedure Click; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function GetAALString(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
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

  TAALLabel = class(TCustomControl, IAALComponent)
  private
    FStyle: Cardinal;
    FStyleEX: Cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
    FCaption: string;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetStaticStyle(val: TStaticStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetStaticStyle: TStaticStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure Paint; override;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function GetAALString(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);
    procedure AddEvents(sl: TStringList);
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Left: integer read GetLeft write SetLeft;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property StaticStyle: TStaticStyles read GetStaticStyle write SetStaticStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read FCaption write SetText;
    property Caption: string read FCaption write SetText;
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

{ Form }

procedure TAALForm.SetFormPos(x, y: integer);
begin
  inherited Left := x;
  inherited Top := y;
end;

procedure TAALForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);
  Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  inherited;
end;

function TAALForm.GetEditorTop: integer;
begin
  Result := inherited Top;
end;

function TAALForm.GetEditorLeft: integer;
begin
  Result := inherited Left;
end;

function TAALForm.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (Pos('ws_', prop) = 1);
end;

function TAALForm.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure TAALForm.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  inherited Caption := '';
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure TAALForm.SetLeft(Val: integer);
begin
  FLeft := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure TAALForm.SetTop(Val: integer);
begin
  FTop := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure TAALForm.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure TAALForm.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function TAALForm.GetLeft: integer;
begin
  Result := FLeft;
end;

function TAALForm.GetTop: integer;
begin
  Result := FTop;
end;

function TAALForm.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TAALForm.GetHeight: integer;
begin
  Result := inherited Height;
end;

procedure TAALForm.SetStyle(val: TWindowStyles);
begin
  FStyle := DWord(val) shl 16;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(FStyle));
end;

function TAALForm.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function TAALForm.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure TAALForm.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

function TAALForm.GetProp(p: string): string;
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
    Result := IntToStr(FStyle);
end;

procedure TAALForm.SetProp(p, val: string);
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
    FStyle := StrToInt(val);
end;

function TAALForm.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure TAALForm.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

constructor TAALForm.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  Height := 312;
  Width := 386;
  inherited Name := 'Form1';
  FCaption := 'Form1';
  inherited Caption := '';
  FEvents.Values['onClick'] := '';
end;

destructor TAALForm.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure TAALForm.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is TAALForm) then
  begin
    (c as TAALForm).Style := Style;
    (c as TAALForm).Events.Assign(FEvents);
  end;
end;

function TAALForm.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateWindow("%s", %d, %d, %d, %d, %d)',
    [Name, FCaption, FLeft, FTop, Width + 16, Height + 32, FStyle]);
end;

procedure TAALForm.SetText(val: string);
begin
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeCaption) then
    FOnChangeCaption(Self);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val);
end;

procedure TAALForm.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

procedure TAALForm.AddEvents(sl: TStringList);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
    if FEvents.ValueFromIndex[i] <> '' then
      sl.Add(Format('SetOnEvent($%s, "%s","%s")',
        [Name, FEvents.Names[i], FEvents.ValueFromIndex[i]]));
end;

{ Edit }

function TAALEdit.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'editstyle') or
    (Pos('ws_', prop) = 1) or (Pos('es_', prop) = 1);
end;

function TAALEdit.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure TAALEdit.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure TAALEdit.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure TAALEdit.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure TAALEdit.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure TAALEdit.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function TAALEdit.GetLeft: integer;
begin
  Result := inherited Left;
end;

function TAALEdit.GetTop: integer;
begin
  Result := inherited Top;
end;

function TAALEdit.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TAALEdit.GetHeight: integer;
begin
  Result := inherited Height;
end;

function TAALEdit.GetText: string;
begin
  Result := inherited Text;
end;

procedure TAALEdit.SetText(val: string);
begin
  inherited Text := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure TAALEdit.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure TAALEdit.SetEditStyle(val: TEditStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or DWord(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'EditStyle', IntToStr(cardinal(val)));
end;

procedure TAALEdit.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function TAALEdit.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function TAALEdit.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function TAALEdit.GetEditStyle: TEditStyles;
begin
  Result := TEditStyles(FStyle and $FFFF);
end;

function TAALEdit.GetProp(prop: string): string;
begin
  prop := LowerCase(prop);
  if prop = 'name' then
    Result := Name
  else if prop = 'text' then
    Result := Text
  else if prop = 'x' then
    Result := IntToStr(Left)
  else if prop = 'y' then
    Result := IntToStr(Top)
  else if prop = 'width' then
    Result := IntToStr(Width)
  else if prop = 'height' then
    Result := IntToStr(Height)
  else if prop = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if prop = 'editstyle' then
    Result := IntToStr(cardinal(GetEditStyle))
  else if prop = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure TAALEdit.SetProp(prop, val: string);
begin
  prop := LowerCase(prop);
  if (prop = 'name') and isValidName(val) then
    Name := val
  else if prop = 'text' then
    Text := val
  else if (prop = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (prop = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (prop = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (prop = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (prop = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (prop = 'editstyle') and isNumeric(val) then
    SetEditStyle(TEditStyles(StrToInt(val)))
  else if (prop = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function TAALEdit.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

function TAALEdit.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure TAALEdit.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
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

procedure TAALEdit.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if (c is TAALEdit) then
  begin
    if Name = Text then
      (c as TAALEdit).Text := c.Name
    else
      (c as TAALEdit).Text := Text;
    (c as TAALEdit).Style := Style;
    (c as TAALEdit).EditStyle := EditStyle;
    (c as TAALEdit).StyleEX := StyleEX;
    (c as TAALEdit).Events.Assign(FEvents);
  end;
end;

function TAALEdit.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateInputbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Text, Left, Top, Width, Height, FStyle, FStyleEX]);
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

function TAALButton.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'buttonstyle') or
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1);
end;

function TAALButton.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure TAALButton.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure TAALButton.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure TAALButton.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure TAALButton.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure TAALButton.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function TAALButton.GetLeft: integer;
begin
  Result := inherited Left;
end;

function TAALButton.GetTop: integer;
begin
  Result := inherited Top;
end;

function TAALButton.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TAALButton.GetHeight: integer;
begin
  Result := inherited Height;
end;

function TAALButton.GetText: string;
begin
  Result := inherited Caption;
end;

procedure TAALButton.SetText(val: string);
begin
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure TAALButton.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure TAALButton.SetButtonStyle(val: TButtonStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)));
end;

procedure TAALButton.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function TAALButton.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function TAALButton.GetButtonStyle: TButtonStyles;
begin
  Result := TButtonStyles(FStyle and $FFFF);
end;

function TAALButton.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function TAALButton.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure TAALButton.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

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
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
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
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
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

procedure TAALButton.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is TAALButton) then
  begin
    (c as TAALButton).CompleteStyle := CompleteStyle;
    (c as TAALButton).StyleEX := StyleEX;
    (c as TAALButton).Events.Assign(FEvents);
  end;
end;

function TAALButton.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateButton($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
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

function TAALCheckBox.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'buttonstyle') or
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1);
end;

function TAALCheckBox.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure TAALCheckBox.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure TAALCheckBox.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure TAALCheckBox.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure TAALCheckBox.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure TAALCheckBox.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function TAALCheckBox.GetLeft: integer;
begin
  Result := inherited Left;
end;

function TAALCheckBox.GetTop: integer;
begin
  Result := inherited Top;
end;

function TAALCheckBox.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TAALCheckBox.GetHeight: integer;
begin
  Result := inherited Height;
end;

function TAALCheckBox.GetText: string;
begin
  Result := inherited Caption;
end;

procedure TAALCheckBox.SetText(val: string);
begin
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure TAALCheckbox.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure TAALCheckbox.SetButtonStyle(val: TButtonStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)));
end;

procedure TAALCheckbox.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function TAALCheckbox.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function TAALCheckbox.GetButtonStyle: TButtonStyles;
begin
  Result := TButtonStyles(FStyle and $FFFF);
end;

function TAALCheckbox.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function TAALCheckBox.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure TAALCheckBox.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

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
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
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
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
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

procedure TAALCheckbox.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is TAALCheckbox) then
  begin
    (c as TAALCheckbox).CompleteStyle := CompleteStyle;
    (c as TAALCheckbox).StyleEX := StyleEX;
    (c as TAALCheckbox).Events.Assign(FEvents);
  end;
end;

function TAALCheckbox.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateCheckbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
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

function TAALLabel.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'staticstyle') or
    (Pos('ws_', prop) = 1) or (Pos('ss_', prop) = 1);
end;

function TAALLabel.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure TAALLabel.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure TAALLabel.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure TAALLabel.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure TAALLabel.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure TAALLabel.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function TAALLabel.GetLeft: integer;
begin
  Result := inherited Left;
end;

function TAALLabel.GetTop: integer;
begin
  Result := inherited Top;
end;

function TAALLabel.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TAALLabel.GetHeight: integer;
begin
  Result := inherited Height;
end;

procedure TAALLabel.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure TAALLabel.SetStaticStyle(val: TStaticStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StaticStyle', IntToStr(cardinal(val)));
end;

procedure TAALLabel.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function TAALLabel.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function TAALLabel.GetStaticStyle: TStaticStyles;
begin
  Result := TStaticStyles(FStyle and $FFFF);
end;

function TAALLabel.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function TAALLabel.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure TAALLabel.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

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
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'staticstyle' then
    Result := IntToStr(cardinal(GetStaticStyle))
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
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'staticstyle') and isNumeric(val) then
    SetStaticStyle(TStaticStyles(StrToInt(val)))
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

procedure TAALLabel.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is TAALLabel) then
  begin
    (c as TAALLabel).CompleteStyle := CompleteStyle;
    (c as TAALLabel).StyleEX := StyleEX;
    (c as TAALLabel).Events.Assign(FEvents);
  end;
end;

function TAALLabel.GetAALString(FormName: string): string;
begin
  Result := Format('$%s = CreateLabel($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure TAALLabel.SetText(val: string);
begin
  Width := Canvas.TextWidth(val);
  Height := Canvas.TextHeight(val);
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val);
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
