unit LuiCairoControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoLCL, CairoClasses, Controls, Types, Menus, Maps, Cairo14,
  LCLType, Graphics;
  
type

  TLuiContainer = class;

  TLuiWidget = class;
  
  TLuiWidgetState = (wsMouseInWidget);
  
  TLuiWidgetStates = set of TLuiWidgetState;

  TLuiWidgetOption = (
    lwoHotTrack
  );

  TLuiWidgetOptions = set of TLuiWidgetOption;
  
  TLuiWidgetDrawEvent = procedure (Sender: TLuiWidget; Context: TCairoContext) of object;

  { TLuiCairoPatternList }

  TLuiCairoPatternList = class
  private
    FBackground: TCairoPattern;
    FBorder: TCairoPattern;
    FPatternMap: TMap;
    FInvalid: Boolean;
    FFixedValues: Boolean;
    FText: TCairoPattern;
    procedure FreePatternMap;
    function GetItems(Index: Integer): TCairoPattern;
    function GetRequiresUpdate: Boolean;
    procedure PatternMapNeeded;
    procedure SetBackground(const AValue: TCairoPattern);
    procedure SetBorder(const AValue: TCairoPattern);
    procedure SetItems(Index: Integer; const AValue: TCairoPattern);
    procedure SetText(const AValue: TCairoPattern);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    procedure Updated;
    property RequiresUpdate: Boolean read GetRequiresUpdate;
    property FixedValues: Boolean read FFixedValues write FFixedValues;
    property Items[Index: Integer]: TCairoPattern read GetItems write SetItems; default;
    //Common patterns
    property Background: TCairoPattern read FBackground write SetBackground;
    property Text: TCairoPattern read FText write SetText;
    property Border: TCairoPattern read FBorder write SetBorder;
  end;

  { TLuiWidget }

  TLuiWidget = class (TComponent)
  private
    FBorderWidth: Integer;
    FBoundsRect: TRect;
    FOnDraw: TLuiWidgetDrawEvent;
    FOnMouseDown: TMouseEvent;
    FPatterns: TLuiCairoPatternList;
    FWidgetOptions: TLuiWidgetOptions;
    FParent: TLuiContainer;
    FStates: TLuiWidgetStates;
    function GetHeight: Integer;
    function GetPatterns: TLuiCairoPatternList;
    function GetWidth: Integer;
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetOnDraw(const AValue: TLuiWidgetDrawEvent);
    procedure SetOnMouseDown(const AValue: TMouseEvent);
    procedure SetOptions(const AValue: TLuiWidgetOptions);
  protected
    procedure Draw(Context: TCairoContext); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    property Patterns: TLuiCairoPatternList read GetPatterns;
  public
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BoundsRect: TRect read FBoundsRect;
    property Height: Integer read GetHeight;
    property Left: Integer read FBoundsRect.Left;
    property WidgetOptions: TLuiWidgetOptions read FWidgetOptions write SetOptions;
    property Parent: TLuiContainer read FParent;
    property States: TLuiWidgetStates read FStates;
    property Top: Integer read FBoundsRect.Top;
    property Width: Integer read GetWidth;
    //events
    property OnDraw: TLuiWidgetDrawEvent read FOnDraw write SetOnDraw;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
  end;

  //todo:
  // - create a common ancestor for all classes with a Caption??
  // - add border/colors/background options

  { TLuiLabel }

  TLuiLabel = class (TLuiWidget)
  private
    FCaption: String;
    procedure SetCaption(const AValue: String);
  protected
    procedure Draw(Context: TCairoContext); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: String read FCaption write SetCaption;
    property Patterns;
  end;

  { TLuiMenuButton }

  TLuiMenuButton = class (TLuiWidget)
  private
    FCaption: String;
    FPopupMenu: TPopupMenu;
    procedure SetCaption(const AValue: String);
    procedure SetPopupMenu(const AValue: TPopupMenu);
  protected
    procedure Draw(Context: TCairoContext); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                      Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: String read FCaption write SetCaption;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Patterns;
  end;

  { TLuiWidgetList }

  TLuiWidgetList = class
  private
    FList: TFPList;
    FOwner: TLuiContainer;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): TLuiWidget; inline;
  public
    constructor Create(AOwner: TLuiContainer);
    destructor Destroy;
    procedure Add(Widget: TLuiWidget);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLuiWidget read GetItems; default;
  end;

  { TLuiContainer }

  TLuiContainer = class (TCustomCairoControl)
  private
    FWidgets: TLuiWidgetList;
    FHoverWidget: TLuiWidget;
    function WidgetInPos(X, Y: Integer): TLuiWidget;
  protected
    procedure DoDraw; override;
    procedure DrawWidgets;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Widgets: TLuiWidgetList read FWidgets;
  end;

  //helper routines
  
  function PosInRect(const ARect: TRect; X, Y: Integer): Boolean;
  
implementation

uses
  CairoUtils;

function PosInRect(const ARect: TRect; X, Y: Integer): Boolean;
begin
  Result := (Y>=ARect.Top) and
            (Y<ARect.Bottom) and
            (X>=ARect.Left) and
            (X<ARect.Right);
end;


{ TLuiWidget }

function TLuiWidget.GetHeight: Integer;
begin
  Result := FBoundsRect.Bottom - FBoundsRect.Top;
end;

function TLuiWidget.GetPatterns: TLuiCairoPatternList;
begin
  if FPatterns = nil then
    FPatterns := TLuiCairoPatternList.Create;
  Result := FPatterns;
end;

function TLuiWidget.GetWidth: Integer;
begin
  Result := FBoundsRect.Right - FBoundsRect.Left;
end;

procedure TLuiWidget.SetBorderWidth(const AValue: Integer);
begin
  if FBorderWidth=AValue then exit;
  FBorderWidth:=AValue;
end;

procedure TLuiWidget.SetOnDraw(const AValue: TLuiWidgetDrawEvent);
begin
  if FOnDraw=AValue then exit;
  FOnDraw:=AValue;
end;

procedure TLuiWidget.SetOnMouseDown(const AValue: TMouseEvent);
begin
  if FOnMouseDown=AValue then exit;
  FOnMouseDown:=AValue;
end;

procedure TLuiWidget.SetOptions(const AValue: TLuiWidgetOptions);
begin
  if FWidgetOptions=AValue then exit;
  FWidgetOptions:=AValue;
end;

procedure TLuiWidget.Draw(Context: TCairoContext);
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self, Context);
end;

procedure TLuiWidget.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TLuiWidget.MouseEnter;
begin
  Include(FStates, wsMouseInWidget);
  if lwoHotTrack in FWidgetOptions then
    FParent.Redraw;
end;

procedure TLuiWidget.MouseLeave;
begin
  Exclude(FStates, wsMouseInWidget);
  if lwoHotTrack in FWidgetOptions then
    FParent.Redraw;
end;

procedure TLuiWidget.MouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

destructor TLuiWidget.Destroy;
begin
  FPatterns.Free;
  inherited Destroy;
end;

procedure TLuiWidget.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FBoundsRect.Left := ALeft;
  FBoundsRect.Top := ATop;
  FBoundsRect.Right := ALeft + AWidth;
  FBoundsRect.Bottom := ATop + AHeight;
end;

{ TLuiContainer }

function TLuiContainer.WidgetInPos(X, Y: Integer): TLuiWidget;
var
  i: Integer;
begin
  for i := 0 to Widgets.Count - 1 do
  begin
    Result := Widgets[i];
    if PosInRect(Result.BoundsRect, X, Y) then
      Exit;
  end;
  Result := nil;
end;

procedure TLuiContainer.DoDraw;
begin
  DrawWidgets;
end;

procedure TLuiContainer.DrawWidgets;
var
  i: Integer;
  Widget: TLuiWidget;
begin
  for i := 0 to Widgets.Count - 1 do
  begin
    Widget := Widgets[i];
    Context.Save;
    Context.Translate(Widget.Left, Widget.Top);
    Widget.Draw(Context);
    Context.Restore;
  end;
end;

procedure TLuiContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FHoverWidget <> nil then
  begin
    FHoverWidget.MouseDown(Button, Shift,
      X - FHoverWidget.Left, Y - FHoverWidget.Top);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TLuiContainer.MouseLeave;
begin
  if FHoverWidget <> nil then
    FHoverWidget.MouseLeave;
  inherited MouseLeave;
end;

procedure TLuiContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHoverWidget: TLuiWidget;
begin
  NewHoverWidget := WidgetInPos(X, Y);
  if NewHoverWidget <> FHoverWidget then
  begin
    if FHoverWidget <> nil then
      FHoverWidget.MouseLeave;
    FHoverWidget := NewHoverWidget;
    if FHoverWidget <> nil then
      FHoverWidget.MouseEnter;
  end;
  if FHoverWidget <> nil then
    FHoverWidget.MouseMove(Shift, X - FHoverWidget.Left, Y - FHoverWidget.Top);
  inherited MouseMove(Shift, X, Y);
end;

constructor TLuiContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TLuiWidgetList.Create(Self);
end;

destructor TLuiContainer.Destroy;
begin
  FWidgets.Destroy;
  inherited Destroy;
end;

{ TLuiWidgetList }

function TLuiWidgetList.GetItems(Index: Integer): TLuiWidget;
begin
  Result := TLuiWidget(FList[Index]);
end;

function TLuiWidgetList.GetCount: Integer;
begin
  Result := FList.Count;
end;

constructor TLuiWidgetList.Create(AOwner: TLuiContainer);
begin
  FOwner := AOwner;
  FList := TFPList.Create;
end;

destructor TLuiWidgetList.Destroy;
begin
  FList.Destroy;
end;

procedure TLuiWidgetList.Add(Widget: TLuiWidget);
begin
  FList.Add(Widget);
  Widget.FParent := FOwner;
end;


{ TLuiMenuButton }

procedure TLuiMenuButton.SetPopupMenu(const AValue: TPopupMenu);
begin
  if FPopupMenu=AValue then exit;
  FPopupMenu:=AValue;
end;

procedure TLuiMenuButton.SetCaption(const AValue: String);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TLuiMenuButton.Draw(Context: TCairoContext);
var
  Extents: cairo_text_extents_t;
  R: TDoubleRect;
begin
  //todo: improve draw. Take as example the digg homepage
  with Context do
  begin
    LineWidth := BorderWidth;
    if wsMouseInWidget in FStates then
    begin
      CalculateSharpRect(0, 0, Width, Height, BorderWidth, R);
      RoundedRectangle(Context, R, 4);
      Source := FPatterns.Background;
      FillPreserve;
      Source := FPatterns.Border;
      Stroke;
    end;
    //draw triangle
    Source := FPatterns.Text;
    MoveTo(Width - 16, (Height / 2) - 2);
    LineTo(Width - 12, (Height / 2) + 2);
    LineTo(Width - 8, (Height / 2) - 2);
    Stroke;

    TextExtents(FCaption, @Extents);
    MoveTo(BorderWidth + 2, ((Height - Extents.height) / 2) + Extents.height);
    ShowText(FCaption);
  end;
  inherited Draw(Context);
end;

procedure TLuiMenuButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
begin
  if (FPopupMenu <> nil) and (Button = mbLeft) then
  begin
    //this check does not work. See why.
    if ActivePopupMenu <> FPopupMenu then
    begin
      P := Parent.ClientToScreen(Point(Left, Top + Height));
      FPopupMenu.PopUp(P.X, P.Y);
    end
    else
      FPopupMenu.Close;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

constructor TLuiMenuButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderWidth := 1;
  Patterns.Text := TCairoSolidPattern.Create(0, 0, 0); //black
  Patterns.Background := TCairoSolidPattern.Create(1, 1, 1); //white
  Patterns.Border := TCairoSolidPattern.Create(0, 0, 0); //black
end;

{ TLuiCairoPatternList }

procedure TLuiCairoPatternList.FreePatternMap;
var
  Iterator: TMapIterator;
  Pattern: TCairoPattern;
begin
  if FPatternMap = nil then
    Exit;
  Iterator := TMapIterator.Create(FPatternMap);
  with Iterator do
  begin
    while not EOM do
    begin
      GetData(Pattern);
      Pattern.Free;
      Next;
    end;
    Destroy;
  end;
  FPatternMap.Destroy;
end;

function TLuiCairoPatternList.GetItems(Index: Integer): TCairoPattern;
begin
  if FPatternMap = nil then
    raise Exception.Create('Pattern Map Not Initialized');
  if not FPatternMap.GetData(Index, Result) then
    raise Exception.Create('Pattern with index ' + IntToStr(Index) + ' not found');
end;

function TLuiCairoPatternList.GetRequiresUpdate: Boolean;
begin
  Result := FInvalid;
end;

procedure TLuiCairoPatternList.PatternMapNeeded;
begin
  if FPatternMap = nil then
    FPatternMap := TMap.Create(its4, SizeOf(TCairoPattern));
end;

procedure TLuiCairoPatternList.SetBackground(const AValue: TCairoPattern);
begin
  if FBackground = AValue then
    Exit;
  FBackground.Free;
  FBackground := AValue;
end;

procedure TLuiCairoPatternList.SetBorder(const AValue: TCairoPattern);
begin
  if FBorder = AValue then
    Exit;
  FBorder.Free;
  FBorder := AValue;
end;

procedure TLuiCairoPatternList.SetItems(Index: Integer;
  const AValue: TCairoPattern);
var
  OldPattern: TCairoPattern;
begin
  PatternMapNeeded;
  if FPatternMap.GetData(Index, OldPattern) then
  begin
    OldPattern.Free;
    FPatternMap.SetData(Index, AValue);
  end
  else
    FPatternMap.Add(Index, AValue);
end;

procedure TLuiCairoPatternList.SetText(const AValue: TCairoPattern);
begin
  if FText = AValue then
    Exit;
  FText.Free;
  FText := AValue;
end;

constructor TLuiCairoPatternList.Create;
begin
  FInvalid := True;
  FFixedValues := True;
end;

destructor TLuiCairoPatternList.Destroy;
begin
  FText.Free;
  FBorder.Free;
  FBackground.Free;
  FreePatternMap;
end;

procedure TLuiCairoPatternList.Invalidate;
begin
  if not FFixedValues then
    FInvalid := True
end;

procedure TLuiCairoPatternList.Updated;
begin
  FInvalid := False;
end;

{ TLuiLabel }

procedure TLuiLabel.SetCaption(const AValue: String);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TLuiLabel.Draw(Context: TCairoContext);
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    Source := FPatterns.Text;
    TextExtents(FCaption, @Extents);
    MoveTo(0, ((Height - Extents.height) / 2) + Extents.height);
    ShowText(FCaption);
  end;
  inherited Draw(Context);
end;

constructor TLuiLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Patterns.Text := TCairoSolidPattern.Create(ColorToCairoColor(clBlack));
  Patterns.Background := TCairoSolidPattern.Create(ColorToCairoColor(clWhite));
  Patterns.Border := TCairoSolidPattern.Create(ColorToCairoColor(clBlack));
end;

end.

