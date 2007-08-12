unit MenuButton;

{$mode objfpc}{$H+}
{. define DEBUG_MENUBUTTON}
interface


uses
  Forms, Classes, SysUtils, Buttons, Menus, LMessages, Controls, ActnList, Graphics {$ifdef DEBUG_MENUBUTTON}, sharedlogger {$endif};

type

  { TToggleSpeedButton }

  TToggleSpeedButton = class (TCustomSpeedButton)
  private
    FInternalDown: Boolean;
    FToggleMode: Boolean;
    FUpdatingStatus: Boolean;
    FForceInvalidate: Boolean;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
  protected
    {$ifdef DEBUG_MENUBUTTON}
    procedure Invalidate; override;
    {$endif}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure ResetState;
    procedure UpdateState(InvalidateOnChange: Boolean); override;
    procedure DoAfterClick; virtual;
  public
    property ToggleMode: Boolean read FToggleMode write FToggleMode;
  end;

  { TArrowButton }

  TArrowButton = class (TToggleSpeedButton)
  private
    FUpperLeft: TPoint;
    FUpperRight: TPoint;
    FBottom: TPoint;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  protected
    procedure DoAfterClick; override;
    procedure Paint; override;
  public
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  { TMenuButton }

  TMenuButton = class (TToggleSpeedButton)
  private
    FMenu: TPopupMenu;
    FArrowButton: TArrowButton;
    FShowButton: Boolean;
    procedure ArrowEntered(Sender: TObject);
    procedure ArrowLeaved(Sender: TObject);
    procedure ArrowClicked(Sender: TObject);
    procedure DelayedUpdate(Data: PtrInt);
    procedure HideMenu;
    procedure MenuClosed(Sender: TObject);
    procedure SetMenu(const AValue: TPopupMenu);
    procedure SetShowArrow(const AValue: Boolean);
    procedure ShowMenu;
    procedure UpdateArrowState;
  protected
    procedure DoAfterClick; override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Menu: TPopupMenu read FMenu write SetMenu;
    property ShowArrow: Boolean read FShowButton write SetShowArrow;
    property Action;
    property Align;
    property Anchors;
    property AllowAllUp;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GroupIndex;
    property Layout;
    property Margin;
    property NumGlyphs;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowCaption;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

uses
  Math;

function SuccPoint(const P: TPoint): TPoint;
begin
  Result := Point(P.x+1,P.y+1);
end;

{ TMenuButton }


procedure TMenuButton.ShowMenu;
var
  P: TPoint;
begin
  //Logger.SendCallStack('ShowMenu');
  if FMenu = nil then
    Exit;
  P := ClientToScreen(Point(0,Height));
  FMenu.PopUp(P.X, P.Y);
end;

procedure TMenuButton.UpdateArrowState;
var
  ArrowWidth: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  if FShowButton then
  begin
    if FArrowButton = nil then
    begin
      FArrowButton := TArrowButton.Create(nil);
      with FArrowButton do
      begin
        //ControlStyle := ControlStyle + [csNoDesignSelectable];
        OnMouseEnter := @ArrowEntered;
        OnMouseLeave := @ArrowLeaved;
        OnClick := @ArrowClicked;
        ToggleMode := True;
        Parent := Self.Parent;
        Flat := Self.Flat;
        ArrowWidth := Max(Self.Width div 3, 10);
        if odd(ArrowWidth) then
          Inc(ArrowWidth);
        SetBounds(Self.Left + Self.Width, Self.Top, ArrowWidth , Self.Height);
      end;
    end;
    ToggleMode := False;
    FArrowButton.Visible := True;
  end
  else
  begin
    if FArrowButton <> nil then
      FArrowButton.Visible := False;
    ToggleMode := True;
  end;
end;

procedure TMenuButton.DoAfterClick;
begin
  if ShowArrow then
    Click
  else
    if FInternalDown then
      ShowMenu;
end;

procedure TMenuButton.Loaded;
begin
  inherited Loaded;
  UpdateArrowState;
end;

procedure TMenuButton.MouseEnter;
begin
  inherited MouseEnter;
  if FArrowButton <> nil then
    FArrowButton.MouseEnter;
end;

procedure TMenuButton.MouseLeave;
begin
  inherited MouseLeave;
  if FArrowButton <> nil then
    FArrowButton.MouseLeave;
end;

procedure TMenuButton.ArrowEntered(Sender: TObject);
begin
  inherited MouseEnter;
end;

procedure TMenuButton.ArrowLeaved(Sender: TObject);
begin
  inherited MouseLeave;
end;

procedure TMenuButton.ArrowClicked(Sender: TObject);
begin
  if FArrowButton.FInternalDown then
    ShowMenu;
end;

procedure TMenuButton.DelayedUpdate(Data: PtrInt);
begin
  if csDestroying in ComponentState then
    Exit;
  if ShowArrow then
    FArrowButton.ResetState;
  ResetState;
end;

procedure TMenuButton.HideMenu;
begin
  if FMenu = nil then
    Exit;
  FMenu.Close;
end;

procedure TMenuButton.MenuClosed(Sender: TObject);
begin
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('MenuClosed');
  {$endif}
  FUpdatingStatus := True;
  Application.QueueAsyncCall(@DelayedUpdate, 0);
  {$ifdef DEBUG_MENUBUTTON}
  Logger.ExitMethod('MenuClosed');
  {$endif}
end;

procedure TMenuButton.SetMenu(const AValue: TPopupMenu);
begin
  FMenu := AValue;
  FMenu.OnClose := @MenuClosed;
end;

procedure TMenuButton.SetShowArrow(const AValue: Boolean);
begin
  if AValue <> FShowButton then
  begin
    FShowButton := AValue;
    UpdateArrowState;
  end;
end;

constructor TMenuButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToggleMode := True;
end;

destructor TMenuButton.Destroy;
begin
  FArrowButton.Free;
  inherited Destroy;
end;

{ TArrowButton }


procedure TArrowButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  ArrowTop, ArrowWidth: Integer;
begin
  inherited;
  //CalculateArrowPoints;
   ArrowWidth := (Width - 1) div 2;
  if Odd(ArrowWidth) then
    Dec(ArrowWidth);
  ArrowTop := (Height - (ArrowWidth div 2)) div 2;
  FUpperLeft.x := ((Width - ArrowWidth) div 2) - 1;
  FUpperLeft.y := ArrowTop;
  FUpperRight.x := FUpperLeft.x + ArrowWidth;
  FUpperRight.y := ArrowTop;
  FBottom.x := FUpperLeft.x + (ArrowWidth div 2);
  FBottom.y := ArrowTop + (ArrowWidth div 2);
end;

procedure TArrowButton.DoAfterClick;
begin
  inherited DoAfterClick;
end;

procedure TArrowButton.Paint;
begin
  inherited Paint;
  with Canvas do
  begin
    Brush.Color := clBlack;
    if FState = bsDown then
      Polygon([SuccPoint(FUpperLeft), SuccPoint(FUpperRight), SuccPoint(FBottom)])
    else
      Polygon([FUpperLeft, FUpperRight, FBottom]);
  end;
end;

{ TToggleSpeedButton }

procedure TToggleSpeedButton.WMLButtonDown(var Message: TLMLButtonDown);
begin
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('WMLButton');
  {$endif}
  if not FToggleMode then
    inherited WMLButtonDown(Message)
  else
  begin
    if FUpdatingStatus then
      Exit;

    Include(FControlState, csClicked);

    if Enabled then
    begin
      FInternalDown := not FInternalDown;
      {$ifdef DEBUG_MENUBUTTON}
      Logger.Send('FInternalDown',FInternalDown);
      {$endif}
      if (Action is TCustomAction) then
        TCustomAction(Action).Checked := FInternalDown;
      UpdateState(True);
      DoAfterClick;

      //FDragging := True;
    end;
  end;
  {$ifdef DEBUG_MENUBUTTON}
  Logger.ExitMethod('WMLButton');
  {$endif}
end;

{$ifdef DEBUG_MENUBUTTON}
procedure TToggleSpeedButton.Invalidate;
begin
  inherited Invalidate;
  Logger.SendCallStack(Self.ClassName);
end;
{$endif}

procedure TToggleSpeedButton.MouseEnter;
begin
  FForceInvalidate := True;
  inherited MouseEnter;
  FForceInvalidate := False;
end;

procedure TToggleSpeedButton.MouseLeave;
begin
  FForceInvalidate := True;
  inherited MouseLeave;
  FForceInvalidate := False;
end;

procedure TToggleSpeedButton.ResetState;
begin
  FUpdatingStatus := False;
  FInternalDown := False;
  UpdateState(True);
end;

procedure TToggleSpeedButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
  if FUpdatingStatus then
    Exit;
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('UpdateState');
  //Logger.SendCallStack('Stack');
  {$endif}
  if not FToggleMode then
    inherited UpdateState(InvalidateOnChange)
  else
  begin
    if Enabled then
    begin
      OldState := FState;
      if FInternalDown then
        FState := bsDown
      else
        FState := bsUp;
    end;
    if InvalidateOnChange and ((OldState <> FState) or FForceInvalidate) then
      Invalidate;
  end;
  {$ifdef DEBUG_MENUBUTTON}
  Logger.ExitMethod('UpdateState');
  {$endif}
end;

procedure TToggleSpeedButton.DoAfterClick;
begin

end;

end.

