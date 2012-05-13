unit DropDownButton;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, DropDownManager, Controls, LCLType, Buttons, Forms,
  Graphics;

type

  TDropDownButtonOption = (dboShowIndicator);

  TDropDownButtonOptions = set of TDropDownButtonOption;

  { TDropDownButton }
  //todo add TDropDownBaseButton or TCustomDropDownButton to merge functions of TMenuButton
  TDropDownButton = class(TCustomSpeedButton)
  private
    FClientWidth: Integer;
    FContentWidth: Integer;
    FManager: TDropDownManager;
    FOptions: TDropDownButtonOptions;
    procedure DropDownHide(Sender: TObject);
    procedure FormVisibleChange(Sender: TObject; Form: TCustomForm);
    function GetDropDownControl: TWinControl;
    function GetDroppedDown: Boolean;
    procedure SetDropDownControl(const Value: TWinControl);
    procedure SetDroppedDown(const Value: Boolean);
    procedure SetOptions(Value: TDropDownButtonOptions);
  protected
    function GetGlyphSize(Drawing: Boolean; PaintRect: TRect): TSize; override;
    function GetTextSize(Drawing: Boolean; PaintRect: TRect): TSize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Paint; override;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Manager: TDropDownManager read FManager;
  published
    property DropDownControl: TWinControl read GetDropDownControl write SetDropDownControl;
    property Options: TDropDownButtonOptions read FOptions write SetOptions default [dboShowIndicator];
    //
    property Action;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
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
  MenuButton;

function SuccPoint(const P: TPoint): TPoint;
begin
  Result := Point(P.x+1,P.y+1);
end;

function CalculateArrowPoints(XOffset, AWidth, AHeight: Integer): TArrowPoints;
var
  ArrowTop, ArrowWidth: Integer;
begin
  ArrowWidth := (AWidth - 1) div 2;
  if Odd(ArrowWidth) then
    Dec(ArrowWidth);
  ArrowTop := (AHeight - (ArrowWidth div 2)) div 2;
  with Result[acLeft] do
  begin
    X := ((AWidth - ArrowWidth) div 2) - 1 + XOffset;
    Y := ArrowTop;
  end;
  with Result[acRight] do
  begin
    X := Result[acLeft].X + ArrowWidth;
    Y := ArrowTop;
  end;
  with Result[acBottom] do
  begin
    X := Result[acLeft].X + (ArrowWidth div 2);
    Y := ArrowTop + (ArrowWidth div 2);
  end;
end;


{ TDropDownButton }

procedure TDropDownButton.DropDownHide(Sender: TObject);
begin
  Down := False;
end;

procedure TDropDownButton.FormVisibleChange(Sender: TObject; Form: TCustomForm);
begin
  if Form.Visible then
  begin
    FManager.UpdateState;
    Down := FManager.DroppedDown;
  end;
  Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChange);
end;

function TDropDownButton.GetDropDownControl: TWinControl;
begin
  Result := FManager.Control;
end;

function TDropDownButton.GetDroppedDown: Boolean;
begin
  Result := FManager.DroppedDown;
end;

procedure TDropDownButton.SetDropDownControl(const Value: TWinControl);
begin
  FManager.Control := Value;
end;

procedure TDropDownButton.SetDroppedDown(const Value: Boolean);
begin
  Down := Value;
  FManager.DroppedDown := Value;
end;

procedure TDropDownButton.SetOptions(Value: TDropDownButtonOptions);
begin
  if FOptions = Value then exit;
  FOptions := Value;
end;

function TDropDownButton.GetGlyphSize(Drawing: Boolean; PaintRect: TRect): TSize;
begin
  Result := inherited GetGlyphSize(Drawing, PaintRect);
  if (dboShowIndicator in FOptions) then
  begin
    Inc(FContentWidth, Result.Cx);
    if Result.cx > 0 then
    begin
      //if no glyph the returned text width must be smaller
      if Result.cx = FContentWidth then
        Inc(Result.cx, 6)
      else
        Inc(Result.cx, 12);
    end;
  end;
end;

function TDropDownButton.GetTextSize(Drawing: Boolean; PaintRect: TRect): TSize;
begin
  Result := inherited GetTextSize(Drawing, PaintRect);
  if (dboShowIndicator in FOptions) then
  begin
    FClientWidth := PaintRect.Right - PaintRect.Left;
    if Layout in [blGlyphLeft, blGlyphRight] then
      Inc(FContentWidth, Result.cx);
    if not ShowCaption or (Caption = '') then
      Inc(Result.Cx, 6);
  end;
end;

procedure TDropDownButton.Loaded;
begin
  inherited Loaded;
  Screen.AddHandlerFormVisibleChanged(@FormVisibleChange);
end;

constructor TDropDownButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FManager := TDropDownManager.Create(Self);
  FManager.MasterControl := Self;
  FManager.OnHide := @DropDownHide;
  //necessary to the button toggle
  AllowAllUp := True;
  GroupIndex := 1;
  FOptions := [dboShowIndicator];
end;

destructor TDropDownButton.Destroy;
begin
  Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChange);
  inherited Destroy;
end;

procedure TDropDownButton.Click;
begin
  inherited Click;
  FManager.DroppedDown := Down;
end;

procedure TDropDownButton.Paint;
var
  ArrowPoints: TArrowPoints;
  XOffset: Integer;
begin
  FContentWidth := 0;
  inherited Paint;
  {
  if (dboShowIndicator in FOptions) then
  begin
    Canvas.Brush.Color := clBlack;
    XOffset := (FClientWidth - FContentWidth) div 2;
    if XOffset >= 6 then
      Dec(XOffset, 1)
    else
      Dec(XOffset, 2);
    ArrowPoints := CalculateArrowPoints(FContentWidth + XOffset, 10, Height);
    if FState = bsDown then
      Canvas.Polygon([SuccPoint(ArrowPoints[acLeft]), SuccPoint(ArrowPoints[acRight]),
        SuccPoint(ArrowPoints[acBottom])])
    else
      Canvas.Polygon([ArrowPoints[acLeft], ArrowPoints[acRight], ArrowPoints[acBottom]]);
  end;
  }
end;

end.

