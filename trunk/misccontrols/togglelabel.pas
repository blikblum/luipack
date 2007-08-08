unit togglelabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LMessages;

type

  { TToggleLabel }

  TToggleLabel = class (TCustomLabel)
  private
    FExpanded: Boolean;
    FExpandedCaption: String;
    FMouseInControl: Boolean;
    FOnChange: TNotifyEvent;
    FTextOffset: Integer;
    procedure SetExpanded(const AValue: Boolean);
    procedure SetExpandedCaption(const AValue: String);
  protected
    function GetLabelText: string; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
    procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer;
                                Lock: boolean = true); override;
  published
    property ExpandedCaption: String read FExpandedCaption write SetExpandedCaption;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

implementation

uses
  LCLIntf;

{ TToggleLabel }

procedure TToggleLabel.SetExpanded(const AValue: Boolean);
begin
  if FExpanded <> AValue then
  begin
    FExpanded := AValue;
    Invalidate;
  end;
end;

procedure TToggleLabel.SetExpandedCaption(const AValue: String);
begin
  if FExpandedCaption <> AValue then
  begin
    FExpandedCaption := AValue;
    TextChanged;
  end;
end;

function TToggleLabel.GetLabelText: string;
begin
  if FExpanded then
    Result := FExpandedCaption
  else
    Result := Caption;
end;

//The MouseInControl idea/code was borrowed from LCL.TCustomSpeedButton

procedure TToggleLabel.MouseEnter;
begin
  inherited MouseEnter;
  if csDesigning in ComponentState then
    Exit;
  if not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TToggleLabel.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then
    Exit;
  if FMouseInControl then
  begin
    FMouseInControl := False;
    //todo: Update only the glyph area
    if Enabled then
      Invalidate;
  end;
end;

procedure TToggleLabel.WMLButtonDown(var Message: TLMLButtonDown);
begin
  FExpanded := not FExpanded;
  if Assigned(FOnChange) then
    FOnChange(Self);
  TextChanged;
  Invalidate;
  inherited WMLButtonDown(Message);
end;

constructor TToggleLabel.Create(TheOwner: TComponent);
begin
  //todo: define Toggle button size dinamically instead of using a fixed value
  FTextOffset := 12;
  inherited Create(TheOwner);
end;

procedure TToggleLabel.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer;
  Lock: boolean);
begin
  inherited SetBoundsKeepBase(aLeft, aTop, aWidth + FTextOffset, aHeight, Lock);
end;

procedure TToggleLabel.Paint;
var
  TR: TTextStyle;
  R: TRect;
  TextLeft, TextTop, lTextHeight, lTextWidth: Integer;
  LabelText: String;

begin
  //There's no way to change the left text position.
  // So i copied the original LCL code until a better option exists
  R := Rect(0,0,Width,Height);
  with Canvas do
  begin
    if Enabled then
      Color := Self.Color
    else
      Color := clNone;
    Font := Self.Font;
    if (Color<>clNone) and not Transparent then
    begin
      Brush.Style:=bsSolid;
      FillRect(R);
    end
    else
      Brush.Style:=bsClear;

    FillChar(TR,SizeOf(TR),0);
    with TR do
    begin
      Alignment := Self.Alignment;
      WordBreak := WordWrap;
      SingleLine:= not WordWrap and not HasMultiLine;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
      SystemFont := False;
      RightToLeft := UseRightToLeftReading;
    end;
    //DoMeasureTextPosition(TextTop, TextLeft);

    TextLeft := FTextOffset; //here's the my change to original code
    if Layout = tlTop then begin
      TextTop := 0;
    end else begin
      CalcSize(lTextWidth, lTextHeight);
      case Layout of
        tlCenter: TextTop := (Height - lTextHeight) div 2;
        tlBottom: TextTop := Height - lTextHeight;
      end;
    end;

    LabelText := GetLabelText;
    if not Enabled then begin
      Font.Color := clBtnHighlight;
      TextRect(R, TextLeft + 1, TextTop + 1, LabelText, TR);
      Font.color := clBtnShadow;
    end;
    TextRect(R, TextLeft, TextTop, LabelText, TR);

    //Paint Toggle button
    //todo: see what todo when not Enabled or color = clNone
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    if FMouseInControl then
      Brush.Color := clWhite
    else
      Brush.Color := clBlack;
    if FExpanded then
      Polygon([Point(0, 4), Point(8, 4), Point(4, 8)])
    else
      Polygon([Point(2, 2), Point(6, 6), Point(2, 10)]);
  end;
end;

end.

