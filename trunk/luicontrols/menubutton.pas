unit MenuButton;

{
  Implements TMenuButton

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}
{.define DEBUG_MENUBUTTON}

//due to LCL bug gtk works only with popupmenu fire at mouseup
{$ifdef LCLGtk}
{$define FORCE_MOUSEUP}
{$endif}
{$ifdef LCLGtk2}
{$define FORCE_MOUSEUP}
{$endif}


interface


uses
  LCLType, LCLProc, Types, Forms, Classes, SysUtils, Buttons, Menus, LMessages, Controls, ActnList, Graphics
  {$ifdef DEBUG_MENUBUTTON}, sharedlogger {$endif};

type

  TMenuButton = class;

  TMenuButtonOption =
  (
    mboPopupOnMouseUp,    // The menu is popped when in MouseUp event
    mboShowIndicator
  );
  
  TMenuButtonStyle = (mbsSingle, mbsCombo);
  
  TMenuButtonOptions = set of TMenuButtonOption;

  TArrowCoordinates = (acLeft, acRight, acBottom);
  TArrowPoints = array[TArrowCoordinates] of TPoint;
  
  { TToggleSpeedButton }

  TToggleSpeedButton = class (TCustomSpeedButton)
  private
    FArrowPoints: TArrowPoints;
    FInternalDown: Boolean;
    FToggleMode: Boolean;
    FUpdateLocked: Boolean;
    FForceInvalidate: Boolean;
    procedure CalculateArrowPoints(XOffset, AWidth, AHeight: Integer);
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
  protected
    {$ifdef DEBUG_MENUBUTTON}
    procedure Invalidate; override;
    {$endif}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure ResetState;
    procedure UpdateState(InvalidateOnChange: Boolean); override;
    procedure DoButtonDown; virtual; abstract;
  public
    property ToggleMode: Boolean read FToggleMode write FToggleMode;
  end;

  { TArrowButton }

  TArrowButton = class (TToggleSpeedButton)
  private
    FMainButton: TMenuButton;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  protected
    procedure DoButtonDown; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    property MainButton: TMenuButton read FMainButton write FMainButton;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  { TMenuButton }

  TMenuButton = class (TToggleSpeedButton)
  private
    FMenu: TPopupMenu;
    FArrowButton: TArrowButton;
    FOptions: TMenuButtonOptions;
    FClientWidth: Integer;
    FContentWidth: Integer;
    FStyle: TMenuButtonStyle;
    procedure ArrowEntered(Sender: TObject);
    procedure ArrowLeaved(Sender: TObject);
    procedure DelayedUnlock(Data: PtrInt);
    procedure MenuClosed(Sender: TObject);
    procedure SetMenu(const AValue: TPopupMenu);
    procedure SetOptions(const AValue: TMenuButtonOptions);
    procedure SetStyle(const AValue: TMenuButtonStyle);
    procedure ShowMenu;
    procedure UpdateArrowPosition;
    procedure UpdateStyle;
  protected
    procedure DoButtonDown; override;
    function GetGlyphSize(PaintRect: TRect): TSize; override;
    function GetTextSize(PaintRect: TRect): TSize; override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    destructor Destroy; override;
    procedure Click; override;
  published
    property Menu: TPopupMenu read FMenu write SetMenu;
    property Options: TMenuButtonOptions read FOptions write SetOptions;
    property Style: TMenuButtonStyle read FStyle write SetStyle;
    //TSpeedButton properties
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
  //necessary to avoid Click eat when hitting multiple menubuttons successively
  //This will unlock update of previous clicked menubutton
  Application.ProcessMessages;
  P := ClientToScreen(Point(0,Height));
  FMenu.PopUp(P.X, P.Y);
end;

procedure TMenuButton.UpdateArrowPosition;
var
  XOffset: Integer;
begin
  XOffset := (FClientWidth - FContentWidth) div 2;
  if XOffset >= 6 then
    Dec(XOffset, 1)
  else
    Dec(XOffset, 2);
  CalculateArrowPoints(FContentWidth + XOffset, 10, Height)
end;

procedure TMenuButton.UpdateStyle;
var
  ArrowWidth: Integer;
begin
  if FStyle = mbsCombo then
  begin
    if FArrowButton = nil then
    begin
      FArrowButton := TArrowButton.Create(nil);
      with FArrowButton do
      begin
        //ControlStyle := ControlStyle + [csNoDesignSelectable];
        MainButton := Self;
        OnMouseEnter := @ArrowEntered;
        OnMouseLeave := @ArrowLeaved;
        ToggleMode := True;
        Parent := Self.Parent;
        Flat := Self.Flat;
        ArrowWidth := Max(Self.Width div 3, 10);
        if Odd(ArrowWidth) then
          Inc(ArrowWidth);
        SetBounds(Self.Left + Self.Width, Self.Top, ArrowWidth, Self.Height);
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

procedure TMenuButton.DoButtonDown;
begin
  if FStyle = mbsCombo then
    Click
  else
    if FInternalDown and not (mboPopupOnMouseUp in FOptions) then
      ShowMenu;
end;

function TMenuButton.GetGlyphSize(PaintRect: TRect): TSize;
begin
  Result := inherited GetGlyphSize(PaintRect);
  if (FStyle = mbsSingle) and (mboShowIndicator in FOptions) then
  begin
    FClientWidth := PaintRect.Right - PaintRect.Left;
    if Layout in [blGlyphLeft, blGlyphRight] then
      Inc(FContentWidth, Result.cx);
    if not ShowCaption or (Caption = '') then
      Inc(Result.Cx, 6);
  end;
end;

function TMenuButton.GetTextSize(PaintRect: TRect): TSize;
begin
  Result := inherited GetTextSize(PaintRect);
  if (FStyle = mbsSingle) and (mboShowIndicator in FOptions) then
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

procedure TMenuButton.Loaded;
begin
  {$ifdef FORCE_MOUSEUP}
  include(FOptions, mboPopupOnMouseUp);
  {$endif}
  inherited Loaded;
  UpdateStyle;
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

procedure TMenuButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FInternalDown and (mboPopupOnMouseUp in FOptions) and (Button = mbLeft) then
    ShowMenu;
end;

procedure TMenuButton.Paint;
begin
  FContentWidth := 0;
  inherited Paint;
  if (FStyle = mbsSingle) and (mboShowIndicator in FOptions) then
  begin
    UpdateArrowPosition;
    with Canvas do
    begin
      Brush.Color := clBlack;
      if FState = bsDown then
        Polygon([SuccPoint(FArrowPoints[acLeft]), SuccPoint(FArrowPoints[acRight]),
          SuccPoint(FArrowPoints[acBottom])])
      else
        Polygon([FArrowPoints[acLeft], FArrowPoints[acRight], FArrowPoints[acBottom]]);
    end;
  end;
end;

procedure TMenuButton.ArrowEntered(Sender: TObject);
begin
  inherited MouseEnter;
end;

procedure TMenuButton.ArrowLeaved(Sender: TObject);
begin
  inherited MouseLeave;
end;

procedure TMenuButton.DelayedUnlock(Data: PtrInt);
begin
  if csDestroying in ComponentState then
    Exit;
  FUpdateLocked := False;
  if FStyle = mbsCombo then
    FArrowButton.FUpdateLocked := False;
end;

procedure TMenuButton.MenuClosed(Sender: TObject);
begin
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('MenuClosed');
  {$endif}
  if FStyle = mbsCombo then
  begin
    FArrowButton.FUpdateLocked := True;
    FArrowButton.ResetState
  end
  else
  begin
    FUpdateLocked := True;
    ResetState;
  end;
  Application.QueueAsyncCall(@DelayedUnlock, 0);
  {$ifdef DEBUG_MENUBUTTON}
  Logger.ExitMethod('MenuClosed');
  {$endif}
end;

procedure TMenuButton.SetMenu(const AValue: TPopupMenu);
begin
  FMenu := AValue;
  if FMenu <> nil then
    FMenu.OnClose := @MenuClosed;
end;

procedure TMenuButton.SetOptions(const AValue: TMenuButtonOptions);
begin
  if FOptions = AValue then
    Exit;
  {$ifdef FORCE_MOUSEUP}
  include(FOptions, mboPopupOnMouseUp);
  {$endif}
  FOptions := AValue;
  UpdateStyle;
end;

procedure TMenuButton.SetStyle(const AValue: TMenuButtonStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  if not (csLoading in ComponentState) then
    UpdateStyle;
end;

destructor TMenuButton.Destroy;
begin
  FArrowButton.Free;
  inherited Destroy;
end;

procedure TMenuButton.Click;
begin
  //skips Click event in mbsSingle style
  if FStyle = mbsCombo then
    inherited Click;
end;

{ TArrowButton }

procedure TArrowButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited;
  CalculateArrowPoints(0, AWidth, AHeight);
end;

procedure TArrowButton.DoButtonDown;
begin
  if FInternalDown and not (mboPopupOnMouseUp in MainButton.Options) then
    MainButton.ShowMenu;
end;

procedure TArrowButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FInternalDown and (Button = mbLeft) and (mboPopupOnMouseUp in MainButton.Options) then
    MainButton.ShowMenu;
end;

procedure TArrowButton.Paint;
begin
  inherited Paint;
  with Canvas do
  begin
    Brush.Color := clBlack;
    if FState = bsDown then
      Polygon([SuccPoint(FArrowPoints[acLeft]), SuccPoint(FArrowPoints[acRight]),
        SuccPoint(FArrowPoints[acBottom])])
    else
      Polygon([FArrowPoints[acLeft], FArrowPoints[acRight], FArrowPoints[acBottom]]);
  end;
end;

{ TToggleSpeedButton }

procedure TToggleSpeedButton.CalculateArrowPoints(XOffset, AWidth,
  AHeight: Integer);
var
  ArrowTop, ArrowWidth: Integer;
begin
  ArrowWidth := (AWidth - 1) div 2;
  if Odd(ArrowWidth) then
    Dec(ArrowWidth);
  ArrowTop := (AHeight - (ArrowWidth div 2)) div 2;
  with FArrowPoints[acLeft] do
  begin
    X := ((AWidth - ArrowWidth) div 2) - 1 + XOffset;
    Y := ArrowTop;
  end;
  with FArrowPoints[acRight] do
  begin
    X := FArrowPoints[acLeft].X + ArrowWidth;
    Y := ArrowTop;
  end;
  with FArrowPoints[acBottom] do
  begin
    X := FArrowPoints[acLeft].X + (ArrowWidth div 2);
    Y := ArrowTop + (ArrowWidth div 2);
  end;
end;

procedure TToggleSpeedButton.WMLButtonDown(var Message: TLMLButtonDown);
begin
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('WMLButton');
  {$endif}
  Include(FControlState, csClicked);
  if not FToggleMode then
  begin
    //Note: this calls TControl.WMLButtonDown and not TCustomSpeedButton one
    inherited WMLButtonDown(Message);
  end
  else
  begin
    if FUpdateLocked then
    begin
      FUpdateLocked := False;
      Exit;
    end;
    if Enabled then
    begin
      FInternalDown := not FInternalDown;
      {$ifdef DEBUG_MENUBUTTON}
      Logger.Send('FInternalDown',FInternalDown);
      {$endif}
      if (Action is TCustomAction) then
        TCustomAction(Action).Checked := FInternalDown;
      UpdateState(True);
      DoButtonDown;
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
  Logger.SendCallStack(Self.ClassName + '.Invalidate');
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
  FInternalDown := False;
  UpdateState(True);
end;

procedure TToggleSpeedButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
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

end.

