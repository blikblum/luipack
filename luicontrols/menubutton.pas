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
interface


uses
  Forms, Classes, SysUtils, Buttons, Menus, LMessages, Controls, ActnList, Graphics {$ifdef DEBUG_MENUBUTTON}, sharedlogger {$endif};

type

  TMenuButton = class;

  { TToggleSpeedButton }

  TToggleSpeedButton = class (TCustomSpeedButton)
  private
    FInternalDown: Boolean;
    FToggleMode: Boolean;
    FUpdateLocked: Boolean;
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
    procedure DoButtonDown; virtual; abstract;
  public
    property ToggleMode: Boolean read FToggleMode write FToggleMode;
  end;

  { TArrowButton }

  TArrowButton = class (TToggleSpeedButton)
  private
    FMainButton: TMenuButton;
    FUpperLeft: TPoint;
    FUpperRight: TPoint;
    FBottom: TPoint;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  protected
    procedure DoButtonDown; override;
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
    FShowArrow: Boolean;
    procedure ArrowEntered(Sender: TObject);
    procedure ArrowLeaved(Sender: TObject);
    procedure DelayedUnlock(Data: PtrInt);
    procedure MenuClosed(Sender: TObject);
    procedure SetMenu(const AValue: TPopupMenu);
    procedure SetShowArrow(const AValue: Boolean);
    procedure ShowMenu;
    procedure UpdateArrowState;
  protected
    procedure DoButtonDown; override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    destructor Destroy; override;
    procedure Click; override;
  published
    property Menu: TPopupMenu read FMenu write SetMenu;
    property ShowArrow: Boolean read FShowArrow write SetShowArrow;
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

procedure TMenuButton.UpdateArrowState;
var
  ArrowWidth: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  if FShowArrow then
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

procedure TMenuButton.DelayedUnlock(Data: PtrInt);
begin
  if csDestroying in ComponentState then
    Exit;
  FUpdateLocked := False;
  if FShowArrow then
    FArrowButton.FUpdateLocked := False;
end;

procedure TMenuButton.MenuClosed(Sender: TObject);
begin
  {$ifdef DEBUG_MENUBUTTON}
  Logger.EnterMethod('MenuClosed');
  {$endif}
  if FShowArrow then
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

procedure TMenuButton.SetShowArrow(const AValue: Boolean);
begin
  if AValue <> FShowArrow then
  begin
    FShowArrow := AValue;
    UpdateArrowState;
  end;
end;

destructor TMenuButton.Destroy;
begin
  FArrowButton.Free;
  inherited Destroy;
end;

procedure TMenuButton.Click;
begin
  //skips Click event when there's no arrow
  if FShowArrow then
    inherited Click;
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

procedure TArrowButton.DoButtonDown;
begin
  if FInternalDown then
    MainButton.ShowMenu;
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

