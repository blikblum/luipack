unit ToggleLabel;

{
  Implements TToggleLabel

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

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LMessages;

type

  TChangingEvent = procedure(Sender: TObject; var Allow: Boolean) of object;

  { TToggleLabel }

  TToggleLabel = class (TCustomLabel)
  private
    FExpanded: Boolean;
    FExpandedCaption: String;
    FMouseInControl: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TChangingEvent;
    FTextOffset: Integer;
    function ChangeAllowed: Boolean;
    procedure SetExpanded(const AValue: Boolean);
    procedure SetExpandedCaption(const AValue: String);
  protected
    procedure DoMeasureTextPosition(var TextTop: integer;
      var TextLeft: integer); override;
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
    property OnChanging: TChangingEvent read FOnChanging write FOnChanging;
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

function TToggleLabel.ChangeAllowed: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
end;

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

procedure TToggleLabel.DoMeasureTextPosition(var TextTop: integer;
  var TextLeft: integer);
begin
  inherited DoMeasureTextPosition(TextTop, TextLeft);
  Inc(TextLeft, FTextOffset);
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
    if ChangeAllowed then
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
    //todo: Update only the glyph area ??
    if Enabled then
      Invalidate;
  end;
end;

procedure TToggleLabel.WMLButtonDown(var Message: TLMLButtonDown);
begin
  if not ChangeAllowed then
  begin
    //todo: paint the arrow not hilighted here.
    //Create a PaintArrow function
    //Invalidate;
    Exit;
  end;
  FExpanded := not FExpanded;
  if Assigned(FOnChange) then
    FOnChange(Self);
  TextChanged;
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

begin
  inherited Paint;
  with Canvas do
  begin
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

