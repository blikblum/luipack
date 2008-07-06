unit CairoLCL;

{
  Integrate cairo classes with LCL

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
  LCLIntf, Classes, SysUtils, LCLType, CairoClasses, Cairo14, Controls, Graphics, LMessages;

type

  { TCairoDCSurface }

  TCairoDCSurface = class(TCairoSurface)
  public
    constructor Create(DC: HDC);
  end;
  
  { TCairoLCLFont }

  TCairoLCLFont = class(TCairoFontFace)
  private
    FCharSet: TFontCharSet;
    FHeight: Integer;
    FName: String;
    FPitch: TFontPitch;
    FSize: Integer;
    FStyle: TFontStyles;
    FUpdateCount: Integer;
    procedure CreateHandle;
    procedure Changed;
    procedure HandleNeeded;
    procedure SetCharSet(const AValue: TFontCharSet);
    procedure SetHeight(const AValue: Integer);
    procedure SetName(const AValue: String);
    procedure SetPitch(const AValue: TFontPitch);
    procedure SetSize(const AValue: Integer);
    procedure SetStyle(const AValue: TFontStyles);
  protected
    function GetHandle: Pcairo_font_face_t; override;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property CharSet: TFontCharSet read FCharSet write SetCharSet default DEFAULT_CHARSET;
    property Height: Integer read FHeight write SetHeight;
    property Name: String read FName write SetName;
    property Pitch: TFontPitch read FPitch write SetPitch default fpDefault;
    property Size: Integer read FSize write SetSize stored false;
    property Style: TFontStyles read FStyle write SetStyle;
  end;

  { TCustomCairoControl }

  TCustomCairoControl = class(TWinControl)
  private
    FBitmap: TBitmap;
    FContext: TCairoContext;
    FOnCreateContext: TNotifyEvent;
  protected
    procedure DoCreateContext; virtual;
    procedure DoDraw; virtual; abstract;
    procedure DoOnResize; override;
    procedure InitBitmap; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    property Bitmap: TBitmap read FBitmap;
    property Context: TCairoContext read FContext;
    property OnCreateContext: TNotifyEvent read FOnCreateContext write FOnCreateContext;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
  end;
  
  { TCairoPaintBox }

  TCairoPaintBox = class(TCustomCairoControl)
  private
    FOnDraw: TNotifyEvent;
  protected
    procedure DoDraw; override;
  public
    property Bitmap;
    property Context;
  published
    constructor Create(AOwner: TComponent); override;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property Align;
    property BorderSpacing;
    property BorderStyle;
    property OnCreateContext;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
function ColorToCairoColor(Color: TColor): TCairoColor; inline;

implementation

{$i cairolcl.inc}

function ColorToCairoColor(Color: TColor): TCairoColor;
begin
  //LCL.ColorToRGB returns a BGR color
  Result := BGRToCairoColor(ColorToRGB(Color));
end;

{ TCustomCairoControl }

procedure TCustomCairoControl.DoCreateContext;
var
  Surface: Pcairo_surface_t;
begin
  FBitmap.Width := Width;
  FBitmap.Height := Height;
  Surface := CreateSurfaceFromDC(FBitmap.Canvas.Handle);
  FContext := TCairoContext.Create(Surface);
  cairo_surface_destroy(Surface);
  if Assigned(FOnCreateContext) then
    FOnCreateContext(Self);
end;

procedure TCustomCairoControl.InitBitmap;
begin
  FBitmap.Canvas.Brush.Color := Parent.Color;
  FBitmap.Canvas.FillRect(0, 0, Width, Height);
end;

procedure TCustomCairoControl.DoOnResize;
begin
  inherited DoOnResize;
  FreeAndNil(FContext);
end;

procedure TCustomCairoControl.PaintWindow(DC: HDC);
begin
  BitBlt(DC, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TCustomCairoControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // for now do nothing
end;

procedure TCustomCairoControl.WMPaint(var Msg: TLMPaint);
begin
  if not Assigned(FContext) then
  begin
    DoCreateContext;
    InitBitmap;
    DoDraw;
  end;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Msg);
  Exclude(FControlState, csCustomPaint);
end;

constructor TCustomCairoControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
end;

destructor TCustomCairoControl.Destroy;
begin
  FContext.Free;
  FBitmap.Destroy;
  inherited Destroy;
end;

procedure TCustomCairoControl.Redraw;
begin
  //sometimes the control can have 0 size
  if (Width <= 0) or (Height <= 0) then
    Exit;
  if not Assigned(FContext) then
    DoCreateContext;
  InitBitmap;
  DoDraw;
  Invalidate;
end;

{ TCairoDCSurface }

constructor TCairoDCSurface.Create(DC: HDC);
begin
  FHandle := CreateSurfaceFromDC(DC);
end;

{ TCairoPaintBox }

procedure TCairoPaintBox.DoDraw;
const
  Dash: Double = 2;
begin
  if csDesigning in ComponentState then
  begin
    Context.SetDash(@Dash, 1, 0);
    Context.Rectangle(0, 0, Width, Height);
    Context.Stroke;
    Exit;
  end;
  if Assigned(FOnDraw) then
  begin
    FContext.Save;
    OnDraw(Self);
    FContext.Restore;
  end;
end;

constructor TCairoPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 105, 105);
end;

{ TCairoLCLFont }

procedure TCairoLCLFont.SetHeight(const AValue: Integer);
begin
  FHeight := AValue;
  Changed;
end;


procedure TCairoLCLFont.Changed;
begin
  if FUpdateCount <= 0 then
  begin
    if FHandle <> nil then
    begin
      cairo_font_face_destroy(FHandle);
      FHandle := nil;
    end;
  end;
end;

procedure TCairoLCLFont.HandleNeeded;
begin
  if FHandle = nil then
    CreateHandle;
end;

procedure TCairoLCLFont.SetCharSet(const AValue: TFontCharSet);
begin
  FCharSet := AValue;
  Changed;
end;

procedure TCairoLCLFont.SetName(const AValue: String);
begin
  FName := AValue;
  Changed;
end;

procedure TCairoLCLFont.SetPitch(const AValue: TFontPitch);
begin
  FPitch := AValue;
  Changed;
end;

procedure TCairoLCLFont.SetSize(const AValue: Integer);
begin
  FSize := AValue;
  Changed;
end;

procedure TCairoLCLFont.SetStyle(const AValue: TFontStyles);
begin
  FStyle := AValue;
  Changed;
end;

function TCairoLCLFont.GetHandle: Pcairo_font_face_t;
begin
  HandleNeeded;
  Result := FHandle;
end;

procedure TCairoLCLFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCairoLCLFont.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

end.
