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
  {$i uses.inc} cairo14,
  LCLIntf, Classes, SysUtils, LCLType, CairoClasses, Controls, Graphics, LMessages;

type

  { TCairoDCSurface }

  TCairoDCSurface = class(TCairoSurface)
  public
    constructor Create(DC: HDC);
  end;

  { TCairoControl }

  TCairoControl = class(TWinControl)
  private
    FBitmap: TBitmap;
    FContext: TCairoContext;
    FOnDraw: TNotifyEvent;
    procedure CreateContext;
    procedure InitBitmap;
  protected
    procedure DoDraw; virtual;
    procedure DoOnResize; override;
    procedure PaintWindow(DC: HDC); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
    property Bitmap: TBitmap read FBitmap;
    property Context: TCairoContext read FContext;
  published
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property Align;
    property BorderSpacing;
    property BorderStyle;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
function ColorToCairoColor(Color: TColor): TCairoColor; inline;

implementation

  {$i cairolcl.inc}

{ TCairoControl }

procedure TCairoControl.CreateContext;
var
  Surface: TCairoDCSurface;
begin
  FBitmap.Width := Width;
  FBitmap.Height := Height;
  InitBitmap;
  Surface := TCairoDCSurface.Create(FBitmap.Canvas.Handle);
  FContext := TCairoContext.Create(Surface);
  Surface.Destroy;
end;

procedure TCairoControl.InitBitmap;
begin
  FBitmap.Canvas.Brush.Color := Parent.Color;
  FBitmap.Canvas.FillRect(0, 0, Width, Height);
end;

procedure TCairoControl.DoDraw;
begin
  if Assigned(FOnDraw) then
  begin
    FContext.Save;
    OnDraw(Self);
    FContext.Restore;
  end;
end;

procedure TCairoControl.DoOnResize;
begin
  inherited DoOnResize;
  FreeAndNil(FContext);
end;

procedure TCairoControl.PaintWindow(DC: HDC);
begin
  BitBlt(DC, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TCairoControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // for now do nothing
end;

procedure TCairoControl.WMPaint(var Msg: TLMPaint);
begin
  if not Assigned(FContext) then
  begin
    CreateContext;
    DoDraw;
  end;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Msg);
  Exclude(FControlState, csCustomPaint);
end;

constructor TCairoControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
end;

destructor TCairoControl.Destroy;
begin
  FBitmap.Destroy;
  inherited Destroy;
end;

procedure TCairoControl.Redraw;
begin
  if not Assigned(FContext) then
    CreateContext;
  InitBitmap;
  DoDraw;
  Invalidate;
end;

{ TCairoDCSurface }

constructor TCairoDCSurface.Create(DC: HDC);
begin
  FHandle := CreateSurfaceFromDC(DC);
end;

end.

