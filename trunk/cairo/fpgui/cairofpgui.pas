unit CairofpGui;

{
  Integrates Cairo with fpGui

  Copyright (C) 2008 Luiz Americo Pereira Camara
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
  Classes, SysUtils, CairoClasses, Cairo14, fpgfx, gfx_widget, gfxbase;

type

  { TCairoFpgCanvasSurface }

  TCairoFpgCanvasSurface = class(TCairoSurface)
  public
    constructor Create(Canvas: TfpgCanvas);
  end;

  { TCustomCairoControl }

  TCustomCairoControl = class(TfpgWidget)
  private
    FContext: TCairoContext;
    FOnCreateContext: TNotifyEvent;
  protected
    procedure DoCreateContext; virtual;
    procedure DoDraw; virtual; abstract;
    procedure HandlePaint; override;
    property Context: TCairoContext read FContext;
    property OnCreateContext: TNotifyEvent read FOnCreateContext write FOnCreateContext;
  public
    procedure Redraw;
  end;
  
  { TCairoPaintBox }

  TCairoPaintBox = class(TCustomCairoControl)
  private
    FOnDraw: TNotifyEvent;
  protected
    procedure DoDraw; override;
  public
    property Context;
  published
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
  end;

  function ColorToCairoColor(const AColor: TfpgColor): TCairoColor;
  function CreateSurfaceFromCanvas(Canvas: TfpgCanvas): Pcairo_surface_t;
  
implementation

{$ifdef windows}
{$i cairo_gdi.inc}
{$endif}

{$ifdef unix}
{$i cairo_x11.inc}
{$endif}

function ColorToCairoColor(const AColor: TfpgColor): TCairoColor;
begin
  Result := RGBToCairoColor(fpgColorToRGB(AColor));
end;

{ TCairoFpgCanvasSurface }

constructor TCairoFpgCanvasSurface.Create(Canvas: TfpgCanvas);
begin
  FHandle := CreateSurfaceFromCanvas(Canvas);
end;

{ TCustomCairoControl }

procedure TCustomCairoControl.Redraw;
begin
  //No special action necessary in fpGui
  Invalidate;
end;

procedure TCustomCairoControl.DoCreateContext;
begin
  if Assigned(FOnCreateContext) then
    FOnCreateContext(Self);
end;

procedure TCustomCairoControl.HandlePaint;
var
  Surface: Pcairo_surface_t;
begin
  Surface := CreateSurfaceFromCanvas(Canvas);
  FContext := TCairoContext.Create(Surface);
  cairo_surface_destroy(Surface);
  try
    Canvas.Color := BackgroundColor;
    Canvas.FillRectangle(0, 0, Width, Height);
    DoCreateContext;
    DoDraw;
  finally
    FreeAndNil(FContext);
  end;
end;

{ TCairoPaintBox }

procedure TCairoPaintBox.DoDraw;
begin
  if Assigned(FOnDraw) then
  begin
    FContext.Save;
    OnDraw(Self);
    FContext.Restore;
  end;
end;

end.

