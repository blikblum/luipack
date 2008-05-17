unit JanaClock;

{

Port of http://svn.o-hand.com/repos/jana/trunk/libjana-gtk/jana-gtk-clock.c

/*
 *  Author: Chris Lord <chris@openedhand.com>
 *
 *  Copyright (c) 2007 OpenedHand Ltd - http://www.openedhand.com/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 */
 
  Adapted to LCL/fpGui by Luiz Américo
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses,
  {$ifdef FPGUI}
  CairofpGui, gfxbase
  {$else}
  Graphics, LCLType, CairoLCL
  {$endif};
  
type

  { TJanaClock }
  {$ifdef FPGUI}
  TJanaClock = class(TfpgCustomCairoControl)
  {$else}
  TJanaClock = class(TCustomCairoControl)
  {$endif}
  private
    FClockBuffer: TCairoSurface;
    FDigital: Boolean;
    FDrawShadow: Boolean;
    FShowSeconds: Boolean;
    FTime: TDateTime;
    procedure SetDigital(const AValue: Boolean);
    procedure SetDrawShadow(const AValue: Boolean);
    procedure SetShowSeconds(const AValue: Boolean);
    procedure SetTime(const AValue: TDateTime);
    procedure CreateClockBuffer;
    procedure UpdateClockBuffer;
  protected
    {$ifdef FPGUI}
    procedure HandleResize(awidth, aheight: TfpgCoord); override;
    {$else}
    procedure DoCreateContext; override;
    {$endif}
    procedure DoDraw; override;
    procedure DrawAnalogueClock(DrawContext: TCairoContext);
    procedure DrawAnalogueFace(DrawContext: TCairoContext; ATime: TDateTime);
    procedure DrawDigitalClock(DrawContext: TCairoContext);
    procedure DrawDigitalFace(DrawContext: TCairoContext; ATime: TDateTime);
    procedure DrawDigitalNumber(DrawContext: TCairoContext; Number: Integer; const ForegroundColor, BackColor: TCairoColor);
  public
    destructor Destroy; override;
    property Digital: Boolean read FDigital write SetDigital;
    property DrawShadow: Boolean read FDrawShadow write SetDrawShadow;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds;
    property Time: TDateTime read FTime write SetTime;
  end;

implementation

uses
  Math, cairo14;

{ TJanaClock }

procedure TJanaClock.SetDrawShadow(const AValue: Boolean);
begin
  if FDrawShadow=AValue then exit;
  FDrawShadow:=AValue;
  UpdateClockBuffer;
  Redraw;
end;

procedure TJanaClock.SetDigital(const AValue: Boolean);
begin
  if FDigital=AValue then exit;
  FDigital:=AValue;
  UpdateClockBuffer;
  Redraw;
end;

procedure TJanaClock.SetShowSeconds(const AValue: Boolean);
begin
  if FShowSeconds=AValue then exit;
  FShowSeconds:=AValue;
  Redraw;
end;

procedure TJanaClock.SetTime(const AValue: TDateTime);
begin
  if FTime=AValue then exit;
  FTime:=AValue;
  Redraw;
end;

procedure TJanaClock.CreateClockBuffer;
begin
  FClockBuffer := TCairoSurface.Create(Context.Target,
    CAIRO_CONTENT_COLOR_ALPHA, Width, Height);
end;

procedure TJanaClock.UpdateClockBuffer;
var
  BufferContext: TCairoContext;
begin
  BufferContext := TCairoContext.Create(FClockBuffer);
  if FDigital then
    DrawDigitalClock(BufferContext)
  else
    DrawAnalogueClock(BufferContext);
  BufferContext.Destroy;
end;

{$ifdef FPGUI}

procedure TJanaClock.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  FreeAndNil(FClockBuffer);
end;

{$else}

procedure TJanaClock.DoCreateContext;
begin
  inherited DoCreateContext;
  FreeAndNil(FClockBuffer);
end;
{$endif}

procedure TJanaClock.DoDraw;
begin
  if FClockBuffer = nil then
  begin
    CreateClockBuffer;
    UpdateClockBuffer;
  end;

  with Context do
  begin
    SetSourceSurface(FClockBuffer, 0, 0);
    Paint;
  end;

  if FDigital then
    DrawDigitalFace(Context, FTime)
  else
    DrawAnalogueFace(Context, FTime);
end;

procedure TJanaClock.DrawAnalogueClock(DrawContext: TCairoContext);
var
  Pattern: TCairoRadialGradient;
  awidth, aheight, size, thickness, i, shadow_radius: Integer;
  base_color, bg_color, fg_color: TCairoColor;
begin
  with DrawContext do
  begin
    (* Draw a Tango-style analogue clock face *)
    
    {$ifdef FPGUI}
    base_color := fpgColorToCairoColor(clSelection);
    bg_color := fpgColorToCairoColor(clWindowBackground);
    fg_color := fpgColorToCairoColor(clText1);
    {$else}
    base_color := ColorToCairoColor(clHighlight);
    bg_color := ColorToCairoColor(clWindow);
    fg_color := ColorToCairoColor(clWindowText);
    {$endif}

    awidth := Width;
    aheight := Height;
    if FDrawShadow then
      aheight := aheight - (aheight div 20); // aheight -= aheight/20;
    size := MIN (awidth, aheight);

    SetOperator (CAIRO_OPERATOR_CLEAR);
    Paint;
    SetOperator (CAIRO_OPERATOR_SOURCE);

    (* Draw shadow *)
    shadow_radius := MIN (awidth, Height) - size;
    if FDrawShadow and (shadow_radius > 0) then
    begin
      Save;
      Translate(awidth/2, (aheight/2) + (size/2));
      Scale (size / (shadow_radius*2), 1.0);
      NewPath;
      Arc (0, 0, shadow_radius, 0, 2 * PI);
      ClosePath;
      Pattern := TCairoRadialGradient.Create(0, 0,
              0, 0, 0, shadow_radius);
      Pattern.AddColorStopRgba (0, 0, 0, 0, 0.5);
      Pattern.AddColorStopRgba (1, 0, 0, 0, 0);
      Source := Pattern;
      Fill;
      Pattern.Destroy;
      Restore;
    end;

    (* Draw clock face *)
    thickness := size div 20;
    NewPath;
    Arc(awidth/2, aheight/2,
      size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    pattern := TCairoRadialGradient.Create (awidth/2, aheight/3,
      0, awidth/2, aheight/2,
      size/2 - thickness/2);
    Pattern.AddColorStopRgb (0, bg_color.Red*2, bg_color.Green*2, bg_color.Blue*2);
    Pattern.AddColorStop(0.3, bg_color);
    Pattern.AddColorStopRgb (1, bg_color.Red/1.15, bg_color.Green/1.15, bg_color.Blue/1.15);
    Source := Pattern;
    Fill;
    Pattern.Destroy;

    (* Draw tick marks *)
    Color := fg_color;
    for i := 0 to 3 do
    begin
      NewPath;
      Arc ((awidth/2) + ((size/2 - thickness/2 - size/6) * cos (i * PI/2)),
        (aheight/2) + ((size/2 - thickness/2 - size/6) * sin (i * PI/2)),
         size/40, 0, 2 * PI);
      ClosePath;
      Fill;
    end;

    (* Draw centre point *)
    NewPath;
    Arc(awidth/2, aheight/2, size/35, 0, 2 * PI);
    ClosePath;
    LineWidth := size/60;
    Stroke;

    (* Draw internal clock-frame shadow *)
    thickness := size div 20;
    NewPath;
    Arc(awidth/2, aheight/2,
      size/2 - thickness, 0, 2 * PI);
    ClosePath;
    Pattern := TCairoRadialGradient.Create ((awidth/2) - (size/4),
      (aheight/2) - (size/4),
      0, awidth/2, aheight/2,
      size/2 - thickness/2);
    Pattern.AddColorStopRgb (0, bg_color.Red/2,
            bg_color.Green/2, bg_color.Blue/2);
    Pattern.AddColorStopRgb (0.5, bg_color.Red/2,
            bg_color.Green/2, bg_color.Blue/2);
    Pattern.AddColorStopRgb (1, bg_color.Red*2,
            bg_color.Green*2, bg_color.Blue*2);
    Source := pattern;
    LineWidth := thickness;
    Stroke;
    Pattern.Destroy;

    (* Draw internal clock-frame *)
    NewPath;
    Arc(awidth/2, aheight/2,
      size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    pattern := TCairoRadialGradient.Create ((awidth/2) - (size/3),
      (aheight/2) - (size/3), 0, awidth/3, aheight/3, size/2);
    Pattern.AddColorStopRgb ( 0, base_color.Red*1.2,
      base_color.Green*1.2, base_color.Blue*1.2);
    Pattern.AddColorStop( 0.7, base_color);
    Pattern.AddColorStopRgb ( 1, base_color.Red/1.2,
      base_color.Green/1.2, base_color.Blue/1.2);
    Source := pattern;
    Stroke;
    Pattern.Destroy;

    (* Dark outline frame *)

    thickness := size div 60;
    NewPath;
    Arc (awidth/2, aheight/2,
      size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    SetSourceRGB (base_color.Red/2,
      base_color.Green/2, base_color.Blue/2);
    LineWidth := thickness;
    Stroke ;
    
    (* Draw less dark inner outline frame *)
    thickness := size div 40;
    NewPath;
    Arc(awidth/2, aheight/2, size/2 -
      (size/20)/2 - thickness, 0, 2 * PI);
    ClosePath ;
    SetSourceRgb(base_color.Red/1.5,
      base_color.Green/1.5, base_color.Blue/1.5);
    LineWidth := thickness;
    Stroke;
  end;
end;

procedure TJanaClock.DrawAnalogueFace(DrawContext: TCairoContext; ATime: TDateTime);
var
  pi_ratio: Double;
  awidth, aheight, size, thickness: Integer;
  Hour, Second, Minute, MSecond: Word;
begin
  with DrawContext do
  begin
    awidth := Width;
    aheight := Height;
    if FDrawShadow then
      Dec(aheight, aheight div 20);
    size := MIN (awidth, aheight);

    //gdk_cairo_set_source_color (cr, &style->fg[GTK_STATE_NORMAL]);
    //Color := ColorToCairoColor(clWindow);
    {$ifdef FPGUI}
    Color := fpgColorToCairoColor(clText1);
    {$else}
    Color := ColorToCairoColor(clWindowText);
    {$endif}
    LineJoin := CAIRO_LINE_JOIN_ROUND;
    LineWidth := MAX (1.5, size / 60);
    thickness := size div 20;

    DecodeTime(ATime, Hour,  Minute, Second, MSecond);

    (* Draw hour hand *)
    pi_ratio := (((Hour * 60) + Minute)/60.0)/6.0;
    NewPath;
    MoveTo ((awidth/2) + ((size/2 - thickness/2 - size/4) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) + ((size/2 - thickness/2 - size/4) * sin ((pi_ratio * PI)-(PI/2))));

    LineTo((awidth/2) + ((size/35) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) +((size/35) * sin ((pi_ratio * PI)-(PI/2))));
    ClosePath;
    Stroke;

    (* Draw minute hand *)
    pi_ratio := Minute/30.0;
    NewPath;
    MoveTo ((awidth/2) + ((size/2 - thickness/2 - size/8) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) + ((size/2 - thickness/2 - size/8) * sin ((pi_ratio * PI)-(PI/2))));
    LineTo((awidth/2) + ((size/35) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) +((size/35) * sin ((pi_ratio * PI)-(PI/2))));
    ClosePath;
    Stroke;

    if not FShowSeconds then
      Exit;
    (* Draw second hand *)
    {$ifdef FPGUI}
    Color := fpgColorToCairoColor(clSelection);
    {$else}
    Color := ColorToCairoColor(clHighlight);
    {$endif}
    LineWidth := MAX (1, size / 120);
    pi_ratio := Second/30.0;
    NewPath;
    MoveTo ((awidth/2) + ((size/2 - thickness/2 - size/8) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) + ((size/2 - thickness/2 - size/8) * sin ((pi_ratio * PI)-(PI/2))));
    LineTo((awidth/2) + ((size/35) * cos ((pi_ratio * PI)-(PI/2))),
      (aheight/2) +((size/35) * sin ((pi_ratio * PI)-(PI/2))));
    ClosePath;
    Stroke;
  end;
end;

procedure TJanaClock.DrawDigitalClock(DrawContext: TCairoContext);
var
  x, y, awidth, aheight, thickness, shadow_radius: Integer;
  base_color: TCairoColor;
  bg_color: TCairoColor;
  fg_color: TCairoColor;
  Pattern: TCairoRadialGradient;
begin
  (* Draw a Tango-style analogue clock face *)
  {$ifdef FPGUI}
  base_color := fpgColorToCairoColor(clSelection);
  bg_color := fpgColorToCairoColor(clWindowBackground);
  fg_color := fpgColorToCairoColor(clText1);
  {$else}
  base_color := ColorToCairoColor(clHighlight);
  bg_color := ColorToCairoColor(clWindow);
  fg_color := ColorToCairoColor(clWindowText);
  {$endif}
  with DrawContext do
  begin
    aheight := Height;
    awidth := Width;
    if FDrawShadow then
    begin
      Dec(aheight, aheight div 10);
      Dec(awidth, awidth div 10);
    end;
    awidth := MIN (awidth, aheight * 2);
    aheight := awidth div 2;
    x := (Width - awidth) div 2;
    y := (Height - aheight) div 2;

    SetOperator (CAIRO_OPERATOR_CLEAR);
    Paint;
    SetOperator (CAIRO_OPERATOR_SOURCE);

    Translate (x, y);

    if FDrawShadow then
    begin
      (* Draw ground shadow *)
      Save;
      Translate (awidth/2, aheight);
      Scale (1.0, (aheight/awidth)/10);

      NewPath;
      shadow_radius := ((10*awidth) div 9) div 2;
      Arc (0, 0, shadow_radius, 0, 2 * PI);
      ClosePath;
      Pattern := TCairoRadialGradient.Create (0, 0, 0,
        0, 0, shadow_radius);
      Pattern.AddColorStopRgba (0, 0, 0, 0, 0.5);
      Pattern.AddColorStopRgba (0.5, 0, 0, 0, 0.5);
      Pattern.AddColorStopRgba (1, 0, 0, 0, 0);
      Source := Pattern;
      Fill;
      Pattern.Destroy;

      Restore;
    end;

    (* Draw internal frame shadow *)
    thickness := awidth div 28;
    NewPath;
    Rectangle (thickness*2, thickness*2,
      awidth - thickness*4, aheight - thickness*4);
    pattern := TCairoRadialGradient.Create (
      awidth - thickness * 4, aheight - thickness * 4, 0,
      awidth - thickness * 4, aheight - thickness * 4, aheight);
    Pattern.AddColorStopRgb (0,
      (2*fg_color.Red+bg_color.Red)/3,
      (2*fg_color.Green+bg_color.Green)/3,
      (2*fg_color.Blue+bg_color.Blue)/3);
    Pattern.AddColorStop(0.5, fg_color);
    Pattern.AddColorStop (1, fg_color);
    Source := Pattern;
    LineWidth := thickness;
    Stroke;
    Pattern.Destroy;

    (* Draw clock face *)
    NewPath;
    Rectangle (thickness*2.5, thickness*2.5,
      awidth - thickness*5, aheight - thickness*5);
    LineJoin := CAIRO_LINE_JOIN_ROUND;
    LineCap := CAIRO_LINE_CAP_ROUND;
    SetSourceRgb (
      (15*fg_color.Red+bg_color.Red)/16,
      (15*fg_color.Green+bg_color.Green)/16,
      (15*fg_color.Blue+bg_color.Blue)/16);
    LineWidth := thickness/2;
    StrokePreserve;
    Fill;

    (* Draw dark outline frame *)
    NewPath;
    Rectangle (thickness/2, thickness/2,
      awidth - thickness, aheight - thickness);
    LineWidth := thickness;
    SetSourceRgb (base_color.Red/2,
      base_color.Green/2, base_color.Blue/2);
    Stroke;

    (* Draw main outline frame *)
    NewPath;
    Rectangle (thickness, thickness,
      awidth - thickness*2, aheight - thickness*2);
    Pattern := TCairoRadialGradient.Create (thickness/2,
      thickness/2, 0, thickness/2, thickness/2, awidth);
    Pattern.AddColorStopRgb (0, base_color.Red*1.2,
      base_color.Green*1.2, base_color.Blue*1.2);
    Pattern.AddColorStop (0.7, base_color);
    Pattern.AddColorStopRgb (1, base_color.Red/1.2,
      base_color.Green/1.2, base_color.Blue/1.2);
    Source := Pattern;
    Stroke;
    Pattern.Destroy;

    (* Draw less dark inner outline frame *)
    NewPath;
    Rectangle (thickness*1.5, thickness*1.5,
      awidth - thickness*3, aheight - thickness*3);
    SetSourceRgb (base_color.Red/1.5,
      base_color.Green/1.5, base_color.Blue/1.5);
    LineWidth := thickness/2;
    Stroke;
  end;
end;

procedure TJanaClock.DrawDigitalFace(DrawContext: TCairoContext; ATime: TDateTime);
var
  x, y, awidth, aheight, thickness: Integer;
  bg_color: TCairoColor;
  fg_color: TCairoColor;
  Hour, Second, Minute, MSecond: Word;
begin
  {$ifdef FPGUI}
  bg_color := fpgColorToCairoColor(clText1);
  fg_color := fpgColorToCairoColor(clWindowBackground);
  {$else}
  bg_color := ColorToCairoColor(clWindowText);
  fg_color := ColorToCairoColor(clWindow);
  {$endif}
  with DrawContext do
  begin
    Save;
    aheight := Height;
    awidth := Width;
    if FDrawShadow then
    begin
      Dec(aheight, aheight div 10);
      Dec(awidth, awidth div 10);
    end;
    awidth := MIN (awidth, aheight * 2);
    aheight := awidth div 2;
    x := (Width - awidth) div 2;
    y := (Height - aheight) div 2;

    thickness := awidth div 28;

    Translate (x + thickness*3, y + thickness*3);
    Scale ((awidth - thickness*6)/5.0,(aheight - thickness*6));

    DecodeTime(ATime, Hour, Minute, Second, MSecond);

    DrawDigitalNumber(DrawContext, Hour div 10, fg_color, bg_color);
    Translate (1.1, 0);
    DrawDigitalNumber(DrawContext, Hour Mod 10, fg_color, bg_color);
    Translate (1.1, 0);

    (* Draw separator *)
    if FShowSeconds and ((Second mod 2) = 1) then
      Color := bg_color
    else
      Color := fg_color;
    NewPath;
    Rectangle (0.15, 2.0/8.0, 0.3, 1.0/8.0);
    Rectangle (0.15, 5.0/8.0, 0.3, 1.0/8.0);
    Fill;

    Translate (0.7, 0);
    DrawDigitalNumber(DrawContext, Minute div 10, fg_color, bg_color);
    Translate (1.1, 0);
    DrawDigitalNumber(DrawContext, Minute mod 10, fg_color, bg_color);
    Restore;
  end;
end;

procedure TJanaClock.DrawDigitalNumber(DrawContext: TCairoContext; Number: Integer; const ForegroundColor,
  BackColor: TCairoColor);
var
  Padding: Double;
begin
  padding := 0.01;
  with DrawContext do
  begin
    (* Draw a segmented number, like on an old digital LCD display *)

    (* Top *)
    if number in  [0, 2, 3, 5, 6, 7, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;
    NewPath;
    MoveTo(0, 0);
    LineTo(8.0/8.0, 0);
    LineTo (4.0/5.0 - padding, 1.0/8.0 - padding);
    LineTo (1.0/5.0 + padding, 1.0/8.0 - padding);
    ClosePath;
    Fill;

    (* Top-left *)
    if number in  [0, 4, 5, 6, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;
    NewPath;
    MoveTo(0, 0);
    LineTo (0, 4.0/8.0);
    LineTo ( 1.0/5.0 - padding, 3.5/8.0 - padding);
    LineTo ( 1.0/5.0 - padding, 1.0/8.0 + padding);
          ClosePath;
    Fill;

    (* Top-right *)
    if number in  [0, 1, 2, 3, 4, 7, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;

    NewPath;
    MoveTo (5.0/5.0, 0);
    LineTo (5.0/5.0, 4.0/8.0);
    LineTo ( 4.0/5.0 + padding, 3.5/8.0 - padding);
    LineTo (4.0/5.0 + padding, 1.0/8.0 + padding);
    ClosePath;
    Fill;

    (* Middle *)
    if number in  [2, 3, 4, 5, 6, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;

    NewPath;
    MoveTo (0, 4.0/8.0);
    LineTo (1.0/5.0 + padding, 3.5/8.0 + padding);
    LineTo (4.0/5.0 - padding, 3.5/8.0 + padding);
    LineTo (5.0/5.0, 4.0/8.0);
    LineTo (4.0/5.0 - padding, 4.5/8.0 - padding);
    LineTo (1.0/5.0 + padding, 4.5/8.0 - padding);
    ClosePath;
    Fill;

    (* Bottom-left *)
    if number in  [0, 2, 6, 8] then
      Color := ForegroundColor
    else
      Color := BackColor;

    NewPath;
    MoveTo (0, 4.0/8.0);
    LineTo (0, 8.0/8.0);
    LineTo (1.0/5.0 - padding, 7.0/8.0 - padding);
    LineTo (1.0/5.0 - padding, 4.5/8.0 + padding);
    ClosePath;
    Fill;

    (* Bottom-right *)

    if number in  [0, 1, 3, 4, 5, 6, 7, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;

    NewPath;
    MoveTo (5.0/5.0, 4.0/8.0);
    LineTo (5.0/5.0, 8.0/8.0);
    LineTo (4.0/5.0 + padding, 7.0/8.0 - padding);
    LineTo (4.0/5.0 + padding, 4.5/8.0 + padding);
    ClosePath;
    Fill;

    (* Bottom *)
    if number in  [0, 2, 3, 5, 6, 8, 9] then
      Color := ForegroundColor
    else
      Color := BackColor;

    NewPath;
    MoveTo (0, 8.0/8.0);
    LineTo (5.0/5.0, 8.0/8.0);
    LineTo ( 4.0/5.0 - padding, 7.0/8.0 + padding);
    LineTo (1.0/5.0 + padding, 7.0/8.0 + padding);
    ClosePath;
    Fill;
  end;
end;

destructor TJanaClock.Destroy;
begin
  FClockBuffer.Free;
  inherited Destroy;
end;

end.

