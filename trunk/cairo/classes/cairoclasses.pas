unit CairoClasses;

{
  Wrapper classes to cairo

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
  cairo14;
  
type

  TCairoColor = record
    Red: Double;
    Green: Double;
    Blue: Double;
    Alpha: Double;
  end;
  
  
  { TCairoSurface }

  TCairoSurface = class
  private
  protected
    FHandle: Pcairo_surface_t;
  public
    constructor Create(Other: TCairoSurface; Content: cairo_content_t; Width, Height: LongInt);
    destructor Destroy; override;
    procedure Finish;
    function  Status: cairo_status_t;
    function  GetType: cairo_surface_type_t;
    function  GetContent: cairo_content_t;
    function  WriteToPng(Filename: String): cairo_status_t;
    function  WriteToPngStream(WriteFunc: cairo_write_func_t; closure: Pointer): cairo_status_t;
    function  GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; DestroyFunc: cairo_destroy_func_t): cairo_status_t;
    procedure GetFontOptions(Options: Pcairo_font_options_t);
    procedure Flush;
    procedure MarkDirty;
    procedure MarkDirtyRectangle(X, Y, Width, Height: LongInt);
    procedure SetDeviceOffset(XOffset, YOffset: Double);
    procedure GetDeviceOffset(XOffset, YOffset: PDouble);
    procedure SetFallbackResolution(XPixelsPerInch, YPixelsPerInch: Double);
  end;
  
  { TCairoPattern }

  TCairoPattern = class
  private
    FHandle: Pcairo_pattern_t;
  public
    destructor Destroy; override;
  end;
  
  { TCairoSolidPattern }

  TCairoSolidPattern = class (TCairoPattern)
  public
    constructor Create(Red, Green, Blue: Double);
    constructor Create(Red, Green, Blue, Alpha: Double);
  end;
  
  { TCairoGradient }

  TCairoGradient = class (TCairoPattern)
  public
    procedure AddColorStopRgb(Offset, Red, Green, Blue: Double);
    procedure AddColorStopRgba(Offset, Red, Green, Blue, Alpha: Double);
    function  GetColorStopRgba(Index: LongInt; Offset, Red, Green, Blue, Alpha: PDouble): cairo_status_t;
    function  GetColorStopCount(Count: PLongInt): cairo_status_t;
  end;
  
  { TCairoLinearGradient }

  TCairoLinearGradient = class (TCairoGradient)
  public
    constructor Create(X0, Y0, X1, Y1: Double);
  end;

  { TCairoRadialGradient }

  TCairoRadialGradient = class (TCairoGradient)
  public
    constructor Create(Cx0, Cy0, Radius0, Cx1, Cy1, Radius1: Double);
  end;

  { TCairoSurfacePattern }

  TCairoSurfacePattern = class (TCairoPattern)
  public
    constructor Create(Surface: TCairoSurface);
  end;

  { TCairoRectangleList }

  TCairoRectangleList = class
    FNumRectangles: Integer;
  private
    FRectangleList: Pcairo_rectangle_list_t;
    function GetNumRectangles: Integer;
    function GetRectangles(Index: Integer): cairo_rectangle_t;
    function GetStatus: cairo_status_t;
  public
    destructor Destroy; override;
    property Status: cairo_status_t read GetStatus;
    property Rectangles[Index: Integer]: cairo_rectangle_t read GetRectangles;
    property NumRectangles: Integer read GetNumRectangles;
  end;
  

  { TCairoFontOptions }

  TCairoFontOptions = class
  private
    FHandle: Pcairo_font_options_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Copy(Original: TCairoFontOptions);
    function  Status: cairo_status_t;
    procedure Merge(Other: TCairoFontOptions);
    function  Equal(Other: TCairoFontOptions): Boolean;
    function  Hash: LongWord;
    procedure SetAntialias(Antialias: cairo_antialias_t);
    function  GetAntialias: cairo_antialias_t;
    procedure SetSubpixelOrder(SubpixelOrder: cairo_subpixel_order_t);
    function  GetSubpixelOrder: cairo_subpixel_order_t;
    procedure SetHintStyle(HintStyle: cairo_hint_style_t);
    function  GetHintStyle: cairo_hint_style_t;
    procedure SetHintMetrics(HintMetrics: cairo_hint_metrics_t);
    function  GetHintMetrics: cairo_hint_metrics_t;
  end;

  { TCairoFontFace }

  TCairoFontFace = class
  private
    FHandle: Pcairo_font_face_t;
  public
    function  Status: cairo_status_t;
    function  GetType: cairo_font_type_t;
    function  GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
  end;

  { TCairoScaledFont }

  TCairoScaledFont = class
  private
    FHandle: Pcairo_scaled_font_t;
  public
    constructor Create(FontFace: TCairoFontFace; FontMatrix, Ctm: Pcairo_matrix_t; Options: TCairoFontOptions);
    destructor Destroy; override;
    function  Status: cairo_status_t;
    function  GetType: cairo_font_type_t;
    function  GetUserData (Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData (Key: Pcairo_user_data_key_t; UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
    procedure Extents(AExtents: Pcairo_font_extents_t);
    procedure TextExtents(Utf8: PChar; AExtents: Pcairo_text_extents_t);
    procedure GlyphExtents(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt; AExtents: Pcairo_text_extents_t);
    function  GetFontFace: Pcairo_font_face_t;
    procedure GetFontMatrix(FontMatrix: Pcairo_matrix_t);
    procedure GetCtm(Ctm: Pcairo_matrix_t);
    procedure GetFontOptions (Options: Pcairo_font_options_t);
  end;


  { TCairoContext }

  TCairoContext = class
  private
    FHandle: Pcairo_t;
    FFontFace: TCairoFontFace;
  public
    constructor Create(Target: TCairoSurface);
    destructor Destroy; override;
    function  GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; DestroyFunc: cairo_destroy_func_t): cairo_status_t;
    procedure Save;
    procedure Restore;
    procedure PushGroup;
    procedure PushGroupWithContent(content: cairo_content_t);
    function  PopGroup: TCairoPattern;
    procedure PopGroupToSource;
    procedure SetOperator(Op: cairo_operator_t);
    procedure SetSource(Source: TCairoPattern);
    procedure SetSourceRgb(Red, Green, Blue: Double);
    procedure SetSourceRgba(Red, Green, Blue, Alpha: Double);
    procedure SetSourceSurface(Surface: TCairoSurface; X, Y: Double);
    procedure SetTolerance(Tolerance: Double);
    procedure SetAntialias(Antialias: cairo_antialias_t);
    procedure SetFillRule(FillRule: cairo_fill_rule_t);
    procedure SetLineWidth(Width: Double);
    procedure SetLineCap(LineCap: cairo_line_cap_t);
    procedure SetLineJoin(LineJoin: cairo_line_join_t);
    procedure SetDash(Dashes: PDouble; NumDashes: LongInt; Offset: Double);
    procedure SetMiterLimit(Limit: Double);
    procedure Translate(Tx, Ty: Double);
    procedure Scale(Sx, Sy: Double);
    procedure Rotate(Angle: Double);
    procedure Transform(Matrix: Pcairo_matrix_t);
    procedure SetMatrix(Matrix: Pcairo_matrix_t);
    procedure IdentityMatrix;
    procedure UserToDevice(X, Y: PDouble);
    procedure UserToDeviceDistance(Dx, Dy: PDouble);
    procedure DeviceToUser(X, Y:PDouble);
    procedure DeviceToUserDistance(Dx, Dy: PDouble);
    procedure NewPath;
    procedure MoveTo(X, Y: Double);
    procedure NewSubPath;
    procedure LineTo(X, Y: Double);
    procedure CurveTo(X1, Y1, X2, Y2, X3, Y3: Double);
    procedure Arc(Xc, Yc, Radius, Angle1, Angle2: Double);
    procedure ArcNegative(Xc, Yc, Radius, Angle1, Angle2: Double);
    procedure RelMoveTo(Dx, Dy: Double);
    procedure RelLineTo(Dx, Dy: Double);
    procedure RelCurveTo(Dx1, Dy1, Dx2, Dy2, Dx3, Dy3: Double);
    procedure Rectangle(X, Y, Width, Height: Double);
    procedure ClosePath;
    procedure Paint;
    procedure PaintWithAlpha(Alpha: Double);
    procedure Mask(Pattern: TCairoPattern);
    procedure MaskSurface(Surface: TCairoSurface; SurfaceX, SurfaceY: Double);
    procedure Stroke;
    procedure StrokePreserve;
    procedure Fill;
    procedure FillPreserve;
    procedure CopyPage;
    procedure ShowPage;
    function  InStroke(X, Y: Double): Boolean;
    function  InFill(X, Y: Double): Boolean;
    procedure StrokeExtents(X1, Y1, X2, Y2: PDouble);
    procedure FillExtents(X1, Y1, X2, Y2: PDouble);
    procedure ResetClip;
    procedure Clip;
    procedure ClipPreserve;
    procedure ClipExtents(X1, Y1, X2, Y2:  PDouble);
    function  CopyClipRectangleList: TCairoRectangleList;
    procedure SelectFontFace(const Family: String; Slant: cairo_font_slant_t; Weight: cairo_font_weight_t);
    procedure SetFontSize(Size: Double);
    procedure SetFontMatrix(Matrix: Pcairo_matrix_t);
    procedure GetFontMatrix(Matrix: Pcairo_matrix_t);
    procedure SetFontOptions(Options: Pcairo_font_options_t);
    procedure GetFontOptions(Options: Pcairo_font_options_t);
    procedure SetFontFace(FontFace: Pcairo_font_face_t);
    function  GetFontFace: TCairoFontFace;
    procedure SetScaledFont(ScaledFont:Pcairo_scaled_font_t);
    function  GetScaledFont: Pcairo_scaled_font_t;
    procedure ShowText(const Text: String);
    procedure ShowGlyphs(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
    procedure TextPath(const Text: String);
    procedure GlyphPath(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
    procedure TextExtents(const Text: String; Extents: Pcairo_text_extents_t);
    procedure GlyphExtents(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt; Extents: Pcairo_text_extents_t);
    procedure FontExtents(Extents: Pcairo_font_extents_t);
  end;
  
  function RGBToCairoColor(R, G, B: Byte): TCairoColor;
  function RGBToCairoColor(RGB: Cardinal): TCairoColor;

implementation

function RGBToCairoColor(R, G, B: Byte): TCairoColor;
begin
  Result.Red := R/255;
  Result.Green := G/255;
  Result.Blue := B/255;
  Result.Alpha := 1;
end;

function RGBToCairoColor(RGB: Cardinal): TCairoColor;
begin
  Result.Red := (RGB and $000000ff)/255;
  Result.Green := ((RGB shr 8) and $000000ff)/255;
  Result.Blue := ((RGB shr 16) and $000000ff)/255;
  Result.Alpha := 1;
end;

{ TCairoContext }


procedure TCairoContext.SetOperator(Op: cairo_operator_t);
begin
  cairo_set_operator(FHandle, Op);
end;

procedure TCairoContext.SetSource(Source: TCairoPattern);
begin
  cairo_set_source(FHandle, Source.FHandle);
end;

procedure TCairoContext.SetSourceRgb(Red, Green, Blue: Double);
begin
  cairo_set_source_rgb(FHandle, Red, Green, Blue);
end;

procedure TCairoContext.SetSourceRgba(Red, Green, Blue, Alpha: Double);
begin
  cairo_set_source_rgba(FHandle, Red, Green, Blue, Alpha);
end;

procedure TCairoContext.SetSourceSurface(Surface: TCairoSurface; X, Y: Double);
begin
  cairo_set_source_surface(FHandle, Surface.FHandle, X, Y);
end;

procedure TCairoContext.SetTolerance(Tolerance: Double);
begin
  cairo_set_tolerance(FHandle, Tolerance);
end;

procedure TCairoContext.SetAntialias(Antialias: cairo_antialias_t);
begin
  cairo_set_antialias(FHandle, Antialias);
end;

procedure TCairoContext.SetFillRule(FillRule: cairo_fill_rule_t);
begin
  cairo_set_fill_rule(FHandle, FillRule);
end;

procedure TCairoContext.SetLineWidth(Width: Double);
begin
  cairo_set_line_width(FHandle, Width);
end;

procedure TCairoContext.SetLineCap(LineCap: cairo_line_cap_t);
begin
  cairo_set_line_cap(FHandle, LineCap);
end;

procedure TCairoContext.SetLineJoin(LineJoin: cairo_line_join_t);
begin
  cairo_set_line_join(FHandle, LineJoin);
end;

procedure TCairoContext.SetDash(Dashes: PDouble; NumDashes: longint;
  Offset: Double);
begin
  cairo_set_dash(FHandle, Dashes, NumDashes, Offset);
end;

procedure TCairoContext.SetMiterLimit(Limit: Double);
begin
  cairo_set_miter_limit(FHandle, Limit);
end;

procedure TCairoContext.Translate(Tx, Ty: Double);
begin
  cairo_translate(FHandle, TX, Ty);
end;

procedure TCairoContext.Scale(Sx, Sy: Double);
begin
  cairo_scale(FHandle, Sx, Sy);
end;

procedure TCairoContext.Rotate(Angle: Double);
begin
  cairo_rotate(FHandle, Angle);
end;

procedure TCairoContext.Transform(Matrix: Pcairo_matrix_t);
begin
  cairo_transform(FHandle, Matrix);
end;

procedure TCairoContext.SetMatrix(Matrix: Pcairo_matrix_t);
begin
  cairo_set_matrix(FHandle, Matrix);
end;

procedure TCairoContext.IdentityMatrix;
begin
  cairo_identity_matrix(FHandle);
end;

procedure TCairoContext.UserToDevice(X, Y: PDouble);
begin
  cairo_user_to_device(FHandle, X, Y);
end;

procedure TCairoContext.UserToDeviceDistance(Dx, Dy: PDouble);
begin
  cairo_user_to_device_distance(FHandle, Dx, Dy);
end;

procedure TCairoContext.DeviceToUser(X, Y: PDouble);
begin
  cairo_device_to_user(FHandle, X, Y);
end;

procedure TCairoContext.DeviceToUserDistance(Dx, Dy: PDouble);
begin
  cairo_device_to_user_distance(FHandle, Dx, Dy);
end;

procedure TCairoContext.NewPath;
begin
  cairo_new_path(FHandle);
end;

procedure TCairoContext.MoveTo(X, Y: Double);
begin
  cairo_move_to(FHandle, X, Y);
end;

procedure TCairoContext.NewSubPath;
begin
  cairo_new_sub_path(FHandle);
end;

procedure TCairoContext.LineTo(X, Y: Double);
begin
  cairo_line_to(FHandle, X, Y);
end;

procedure TCairoContext.CurveTo(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  cairo_curve_to(FHandle, X1, Y1, X2, Y2, X3, Y3);
end;

procedure TCairoContext.Arc(Xc, Yc, Radius, Angle1, Angle2: Double);
begin
  cairo_arc(FHandle, Xc, Yc, Radius, Angle1, Angle2);
end;

procedure TCairoContext.ArcNegative(Xc, Yc, Radius, Angle1, Angle2: Double);
begin
  cairo_arc_negative(FHandle, Xc, Yc, Radius, Angle1, Angle2);
end;

procedure TCairoContext.RelMoveTo(Dx, Dy: Double);
begin
  cairo_rel_move_to(FHandle, Dx, Dy);
end;

procedure TCairoContext.RelLineTo(Dx, Dy: Double);
begin
  cairo_rel_line_to(FHandle, Dx, Dy);
end;

procedure TCairoContext.RelCurveTo(Dx1, Dy1, Dx2, Dy2, Dx3, Dy3: Double);
begin
  cairo_rel_curve_to(FHandle, Dx1, Dy1, Dx2, Dy2, Dx3, Dy3);
end;

procedure TCairoContext.Rectangle(X, Y, Width, Height: Double);
begin
  cairo_rectangle(FHandle, X, Y, Width, Height);
end;

procedure TCairoContext.ClosePath;
begin
  cairo_close_path(FHandle) ;
end;

procedure TCairoContext.Paint;
begin
  cairo_paint(FHandle);
end;

procedure TCairoContext.PaintWithAlpha(Alpha: Double);
begin
  cairo_paint_with_alpha(FHandle, Alpha);
end;

procedure TCairoContext.Mask(Pattern: TCairoPattern);
begin
  cairo_mask(FHandle, Pattern.FHandle);
end;

procedure TCairoContext.MaskSurface(Surface: TCairoSurface; SurfaceX,
  SurfaceY: Double);
begin
  cairo_mask_surface(FHandle, Surface.FHandle, SurfaceX, SurfaceY);
end;

procedure TCairoContext.Stroke;
begin
  cairo_stroke(FHandle);
end;

procedure TCairoContext.StrokePreserve;
begin
  cairo_stroke_preserve(FHandle);
end;

procedure TCairoContext.Fill;
begin
  cairo_fill(FHandle);
end;

procedure TCairoContext.FillPreserve;
begin
  cairo_fill_preserve(FHandle);
end;

procedure TCairoContext.CopyPage;
begin
  cairo_copy_page(FHandle);
end;

procedure TCairoContext.ShowPage;
begin
  cairo_show_page(FHandle);
end;

function TCairoContext.InStroke(X, Y: Double): Boolean;
begin
  Result := Boolean(cairo_in_stroke(FHandle, X, Y));
end;

function TCairoContext.InFill(X, Y: Double): Boolean;
begin
  Result := Boolean(cairo_in_fill(FHandle, X, Y));
end;

procedure TCairoContext.StrokeExtents(X1, Y1, X2, Y2: PDouble);
begin
  cairo_stroke_extents(FHandle, X1, Y1, X2, Y2);
end;

procedure TCairoContext.FillExtents(X1, Y1, X2, Y2: PDouble);
begin
  cairo_fill_extents(FHandle, X1, Y1, X2, Y2);
end;

procedure TCairoContext.ResetClip;
begin
  cairo_reset_clip(FHandle);
end;

procedure TCairoContext.Clip;
begin
  cairo_clip(FHandle);
end;

procedure TCairoContext.ClipPreserve;
begin
  cairo_clip_preserve(FHandle);
end;

procedure TCairoContext.ClipExtents(X1, Y1, X2, Y2: PDouble);
begin
  cairo_clip_extents(FHandle, X1, Y1, X2, Y2);
end;

function TCairoContext.CopyClipRectangleList: TCairoRectangleList;
begin
  Result := TCairoRectangleList.Create;
  Result.FRectangleList := cairo_copy_clip_rectangle_list(FHandle);
end;

procedure TCairoContext.SelectFontFace(const Family: String;
  Slant: cairo_font_slant_t; Weight: cairo_font_weight_t);
begin
  cairo_select_font_face(FHandle, PChar(Family), Slant, Weight);
end;

procedure TCairoContext.SetFontSize(Size: Double);
begin
  cairo_set_font_size(FHandle, Size);
end;

procedure TCairoContext.SetFontMatrix(Matrix: Pcairo_matrix_t);
begin
  cairo_set_font_matrix(FHandle, Matrix);
end;

procedure TCairoContext.GetFontMatrix(Matrix: Pcairo_matrix_t);
begin
  cairo_get_font_matrix(FHandle, Matrix);
end;

procedure TCairoContext.SetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_set_font_options(FHandle, Options);
end;

procedure TCairoContext.GetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_get_font_options(FHandle, Options);
end;

procedure TCairoContext.SetFontFace(FontFace: Pcairo_font_face_t);
begin
  cairo_set_font_face(FHandle, FontFace);
end;

function TCairoContext.GetFontFace: TCairoFontFace;
begin
  if FFontFace = nil then
  begin
    FFontFace := TCairoFontFace.Create;
    FFontFace.FHandle := cairo_get_font_face(FHandle);
  end;
  Result := FFontFace;
end;

procedure TCairoContext.SetScaledFont(ScaledFont: Pcairo_scaled_font_t);
begin
  cairo_set_scaled_font(FHandle, ScaledFont);
end;

function TCairoContext.GetScaledFont: Pcairo_scaled_font_t;
begin
  Result := cairo_get_scaled_font(FHandle);
end;

procedure TCairoContext.ShowText(const Text: String);
begin
  cairo_show_text(FHandle, PChar(Text));
end;

procedure TCairoContext.ShowGlyphs(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
begin
  cairo_show_glyphs(FHandle, Glyphs, NumGlyphs);
end;

procedure TCairoContext.TextPath(const Text: String);
begin
  cairo_text_path(FHandle, PChar(Text));
end;

procedure TCairoContext.GlyphPath(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
begin
  cairo_glyph_path(FHandle, Glyphs, NumGlyphs);
end;

procedure TCairoContext.TextExtents(const Text: String;
  Extents: Pcairo_text_extents_t);
begin
  cairo_text_extents(FHandle, PChar(Text), Extents);
end;

procedure TCairoContext.GlyphExtents(Glyphs: Pcairo_glyph_t;
  NumGlyphs: LongInt; Extents: Pcairo_text_extents_t);
begin
  cairo_glyph_extents(FHandle, Glyphs, NumGlyphs, Extents);
end;

procedure TCairoContext.FontExtents(Extents: Pcairo_font_extents_t);
begin
  cairo_font_extents(FHandle, Extents);
end;

constructor TCairoContext.Create(Target: TCairoSurface);
begin
  FHandle := cairo_create(Target.FHandle);
end;

destructor TCairoContext.Destroy;
begin
  cairo_destroy(FHandle);
  FFontFace.Free;
end;

function TCairoContext.GetUserData(Key: Pcairo_user_data_key_t): Pointer;
begin
  Result := cairo_get_user_data(FHandle, Key);
end;

function TCairoContext.PopGroup: TCairoPattern;
begin
  Result := TCairoPattern.Create;
  Result.FHandle := cairo_pop_group(FHandle);
end;

procedure TCairoContext.PopGroupToSource;
begin
  cairo_pop_group_to_source(FHandle);
end;

procedure TCairoContext.PushGroup;
begin
  cairo_push_group(FHandle);
end;

procedure TCairoContext.PushGroupWithContent(content: cairo_content_t);
begin
  cairo_push_group_with_content(FHandle, content);
end;

procedure TCairoContext.Restore;
begin
  cairo_restore(FHandle);
end;

procedure TCairoContext.Save;
begin
  cairo_save(FHandle);
end;

function TCairoContext.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; DestroyFunc: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_set_user_data(FHandle, Key, UserData, DestroyFunc);
end;

{ TCairoPattern }

destructor TCairoPattern.Destroy;
begin
  cairo_pattern_destroy(FHandle);
end;

{ TCairoSurface }

constructor TCairoSurface.Create(Other: TCairoSurface;
  Content: cairo_content_t; Width, Height: LongInt);
begin
  FHandle := cairo_surface_create_similar(Other.FHandle, Content, Width, Height);
end;

destructor TCairoSurface.Destroy;
begin
  cairo_surface_destroy(FHandle);
end;

procedure TCairoSurface.Finish;
begin
  cairo_surface_finish(FHandle);
end;

function TCairoSurface.Status: cairo_status_t;
begin
  Result := cairo_surface_status(FHandle);
end;

function TCairoSurface.GetType: cairo_surface_type_t;
begin
  Result := cairo_surface_get_type(FHandle);
end;

function TCairoSurface.GetContent: cairo_content_t;
begin
  Result := cairo_surface_get_content(FHandle);
end;

function TCairoSurface.WriteToPng(Filename: String): cairo_status_t;
begin
  Result := cairo_surface_write_to_png(FHandle, PChar(Filename));
end;

function TCairoSurface.WriteToPngStream(WriteFunc: cairo_write_func_t;
  closure: Pointer): cairo_status_t;
begin
  Result := cairo_surface_write_to_png_stream(FHandle, WriteFunc, closure);
end;

function TCairoSurface.GetUserData(Key: Pcairo_user_data_key_t): Pointer;
begin
  Result := cairo_surface_get_user_data(FHandle, Key);
end;

function TCairoSurface.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; DestroyFunc: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_surface_set_user_data(FHandle, Key, UserData, DestroyFunc);
end;

procedure TCairoSurface.GetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_surface_get_font_options(FHandle, Options);
end;

procedure TCairoSurface.Flush;
begin
  cairo_surface_flush(FHandle);
end;

procedure TCairoSurface.MarkDirty;
begin
  cairo_surface_mark_dirty(FHandle);
end;

procedure TCairoSurface.MarkDirtyRectangle(X, Y, Width, Height: LongInt);
begin
  cairo_surface_mark_dirty_rectangle(FHandle, X, Y, Width, Height);
end;

procedure TCairoSurface.SetDeviceOffset(XOffset, YOffset: Double);
begin
  cairo_surface_set_device_offset(FHandle, XOffset, YOffset);
end;

procedure TCairoSurface.GetDeviceOffset(XOffset, YOffset: PDouble);
begin
  cairo_surface_get_device_offset(FHandle, XOffset, YOffset);
end;

procedure TCairoSurface.SetFallbackResolution(XPixelsPerInch,
  YPixelsPerInch: Double);
begin
  cairo_surface_set_fallback_resolution(FHandle, XPixelsPerInch, YPixelsPerInch);
end;

{ TCairoRectangleList }

function TCairoRectangleList.GetStatus: cairo_status_t;
begin
  Result := FRectangleList^.status;
end;

function TCairoRectangleList.GetRectangles(Index: Integer): cairo_rectangle_t;
begin
  if (Index < 0) or (Index >= NumRectangles) then
    Exit;
  Result := FRectangleList^.rectangles[Index];
end;

function TCairoRectangleList.GetNumRectangles: Integer;
begin
  Result := FRectangleList^.num_rectangles;
end;

destructor TCairoRectangleList.Destroy;
begin
  cairo_rectangle_list_destroy(FRectangleList);
end;

{ TCairoSurfacePattern }

constructor TCairoSurfacePattern.Create(Surface: TCairoSurface);
begin
  FHandle := cairo_pattern_create_for_surface(Surface.FHandle);
end;

{ TCairoSolidPattern }

constructor TCairoSolidPattern.Create(Red, Green, Blue: Double);
begin
  FHandle := cairo_pattern_create_rgb(Red, Green, Blue);
end;

constructor TCairoSolidPattern.Create(Red, Green, Blue, Alpha: Double);
begin
  FHandle := cairo_pattern_create_rgba(Red, Green, Blue, Alpha);
end;

{ TCairoLinearGradient }

constructor TCairoLinearGradient.Create(X0, Y0, X1, Y1: Double);
begin
  FHandle := cairo_pattern_create_linear(X0, Y0, X1, Y1);
end;

{ TCairoRadialGradient }

constructor TCairoRadialGradient.Create(Cx0, Cy0, Radius0, Cx1, Cy1,
  Radius1: Double);
begin
  FHandle := cairo_pattern_create_radial(Cx0, Cy0, Radius0, Cx1, Cy1, Radius1);
end;

{ TCairoGradient }

procedure TCairoGradient.AddColorStopRgb(Offset, Red, Green, Blue: Double);
begin
  cairo_pattern_add_color_stop_rgb(FHandle, Offset, Red, Green, Blue);
end;

procedure TCairoGradient.AddColorStopRgba(Offset, Red, Green, Blue,
  Alpha: Double);
begin
  cairo_pattern_add_color_stop_rgba(FHandle, Offset, Red, Green, Blue, Alpha);
end;

function TCairoGradient.GetColorStopRgba(Index: LongInt; Offset, Red, Green,
  Blue, Alpha: PDouble): cairo_status_t;
begin
  Result := cairo_pattern_get_color_stop_rgba(FHandle, Index, Offset, Red, Green,  Blue, Alpha);
end;

function TCairoGradient.GetColorStopCount(Count: PLongInt): cairo_status_t;
begin
  Result := cairo_pattern_get_color_stop_count(FHandle, Count);
end;

{ TCairoFontOptions }

constructor TCairoFontOptions.Create;
begin
  FHandle := cairo_font_options_create;
end;

destructor TCairoFontOptions.Destroy;
begin
  cairo_font_options_destroy(FHandle);
end;

procedure TCairoFontOptions.Copy(Original: TCairoFontOptions);
begin
  if Assigned(FHandle) then
    cairo_font_options_destroy(FHandle);
  FHandle := cairo_font_options_copy(Original.FHandle);
end;

function TCairoFontOptions.Status: cairo_status_t;
begin
  Result := cairo_font_options_status(FHandle);
end;

procedure TCairoFontOptions.Merge(Other: TCairoFontOptions);
begin
  cairo_font_options_merge(FHandle, Other.FHandle);
end;

function TCairoFontOptions.Equal(Other: TCairoFontOptions): Boolean;
begin
  Result := Boolean(cairo_font_options_equal(FHandle, Other.FHandle))
end;

function TCairoFontOptions.Hash: LongWord;
begin
  Result := cairo_font_options_hash(FHandle);
end;

procedure TCairoFontOptions.SetAntialias(Antialias: cairo_antialias_t);
begin
  cairo_font_options_set_antialias(FHandle, Antialias);
end;

function TCairoFontOptions.GetAntialias: cairo_antialias_t;
begin
  Result := cairo_font_options_get_antialias(FHandle);
end;

procedure TCairoFontOptions.SetSubpixelOrder(
  SubpixelOrder: cairo_subpixel_order_t);
begin
  cairo_font_options_set_subpixel_order(FHandle, SubpixelOrder);
end;

function TCairoFontOptions.GetSubpixelOrder: cairo_subpixel_order_t;
begin
  Result := cairo_font_options_get_subpixel_order(FHandle);
end;

procedure TCairoFontOptions.SetHintStyle(HintStyle: cairo_hint_style_t);
begin
  cairo_font_options_set_hint_style(FHandle, HintStyle);
end;

function TCairoFontOptions.GetHintStyle: cairo_hint_style_t;
begin
  Result := cairo_font_options_get_hint_style(FHandle);
end;

procedure TCairoFontOptions.SetHintMetrics(HintMetrics: cairo_hint_metrics_t);
begin
  cairo_font_options_set_hint_metrics(FHandle, HintMetrics);
end;

function TCairoFontOptions.GetHintMetrics: cairo_hint_metrics_t;
begin
  Result := cairo_font_options_get_hint_metrics(FHandle);
end;

{ TCairoFontFace }

function TCairoFontFace.Status: cairo_status_t;
begin
  Result := cairo_font_face_status(FHandle);
end;

function TCairoFontFace.GetType: cairo_font_type_t;
begin
  Result := cairo_font_face_get_type(FHandle);
end;

function TCairoFontFace.GetUserData(Key: Pcairo_user_data_key_t): Pointer;
begin
  Result := cairo_font_face_get_user_data(FHandle, Key);
end;

function TCairoFontFace.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_font_face_set_user_data(FHandle, Key, UserData, destroy_func);
end;

{ TCairoScaledFont }

constructor TCairoScaledFont.Create(FontFace: TCairoFontFace; FontMatrix,
  Ctm: Pcairo_matrix_t; Options: TCairoFontOptions);
begin
  FHandle := cairo_scaled_font_create(FontFace.FHandle, FontMatrix, Ctm, Options.FHandle)
end;

destructor TCairoScaledFont.Destroy;
begin
  cairo_scaled_font_destroy(FHandle);
end;

function TCairoScaledFont.Status: cairo_status_t;
begin
  Result := cairo_scaled_font_status(FHandle);
end;

function TCairoScaledFont.GetType: cairo_font_type_t;
begin
  Result := cairo_scaled_font_get_type(FHandle);
end;

function TCairoScaledFont.GetUserData(Key: Pcairo_user_data_key_t): Pointer;
begin
  Result := cairo_scaled_font_get_user_data(FHandle, Key);
end;

function TCairoScaledFont.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_scaled_font_set_user_data(FHandle, Key, UserData, destroy_func);
end;

procedure TCairoScaledFont.Extents(AExtents: Pcairo_font_extents_t);
begin
  cairo_scaled_font_extents(FHandle, AExtents);
end;

procedure TCairoScaledFont.TextExtents(Utf8: PChar;
  AExtents: Pcairo_text_extents_t);
begin
  cairo_scaled_font_text_extents(FHandle, Utf8, AExtents);
end;

procedure TCairoScaledFont.GlyphExtents(Glyphs: Pcairo_glyph_t;
  NumGlyphs: LongInt; AExtents: Pcairo_text_extents_t);
begin
  cairo_scaled_font_glyph_extents(FHandle, Glyphs, NumGlyphs, AExtents);
end;

function TCairoScaledFont.GetFontFace: Pcairo_font_face_t;
begin
  Result := cairo_scaled_font_get_font_face(FHandle);
end;

procedure TCairoScaledFont.GetFontMatrix(FontMatrix: Pcairo_matrix_t);
begin
  cairo_scaled_font_get_font_matrix(FHandle, FontMatrix);
end;

procedure TCairoScaledFont.GetCtm(Ctm: Pcairo_matrix_t);
begin
  cairo_scaled_font_get_ctm(FHandle, Ctm);
end;

procedure TCairoScaledFont.GetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_scaled_font_get_font_options(FHandle, Options);
end;

end.

