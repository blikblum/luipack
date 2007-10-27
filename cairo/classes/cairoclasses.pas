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
  cairo;
  
type

  { TCairoSurface }

  TCairoSurface = class
  private
  protected
    FHandle: Pcairo_surface_t;
  public
    constructor CreateSimilar(Other: TCairoSurface; Content: cairo_content_t; Width, Height: LongInt);
    destructor Destroy; override;
    function  Reference: TCairoSurface;
    procedure Finish;
    function  GetReferenceCount: LongWord;
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
    constructor CreateRGB(Red, Green, Blue: Double);
  end;

  { TCairoContext }

  TCairoContext = class
  private
    FHandle: Pcairo_t;
  public
    constructor Create(Target: TCairoSurface);
    destructor Destroy; override;
    procedure Reference;
    function  GetReferenceCount: LongWord;
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
    procedure SetSourceRGB(Red, Green, Blue: Double);
    procedure SetSourceRGBA(Red, Green, Blue, Alpha: Double);
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
    //create a TRectangleList object?
    function  CopyClipRectangleList: Pcairo_rectangle_list_t;
    procedure RectangleListDestroy(RectangleList: Pcairo_rectangle_list_t);
  end;

implementation

{ TCairoContext }

function TCairoContext.GetReferenceCount: LongWord;
begin
  Result := cairo_get_reference_count(FHandle);
end;

procedure TCairoContext.SetOperator(Op: cairo_operator_t);
begin
  cairo_set_operator(FHandle, Op);
end;

procedure TCairoContext.SetSource(Source: TCairoPattern);
begin
  cairo_set_source(FHandle, Source.FHandle);
end;

procedure TCairoContext.SetSourceRGB(Red, Green, Blue: Double);
begin
  cairo_set_source_rgb(FHandle, Red, Green, Blue);
end;

procedure TCairoContext.SetSourceRGBA(Red, Green, Blue, Alpha: Double);
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

function TCairoContext.CopyClipRectangleList: Pcairo_rectangle_list_t;
begin
  Result := cairo_copy_clip_rectangle_list(FHandle);
end;

procedure TCairoContext.RectangleListDestroy(
  RectangleList: Pcairo_rectangle_list_t);
begin
  cairo_rectangle_list_destroy(RectangleList);
end;

constructor TCairoContext.Create(Target: TCairoSurface);
begin
  FHandle := cairo_create(Target.FHandle);
end;

destructor TCairoContext.Destroy;
begin
  cairo_destroy(FHandle);
  inherited Destroy;
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

procedure TCairoContext.Reference;
begin
  cairo_reference(FHandle);
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

constructor TCairoPattern.CreateRGB(Red, Green, Blue: Double);
begin
  FHandle := cairo_pattern_create_rgb(Red, Green, Blue);
end;

{ TCairoSurface }

constructor TCairoSurface.CreateSimilar(Other: TCairoSurface;
  Content: cairo_content_t; Width, Height: LongInt);
begin
  FHandle := cairo_surface_create_similar(Other.FHandle, Content, Width, Height);
end;

destructor TCairoSurface.Destroy;
begin
  cairo_surface_destroy(FHandle);
  inherited Destroy;
end;

function TCairoSurface.Reference: TCairoSurface;
begin
  Result := TCairoSurface.Create;
  Result.FHandle := cairo_surface_reference(FHandle);
end;

procedure TCairoSurface.Finish;
begin
  cairo_surface_finish(FHandle);
end;

function TCairoSurface.GetReferenceCount: LongWord;
begin
  Result := cairo_surface_get_reference_count(FHandle);
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

end.

