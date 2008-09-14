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
  Classes, Cairo, ObjectAvlTree;
  
type

  TCairoColor = record
    Red: Double;
    Green: Double;
    Blue: Double;
    Alpha: Double;
  end;
  
  TReferenceNotify = procedure(Key: Pointer) of object;
  
  { TCairoMatrix }

  TCairoMatrix = object
  private
    FData: cairo_matrix_t;
  public
    procedure Init(XX, YX, XY, YY, X0, Y0: Double);
    procedure InitIdentity;
    procedure InitTranslate(Tx, Ty: Double);
    procedure InitScale(Sx, Sy: Double);
    procedure InitRotate(Radians: Double);
    procedure Translate(Tx, Ty: Double);
    procedure Scale(Sx, Sy: Double);
    procedure Rotate(Radians: Double);
    function  Invert: cairo_status_t;
    procedure Multiply(const A, B: TCairoMatrix);
    procedure MatrixTransformDistance(Dx, Dy: PDouble);
    procedure TransformPoint(X, Y: PDouble);
  end;
  
  { TCairoSurface }

  TCairoSurface = class
  protected
    FHandle: Pcairo_surface_t;
    FOnReference: TReferenceNotify;
    FHandleIsPrivate: Boolean;
    function  GetContent: cairo_content_t;
  public
    constructor Create(AHandle: Pcairo_surface_t);
    constructor Create(Other: TCairoSurface; Content: cairo_content_t; Width, Height: LongInt);
    destructor Destroy; override;
    procedure Finish;
    procedure Reference;
    function  Status: cairo_status_t;
    function  GetType: cairo_surface_type_t;
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
    property Content: cairo_content_t read GetContent;
  end;
  
  { TCairoImageSurface }

  TCairoImageSurface = class(TCairoSurface)
  private
    function GetData: PChar;
    function GetFormat: cairo_format_t;
    function GetWidth: LongInt;
    function GetHeight: LongInt;
    function GetStride: LongInt;
  public
    constructor Create(Format: cairo_format_t; width, height: LongInt);
    constructor Create(Data: PByte; Format: cairo_format_t; Width, Height, Stride: LongInt);
    constructor Create(const Filename: String);
    constructor Create(ReadFunc: cairo_read_func_t; Closure: pointer);
    property Data: PChar read GetData;
    property Format: cairo_format_t read GetFormat;
    property Height: LongInt read GetHeight;
    property Stride: LongInt read GetStride;
    property Width: LongInt read GetWidth;
  end;
  
  { TCairoPdfSurface }

  TCairoPDFSurface = class(TCairoSurface)
  public
    constructor Create(const Filename: String; WidthInPoints, HeightInPoints: Double);
    constructor Create(WriteFunc: cairo_write_func_t; Closure: Pointer; WidthInPoints, HeightInPoints: Double);
    procedure SetSize(WidthInPoints, HeightInPoints: Double);
  end;
  
  { TCairoPSSurface }

  TCairoPSSurface = class(TCairoSurface)
  public
    constructor Create(const Filename: String; WidthInPoints, HeightInPoints: Double);
    constructor Create(WriteFunc: cairo_write_func_t; Closure: Pointer; WidthInPoints, HeightInPoints: Double);
    procedure DscBeginPageSetup;
    procedure DscBeginSetup;
    procedure DscComment(const Comment: String);
    procedure SetSize(WidthInPoints, HeightInPoints: Double);
  end;

  { TCairoSVGSurface }

  TCairoSVGSurface = class(TCairoSurface)
  public
    constructor Create(const Filename: String; WidthInPoints, HeightInPoints: Double);
    constructor Create(WriteFunc: cairo_write_func_t; Closure: Pointer; WidthInPoints, HeightInPoints: Double);
    procedure RestrictToVersion(Version: cairo_svg_version_t);
  end;
  
  { TCairoPattern }

  TCairoPattern = class
  private
    FHandle: Pcairo_pattern_t;
    FOnReference: TReferenceNotify;
    FHandleIsPrivate: Boolean;
    function  GetExtend: cairo_extend_t;
    function  GetFilter: cairo_filter_t;
    procedure SetExtend(Extend: cairo_extend_t);
    procedure SetFilter(Filter: cairo_filter_t);
  public
    destructor Destroy; override;
    procedure Reference;
    procedure SetMatrix(const Matrix: TCairoMatrix);
    procedure GetMatrix(const Matrix: TCairoMatrix);
    function  Status: cairo_status_t;
    function  GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
    function  GetType: cairo_pattern_type_t;
    property Extend: cairo_extend_t read GetExtend write SetExtend;
    property Filter: cairo_filter_t read GetFilter write SetFilter;
  end;
  
  { TCairoSolidPattern }

  TCairoSolidPattern = class (TCairoPattern)
  public
    constructor Create(Red, Green, Blue: Double);
    constructor Create(Red, Green, Blue, Alpha: Double);
    constructor Create(const Color: TCairoColor);
    function  GetRgba(Red, Green, Blue, Alpha: PDouble): cairo_status_t;
  end;
  
  { TCairoGradient }

  TCairoGradient = class (TCairoPattern)
  public
    procedure AddColorStopRgb(Offset, Red, Green, Blue: Double);
    procedure AddColorStopRgba(Offset, Red, Green, Blue, Alpha: Double);
    procedure AddColorStop(Offset: Double; const Color: TCairoColor);
    function  GetColorStopRgba(Index: LongInt; Offset, Red, Green, Blue, Alpha: PDouble): cairo_status_t;
    function  GetColorStopCount(Count: PLongInt): cairo_status_t;
  end;
  
  { TCairoLinearGradient }

  TCairoLinearGradient = class (TCairoGradient)
  public
    constructor Create(X0, Y0, X1, Y1: Double);
    function  GetLinearPoints (X0, Y0, X1, Y1: PDouble): cairo_status_t;
  end;

  { TCairoRadialGradient }

  TCairoRadialGradient = class (TCairoGradient)
  public
    constructor Create(Cx0, Cy0, Radius0, Cx1, Cy1, Radius1: Double);
    function  GetRadialCircles (X0, Y0, R0, X1, Y1, R1: PDouble): cairo_status_t;
  end;

  { TCairoSurfacePattern }

  TCairoSurfacePattern = class (TCairoPattern)
  private
    FSurface: TCairoSurface;
    function  GetSurface: TCairoSurface;
  public
    constructor Create(Surface: TCairoSurface);
    destructor Destroy; override;
    property Surface: TCairoSurface read GetSurface;
  end;

  { TCairoRectangleList }

  TCairoRectangleList = class
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
  
  { TCairoPath }

  TCairoPath = class
  private
    FPath: Pcairo_path_t;
  public
    destructor Destroy; override;
  end;

  { TCairoFontOptions }

  TCairoFontOptions = class
  private
    FHandle: Pcairo_font_options_t;
    procedure SetAntialias(Antialias: cairo_antialias_t);
    function  GetAntialias: cairo_antialias_t;
    procedure SetHintMetrics(HintMetrics: cairo_hint_metrics_t);
    function  GetHintMetrics: cairo_hint_metrics_t;
    procedure SetHintStyle(HintStyle: cairo_hint_style_t);
    function  GetHintStyle: cairo_hint_style_t;
    procedure SetSubpixelOrder(SubpixelOrder: cairo_subpixel_order_t);
    function  GetSubpixelOrder: cairo_subpixel_order_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Copy(Original: TCairoFontOptions);
    function  Status: cairo_status_t;
    procedure Merge(Other: TCairoFontOptions);
    function  Equal(Other: TCairoFontOptions): Boolean;
    function  Hash: LongWord;
    property Antialias: cairo_antialias_t read GetAntialias write SetAntialias;
    property HintMetrics: cairo_hint_metrics_t read GetHintMetrics write SetHintMetrics;
    property HintStyle: cairo_hint_style_t read GetHintStyle write SetHintStyle;
    property SubpixelOrder: cairo_subpixel_order_t read GetSubpixelOrder write SetSubpixelOrder;
  end;

  { TCairoFontFace }

  TCairoFontFace = class
  protected
    FHandle: Pcairo_font_face_t;
    FOnReference: TReferenceNotify;
    FHandleIsPrivate: Boolean;
    function GetHandle: Pcairo_font_face_t; virtual;
  public
    destructor Destroy; override;
    function Status: cairo_status_t;
    function GetType: cairo_font_type_t;
    function GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    procedure Reference;
    function SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
    property Handle: Pcairo_font_face_t read GetHandle;
  end;

  { TCairoScaledFont }

  TCairoScaledFont = class
  private
    FHandle: Pcairo_scaled_font_t;
    FFontFace: TCairoFontFace;
    FOnReference: TReferenceNotify;
    FHandleIsPrivate: Boolean;
    constructor Create;
    procedure PrivateObjectReferenced(Key: Pointer);
    function  GetFontFace: TCairoFontFace;
  public
    constructor Create(FontFace: TCairoFontFace; const FontMatrix, Ctm: TCairoMatrix; Options: TCairoFontOptions);
    destructor Destroy; override;
    function  Status: cairo_status_t;
    function  GetType: cairo_font_type_t;
    function  GetUserData (Key: Pcairo_user_data_key_t): Pointer;
    function  SetUserData (Key: Pcairo_user_data_key_t; UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
    procedure Extents(AExtents: Pcairo_font_extents_t);
    procedure TextExtents(Utf8: PChar; AExtents: Pcairo_text_extents_t);
    procedure GlyphExtents(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt; AExtents: Pcairo_text_extents_t);
    procedure GetFontMatrix(var FontMatrix: TCairoMatrix);
    procedure GetCtm(var Ctm: TCairoMatrix);
    procedure GetFontOptions (Options: Pcairo_font_options_t);
    procedure Reference;
    property FontFace: TCairoFontFace read GetFontFace;
  end;

  { TCairoContext }

  TCairoContext = class
  private
    FHandle: Pcairo_t;
    FObjectTree: TPointerToPointerTree; //stores the private objects
    procedure FreePrivateObjects;
    procedure ObjectTreeNeeded;
    procedure PrivateObjectReferenced(Key: Pointer);
    procedure SetColor(const Value: TCairoColor); inline;
  protected
    function  GetAntialias: cairo_antialias_t;
    function  GetDashCount: LongInt;
    function  GetFillRule: cairo_fill_rule_t;
    function  GetFontFace: TCairoFontFace;
    function  GetGroupTarget: TCairoSurface;
    function  GetLineWidth: Double;
    function  GetLineCap: cairo_line_cap_t;
    function  GetLineJoin: cairo_line_join_t;
    function  GetMiterLimit: Double;
    function  GetScaledFont: TCairoScaledFont;
    function  GetSource: TCairoPattern;
    function  GetTarget: TCairoSurface;
    function  GetTolerance: Double;
    procedure SetAntialias(Antialias: cairo_antialias_t);
    procedure SetFillRule(FillRule: cairo_fill_rule_t);
    procedure SetFontFace(FontFace: TCairoFontFace);
    procedure SetFontSize(Size: Double);
    procedure SetLineWidth(Width: Double);
    procedure SetLineCap(LineCap: cairo_line_cap_t);
    procedure SetLineJoin(LineJoin: cairo_line_join_t);
    procedure SetMiterLimit(Limit: Double);
    procedure SetScaledFont(ScaledFont: TCairoScaledFont);
    procedure SetSource(Source: TCairoPattern);
    procedure SetTolerance(Tolerance: Double);
  public
    constructor Create(Target: TCairoSurface);
    constructor Create(Target: Pcairo_surface_t);
    destructor Destroy; override;
    procedure AppendPath(Path: TCairoPath);
    procedure Arc(Xc, Yc, Radius, Angle1, Angle2: Double);
    procedure ArcNegative(Xc, Yc, Radius, Angle1, Angle2: Double);
    procedure Clip;
    procedure ClipExtents(X1, Y1, X2, Y2:  PDouble);
    procedure ClipPreserve;
    procedure ClosePath;
    function  CopyClipRectangleList: TCairoRectangleList;
    procedure CopyPage;
    function  CopyPath: TCairoPath;
    function  CopyPathFlat: TCairoPath;
    procedure CurveTo(X1, Y1, X2, Y2, X3, Y3: Double);
    procedure DeviceToUser(X, Y:PDouble);
    procedure DeviceToUserDistance(Dx, Dy: PDouble);
    procedure Fill;
    procedure FillExtents(X1, Y1, X2, Y2: PDouble);
    procedure FillPreserve;
    procedure FontExtents(Extents: Pcairo_font_extents_t);
    procedure GetCurrentPoint(X, Y: PDouble);
    procedure GetDash(Dashes, Offset: PDouble);
    procedure GetFontMatrix(const Matrix: TCairoMatrix);
    procedure GetFontOptions(Options: TCairoFontOptions);
    procedure GetMatrix(const Matrix: TCairoMatrix);
    function  GetOperator: cairo_operator_t;
    function  GetUserData(Key: Pcairo_user_data_key_t): Pointer;
    procedure GlyphExtents(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt; Extents: Pcairo_text_extents_t);
    procedure GlyphPath(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
    procedure IdentityMatrix;
    function  InFill(X, Y: Double): Boolean;
    function  InStroke(X, Y: Double): Boolean;
    procedure LineTo(X, Y: Double);
    procedure Mask(Pattern: TCairoPattern);
    procedure MaskSurface(Surface: TCairoSurface; SurfaceX, SurfaceY: Double);
    procedure MoveTo(X, Y: Double);
    procedure NewPath;
    procedure NewSubPath;
    procedure Paint;
    procedure PaintWithAlpha(Alpha: Double);
    function  PopGroup: TCairoPattern;
    procedure PopGroupToSource;
    procedure PushGroup;
    procedure PushGroupWithContent(Content: cairo_content_t);
    procedure Rectangle(X, Y, Width, Height: Double);
    procedure RelCurveTo(Dx1, Dy1, Dx2, Dy2, Dx3, Dy3: Double);
    procedure RelLineTo(Dx, Dy: Double);
    procedure RelMoveTo(Dx, Dy: Double);
    procedure ResetClip;
    procedure Restore;
    procedure Rotate(Angle: Double);
    procedure Save;
    procedure Scale(Sx, Sy: Double);
    procedure ShowGlyphs(Glyphs: Pcairo_glyph_t; NumGlyphs: LongInt);
    procedure ShowText(const Text: String);
    procedure SelectFontFace(const Family: String; Slant: cairo_font_slant_t; Weight: cairo_font_weight_t);
    procedure SetDash(Dashes: PDouble; NumDashes: LongInt; Offset: Double);
    procedure SetFontMatrix(const Matrix: TCairoMatrix);
    procedure SetFontOptions(Options: Pcairo_font_options_t);
    procedure SetMatrix(const Matrix: TCairoMatrix);
    procedure SetOperator(Op: cairo_operator_t);
    procedure SetSourceRgb(Red, Green, Blue: Double);
    procedure SetSourceRgba(Red, Green, Blue, Alpha: Double);
    procedure SetSourceSurface(Surface: TCairoSurface; X, Y: Double);
    function  SetUserData(Key: Pcairo_user_data_key_t; UserData: Pointer; DestroyFunc: cairo_destroy_func_t): cairo_status_t;
    procedure ShowPage;
    function  Status: cairo_status_t;
    procedure Stroke;
    procedure StrokeExtents(X1, Y1, X2, Y2: PDouble);
    procedure StrokePreserve;
    procedure TextExtents(const Text: String; Extents: Pcairo_text_extents_t);
    procedure TextPath(const Text: String);
    procedure Transform(const Matrix: TCairoMatrix);
    procedure Translate(Tx, Ty: Double);
    procedure UserToDevice(X, Y: PDouble);
    procedure UserToDeviceDistance(Dx, Dy: PDouble);
    //properties
    property Antialias: cairo_antialias_t read GetAntialias write SetAntialias;
    property Color: TCairoColor write SetColor;
    property DashCount: LongInt read GetDashCount;
    property FillRule: cairo_fill_rule_t read GetFillRule write SetFillRule;
    property FontFace: TCairoFontFace read GetFontFace write SetFontFace;
    property FontSize: Double write SetFontSize;
    property GroupTarget: TCairoSurface read GetGroupTarget;
    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property LineCap: cairo_line_cap_t read GetLineCap write SetLineCap;
    property LineJoin: cairo_line_join_t read GetLineJoin write SetLineJoin;
    property MiterLimit: Double read GetMiterLimit write SetMiterLimit;
    //property Operator: cairo_operator_t read GetOperator;
    property ScaledFont: TCairoScaledFont read GetScaledFont write SetScaledFont;
    property Source: TCairoPattern read GetSource write SetSource;
    property Target: TCairoSurface read GetTarget;
    property Tolerance: Double read GetTolerance write SetTolerance;
    property Handle: Pcairo_t read FHandle;
  end;
  
  function CairoColor(Red, Green, Blue, Alpha: Double): TCairoColor;

  function RGBToCairoColor(R, G, B: Byte): TCairoColor;
  function RGBAToCairoColor(R, G, B, A: Byte): TCairoColor;
  
  function RGBToCairoColor(RGB: Cardinal): TCairoColor;
  function BGRToCairoColor(BGR: Cardinal): TCairoColor;

  function ARGBToCairoColor(ARGB: Cardinal): TCairoColor;
  function ABGRToCairoColor(ABGR: Cardinal): TCairoColor;
  
  function StatusToString(Status: cairo_status_t): String;

implementation

function CairoColor(Red, Green, Blue, Alpha: Double): TCairoColor;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
  Result.Alpha := Alpha;
end;

function RGBToCairoColor(R, G, B: Byte): TCairoColor;
begin
  Result.Red := R / 255;
  Result.Green := G / 255;
  Result.Blue := B / 255;
  Result.Alpha := 1;
end;

function RGBAToCairoColor(R, G, B, A: Byte): TCairoColor;
begin
  Result.Red := R / 255;
  Result.Green := G / 255;
  Result.Blue := B / 255;
  Result.Alpha := A / 255;
end;

function RGBToCairoColor(RGB: Cardinal): TCairoColor;
begin
  Result.Red := ((RGB shr 16) and $000000FF) / 255;
  Result.Green := ((RGB shr 8) and $000000FF) / 255;
  Result.Blue := (RGB and $000000FF) / 255;
  Result.Alpha := 1;
end;

function BGRToCairoColor(BGR: Cardinal): TCairoColor;
begin
  Result.Red := (BGR and $000000FF) / 255;
  Result.Green := ((BGR shr 8) and $000000FF) / 255;
  Result.Blue := ((BGR shr 16) and $000000FF) / 255;
  Result.Alpha := 1;
end;

function ARGBToCairoColor(ARGB: Cardinal): TCairoColor;
begin
  Result.Red := ((ARGB shr 16) and $000000FF) / 255;
  Result.Green := ((ARGB shr 8) and $000000FF) / 255;
  Result.Blue := (ARGB and $000000FF) / 255;
  Result.Alpha := ((ARGB shr 32) and $000000FF) / 255; //Correct??
end;

function ABGRToCairoColor(ABGR: Cardinal): TCairoColor;
begin
  Result.Red := (ABGR and $000000FF) / 255;
  Result.Green := ((ABGR shr 8) and $000000FF) / 255;
  Result.Blue := ((ABGR shr 16) and $000000FF) / 255;
  Result.Alpha := ((ABGR shr 32) and $000000FF) / 255; //Correct??
end;

function StatusToString(Status: cairo_status_t): String;
begin
  Result := cairo_status_to_string(Status);
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

procedure TCairoContext.Transform(const Matrix: TCairoMatrix);
begin
  cairo_transform(FHandle, @Matrix.FData);
end;

procedure TCairoContext.SetMatrix(const Matrix: TCairoMatrix);
begin
  cairo_set_matrix(FHandle, @Matrix.FData);
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

procedure TCairoContext.SetFontMatrix(const Matrix: TCairoMatrix);
begin
  cairo_set_font_matrix(FHandle, @Matrix.FData);
end;

procedure TCairoContext.GetFontMatrix(const Matrix: TCairoMatrix);
begin
  cairo_get_font_matrix(FHandle, @Matrix.FData);
end;

procedure TCairoContext.SetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_set_font_options(FHandle, Options);
end;

procedure TCairoContext.GetFontOptions(Options: TCairoFontOptions);
begin
  cairo_get_font_options(FHandle, Options.FHandle);
end;

procedure TCairoContext.SetFontFace(FontFace: TCairoFontFace);
begin
  cairo_set_font_face(FHandle, FontFace.Handle);
end;

function TCairoContext.GetFontFace: TCairoFontFace;
var
  FontFaceHandle: Pcairo_font_face_t;
begin
  ObjectTreeNeeded;
  FontFaceHandle := cairo_get_font_face(FHandle);
  Result := TCairoFontFace(FObjectTree[FontFaceHandle]);
  if Result = nil then
  begin
    Result := TCairoFontFace.Create;
    Result.FHandle := FontFaceHandle;
    Result.FHandleIsPrivate := True;
    Result.FOnReference := @PrivateObjectReferenced;
    FObjectTree[FontFaceHandle] := Result;
  end;
end;

procedure TCairoContext.SetScaledFont(ScaledFont: TCairoScaledFont);
begin
  cairo_set_scaled_font(FHandle, ScaledFont.FHandle);
end;

function TCairoContext.GetScaledFont: TCairoScaledFont;
var
  ScaledFontHandle: Pcairo_scaled_font_t;
begin
  ObjectTreeNeeded;
  ScaledFontHandle := cairo_get_scaled_font(FHandle);
  Result := TCairoScaledFont(FObjectTree[ScaledFontHandle]);
  if Result = nil then
  begin
    Result := TCairoScaledFont.Create;
    Result.FHandle := ScaledFontHandle;
    Result.FHandleIsPrivate := True;
    Result.FOnReference := @PrivateObjectReferenced;
    FObjectTree[ScaledFontHandle] := Result;
  end;
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

function TCairoContext.GetOperator: cairo_operator_t;
begin
  Result := cairo_get_operator(FHandle);
end;

function TCairoContext.GetSource: TCairoPattern;
var
  SourceHandle: Pcairo_pattern_t;
begin
  ObjectTreeNeeded;
  SourceHandle := cairo_get_source(FHandle);
  Result := TCairoPattern(FObjectTree[SourceHandle]);
  if Result = nil then
  begin
    Result := TCairoPattern.Create;
    Result.FHandle := SourceHandle;
    Result.FHandleIsPrivate := True;
    Result.FOnReference := @PrivateObjectReferenced;
    FObjectTree[SourceHandle] := Result;
  end;
end;

function TCairoContext.GetTolerance: Double;
begin
  Result := cairo_get_tolerance(FHandle);
end;

function TCairoContext.GetAntialias: cairo_antialias_t;
begin
  Result := cairo_get_antialias(FHandle);
end;

procedure TCairoContext.GetCurrentPoint(X, Y: PDouble);
begin
  cairo_get_current_point(FHandle, X, Y);
end;

function TCairoContext.GetFillRule: cairo_fill_rule_t;
begin
  Result := cairo_get_fill_rule(FHandle);
end;

function TCairoContext.GetLineWidth: Double;
begin
  Result := cairo_get_line_width(FHandle);
end;

function TCairoContext.GetLineCap: cairo_line_cap_t;
begin
  Result := cairo_get_line_cap(FHandle);
end;

function TCairoContext.GetLineJoin: cairo_line_join_t;
begin
  Result := cairo_get_line_join(FHandle);
end;

function TCairoContext.GetMiterLimit: Double;
begin
  Result := cairo_get_miter_limit(FHandle);
end;

function TCairoContext.GetDashCount: LongInt;
begin
  Result := cairo_get_dash_count(FHandle);
end;

procedure TCairoContext.GetDash(Dashes, Offset: PDouble);
begin
  cairo_get_dash(FHandle, Dashes, Offset);
end;

procedure TCairoContext.GetMatrix(const Matrix: TCairoMatrix);
begin
  cairo_get_matrix(FHandle, @Matrix.FData);
end;

function TCairoContext.GetTarget: TCairoSurface;
var
  TargetHandle: Pcairo_surface_t;
begin
  ObjectTreeNeeded;
  TargetHandle := cairo_get_target(FHandle);
  Result := TCairoSurface(FObjectTree[TargetHandle]);
  if Result = nil then
  begin
    Result := TCairoSurface.Create(TargetHandle);
    Result.FHandleIsPrivate := True;
    Result.FOnReference := @PrivateObjectReferenced;
    FObjectTree[TargetHandle] := Result;
  end;
end;

function TCairoContext.GetGroupTarget: TCairoSurface;
var
  GroupTargetHandle: Pcairo_surface_t;
begin
  ObjectTreeNeeded;
  GroupTargetHandle := cairo_get_group_target(FHandle);
  Result := TCairoSurface(FObjectTree[GroupTargetHandle]);
  if Result = nil then
  begin
    Result := TCairoSurface.Create(GroupTargetHandle);
    Result.FHandleIsPrivate := True;
    Result.FOnReference := @PrivateObjectReferenced;
    FObjectTree[GroupTargetHandle] := Result;
  end;
end;

function TCairoContext.Status: cairo_status_t;
begin
  Result := cairo_status(FHandle);
end;

function TCairoContext.CopyPath: TCairoPath;
begin
  Result := TCairoPath.Create;
  Result.FPath := cairo_copy_path(FHandle);
end;

function TCairoContext.CopyPathFlat: TCairoPath;
begin
  Result := TCairoPath.Create;
  Result.FPath := cairo_copy_path_flat(FHandle);
end;

constructor TCairoContext.Create(Target: Pcairo_surface_t);
begin
  FHandle := cairo_create(Target);
end;

procedure TCairoContext.AppendPath(Path: TCairoPath);
begin
  cairo_append_path(FHandle, Path.FPath);
end;

procedure TCairoContext.FreePrivateObjects;
var
  Key, NextKey, AnObject: Pointer;
begin
  if FObjectTree = nil then
    Exit;
  if FObjectTree.GetFirst(Key, AnObject) then
  begin
    TObject(AnObject).Free;
    while FObjectTree.GetNext(Key, NextKey, AnObject) do
    begin
      TObject(AnObject).Free;
      Key := NextKey;
    end;
  end;
  FObjectTree.Destroy;
end;

procedure TCairoContext.ObjectTreeNeeded;
begin
  if FObjectTree = nil then
    FObjectTree := TPointerToPointerTree.Create;
end;

procedure TCairoContext.PrivateObjectReferenced(Key: Pointer);
begin
  FObjectTree.Remove(Key);
end;

procedure TCairoContext.SetColor(const Value: TCairoColor);
begin
  with Value do
    SetSourceRgba(Red, Green, Blue, Alpha);
end;

constructor TCairoContext.Create(Target: TCairoSurface);
begin
  FHandle := cairo_create(Target.FHandle);
end;

destructor TCairoContext.Destroy;
begin
  FreePrivateObjects;
  cairo_destroy(FHandle);
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
  if not FHandleIsPrivate then
    cairo_pattern_destroy(FHandle);
end;

procedure TCairoPattern.Reference;
begin
  if FHandleIsPrivate then
  begin
    FHandleIsPrivate := False;
    cairo_pattern_reference(FHandle);
    FOnReference(FHandle);
  end;
end;

procedure TCairoPattern.SetMatrix(const Matrix: TCairoMatrix);
begin
  cairo_pattern_set_matrix(FHandle, @Matrix.FData);
end;

procedure TCairoPattern.GetMatrix(const Matrix: TCairoMatrix);
begin
  cairo_pattern_get_matrix(FHandle, @Matrix.FData);
end;

procedure TCairoPattern.SetExtend(Extend: cairo_extend_t);
begin
  cairo_pattern_set_extend(FHandle, Extend);
end;

function TCairoPattern.GetExtend: cairo_extend_t;
begin
  Result := cairo_pattern_get_extend(FHandle);
end;

procedure TCairoPattern.SetFilter(Filter: cairo_filter_t);
begin
  cairo_pattern_set_filter(FHandle, Filter);
end;

function TCairoPattern.GetFilter: cairo_filter_t;
begin
  Result := cairo_pattern_get_filter(FHandle);
end;

function TCairoPattern.Status: cairo_status_t;
begin
  Result := cairo_pattern_status(FHandle);
end;

function TCairoPattern.GetUserData(Key: Pcairo_user_data_key_t): Pointer;
begin
  Result := cairo_pattern_get_user_data(FHandle, Key);
end;

function TCairoPattern.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_pattern_set_user_data(FHandle, Key, UserData, destroy_func);
end;

function TCairoPattern.GetType: cairo_pattern_type_t;
begin
  Result := cairo_pattern_get_type(FHandle);
end;

{ TCairoSurface }

constructor TCairoSurface.Create(AHandle: Pcairo_surface_t);
begin
  FHandle := AHandle;
end;

constructor TCairoSurface.Create(Other: TCairoSurface;
  Content: cairo_content_t; Width, Height: LongInt);
begin
  FHandle := cairo_surface_create_similar(Other.FHandle, Content, Width, Height);
end;

destructor TCairoSurface.Destroy;
begin
  if not FHandleIsPrivate then
    cairo_surface_destroy(FHandle);
end;

procedure TCairoSurface.Finish;
begin
  cairo_surface_finish(FHandle);
end;

procedure TCairoSurface.Reference;
begin
  if FHandleIsPrivate then
  begin
    FHandleIsPrivate := False;
    cairo_surface_reference(FHandle);
    FOnReference(FHandle);
  end;
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

destructor TCairoSurfacePattern.Destroy;
begin
  inherited Destroy;
  FSurface.Free;
end;

function TCairoSurfacePattern.GetSurface: TCairoSurface;
var
  SurfaceHandle: Pcairo_surface_t;
begin
  if FSurface = nil then
  begin
    cairo_pattern_get_surface(FHandle, @SurfaceHandle);
    FSurface := TCairoSurface.Create(SurfaceHandle);
    FSurface.FHandleIsPrivate := True;
  end;
  Result := FSurface;
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

constructor TCairoSolidPattern.Create(const Color: TCairoColor);
begin
  with Color do
    FHandle := cairo_pattern_create_rgba(Red, Green, Blue, Alpha);
end;

function TCairoSolidPattern.GetRgba(Red, Green, Blue, Alpha: PDouble
  ): cairo_status_t;
begin
  Result := cairo_pattern_get_rgba(FHandle, Red, Green, Blue, Alpha);
end;

{ TCairoLinearGradient }

constructor TCairoLinearGradient.Create(X0, Y0, X1, Y1: Double);
begin
  FHandle := cairo_pattern_create_linear(X0, Y0, X1, Y1);
end;

function TCairoLinearGradient.GetLinearPoints(X0, Y0, X1, Y1: PDouble
  ): cairo_status_t;
begin
  Result := cairo_pattern_get_linear_points(FHandle, X0, Y0, X1, Y1);
end;

{ TCairoRadialGradient }

constructor TCairoRadialGradient.Create(Cx0, Cy0, Radius0, Cx1, Cy1,
  Radius1: Double);
begin
  FHandle := cairo_pattern_create_radial(Cx0, Cy0, Radius0, Cx1, Cy1, Radius1);
end;

function TCairoRadialGradient.GetRadialCircles(X0, Y0, R0, X1, Y1, R1: PDouble
  ): cairo_status_t;
begin
  Result := cairo_pattern_get_radial_circles(FHandle, X0, Y0, R0, X1, Y1, R1);
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

procedure TCairoGradient.AddColorStop(Offset: Double; const Color: TCairoColor
  );
begin
  with Color do
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

destructor TCairoFontFace.Destroy;
begin
  if not FHandleIsPrivate then
    cairo_font_face_destroy(FHandle);
end;

function TCairoFontFace.GetHandle: Pcairo_font_face_t;
begin
  Result := FHandle;
end;

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

procedure TCairoFontFace.Reference;
begin
  if FHandleIsPrivate then
  begin
    FHandleIsPrivate := False;
    cairo_font_face_reference(FHandle);
    FOnReference(FHandle);
  end;
end;

function TCairoFontFace.SetUserData(Key: Pcairo_user_data_key_t;
  UserData: Pointer; destroy_func: cairo_destroy_func_t): cairo_status_t;
begin
  Result := cairo_font_face_set_user_data(FHandle, Key, UserData, destroy_func);
end;

{ TCairoScaledFont }

constructor TCairoScaledFont.Create(FontFace: TCairoFontFace; const FontMatrix, Ctm: TCairoMatrix; Options: TCairoFontOptions);
begin
  FHandle := cairo_scaled_font_create(FontFace.Handle, @FontMatrix.FData, @Ctm.FData, Options.FHandle)
end;

constructor TCairoScaledFont.Create;
begin
  //necessary to create as private object in context
end;

destructor TCairoScaledFont.Destroy;
begin
  if not FHandleIsPrivate then
    cairo_scaled_font_destroy(FHandle);
  FFontFace.Free;
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

procedure TCairoScaledFont.PrivateObjectReferenced(Key: Pointer);
begin
  FFontFace := nil;
end;

procedure TCairoScaledFont.Reference;
begin
  if FHandleIsPrivate then
  begin
    FHandleIsPrivate := False;
    cairo_scaled_font_reference(FHandle);
    FOnReference(FHandle);
  end;
end;

function TCairoScaledFont.GetFontFace: TCairoFontFace;
begin
  if FFontFace = nil then
  begin
    FFontFace := TCairoFontFace.Create;
    FFontFace.FHandle := cairo_scaled_font_get_font_face(FHandle);
    FFontFace.FHandleIsPrivate := True;
    FFontFace.FOnReference := @PrivateObjectReferenced;
  end;
  Result := FFontFace;
end;

procedure TCairoScaledFont.GetFontMatrix(var FontMatrix: TCairoMatrix);
begin
  cairo_scaled_font_get_font_matrix(FHandle, @FontMatrix.FData);
end;

procedure TCairoScaledFont.GetCtm(var Ctm: TCairoMatrix);
begin
  cairo_scaled_font_get_ctm(FHandle, @Ctm.FData);
end;

procedure TCairoScaledFont.GetFontOptions(Options: Pcairo_font_options_t);
begin
  cairo_scaled_font_get_font_options(FHandle, Options);
end;

{ TCairoImageSurface }

constructor TCairoImageSurface.Create(Format: cairo_format_t; Width,
  Height: LongInt);
begin
  FHandle := cairo_image_surface_create(Format, Width, Height);
end;

constructor TCairoImageSurface.Create(Data: PByte; Format: cairo_format_t;
  Width, Height, Stride: LongInt);
begin
  FHandle := cairo_image_surface_create_for_data(Data, Format, Width, Height, Stride);
end;

constructor TCairoImageSurface.Create(const Filename: String);
begin
  FHandle := cairo_image_surface_create_from_png(PChar(Filename));
end;

constructor TCairoImageSurface.Create(ReadFunc: cairo_read_func_t;
  Closure: pointer);
begin
  FHandle := cairo_image_surface_create_from_png_stream(ReadFunc, Closure);
end;

function TCairoImageSurface.GetData: PChar;
begin
  Result := cairo_image_surface_get_data(FHandle);
end;

function TCairoImageSurface.GetFormat: cairo_format_t;
begin
  Result := cairo_image_surface_get_format(FHandle);
end;

function TCairoImageSurface.GetWidth: LongInt;
begin
  Result := cairo_image_surface_get_width(FHandle);
end;

function TCairoImageSurface.GetHeight: LongInt;
begin
  Result := cairo_image_surface_get_height(FHandle);
end;

function TCairoImageSurface.GetStride: LongInt;
begin
  Result := cairo_image_surface_get_stride(FHandle);
end;

{ TCairoMatrix }

procedure TCairoMatrix.Init(XX, YX, XY, YY, X0, Y0: Double);
begin
  cairo_matrix_init(@FData, XX, YX, XY, YY, X0, Y0);
end;

procedure TCairoMatrix.InitIdentity;
begin
  cairo_matrix_init_identity(@FData);
end;

procedure TCairoMatrix.InitTranslate(Tx, Ty: Double);
begin
  cairo_matrix_init_translate(@FData, Tx, Ty);
end;

procedure TCairoMatrix.InitScale(Sx, Sy: Double);
begin
  cairo_matrix_init_scale(@FData, Sx, Sy);
end;

procedure TCairoMatrix.InitRotate(Radians: Double);
begin
  cairo_matrix_init_rotate(@FData, Radians);
end;

procedure TCairoMatrix.Translate(Tx, Ty: Double);
begin
  cairo_matrix_translate(@FData, Tx, Ty);
end;

procedure TCairoMatrix.Scale(Sx, Sy: Double);
begin
  cairo_matrix_scale(@FData, Sx, Sy);
end;

procedure TCairoMatrix.Rotate(Radians: Double);
begin
  cairo_matrix_rotate(@FData, Radians);
end;

function TCairoMatrix.Invert: cairo_status_t;
begin
  Result := cairo_matrix_invert(@FData);
end;

procedure TCairoMatrix.Multiply(const A, B: TCairoMatrix);
begin
  cairo_matrix_multiply(@FData, @A.FData, @B.FData);
end;

procedure TCairoMatrix.MatrixTransformDistance(Dx, Dy: PDouble);
begin
  cairo_matrix_transform_distance(@FData, Dx, Dy);
end;

procedure TCairoMatrix.TransformPoint(X, Y: PDouble);
begin
  cairo_matrix_transform_point(@FData, X, Y);
end;


{ TCairoPath }

destructor TCairoPath.Destroy;
begin
  cairo_path_destroy(FPath);
end;

{ TCairoPDFSurface }

constructor TCairoPDFSurface.Create(const Filename: String; WidthInPoints,
  HeightInPoints: Double);
begin
  FHandle := cairo_pdf_surface_create(PChar(Filename), WidthInPoints, HeightInPoints);
end;

constructor TCairoPDFSurface.Create(WriteFunc: cairo_write_func_t;
  Closure: Pointer; WidthInPoints, HeightInPoints: Double);
begin
  FHandle := cairo_pdf_surface_create_for_stream(WriteFunc, Closure, WidthInPoints, HeightInPoints);
end;

procedure TCairoPDFSurface.SetSize(WidthInPoints, HeightInPoints: Double);
begin
  cairo_pdf_surface_set_size(FHandle, WidthInPoints, HeightInPoints);
end;

{ TCairoPSSurface }

constructor TCairoPSSurface.Create(const Filename: String; WidthInPoints,
  HeightInPoints: Double);
begin
  FHandle := cairo_ps_surface_create(PChar(Filename), WidthInPoints, HeightInPoints);
end;

constructor TCairoPSSurface.Create(WriteFunc: cairo_write_func_t;
  Closure: Pointer; WidthInPoints, HeightInPoints: Double);
begin
  FHandle := cairo_ps_surface_create_for_stream(WriteFunc, Closure, WidthInPoints, HeightInPoints);
end;

procedure TCairoPSSurface.DscBeginPageSetup;
begin
  cairo_ps_surface_dsc_begin_page_setup(FHandle);
end;

procedure TCairoPSSurface.DscBeginSetup;
begin
  cairo_ps_surface_dsc_begin_setup(FHandle);
end;

procedure TCairoPSSurface.DscComment(const Comment: String);
begin
  cairo_ps_surface_dsc_comment(FHandle, PChar(Comment));
end;

procedure TCairoPSSurface.SetSize(WidthInPoints, HeightInPoints: Double);
begin
  cairo_ps_surface_set_size(FHandle, WidthInPoints, HeightInPoints);
end;

{ TCairoSVGSurface }

constructor TCairoSVGSurface.Create(const Filename: String; WidthInPoints,
  HeightInPoints: Double);
begin
  FHandle := cairo_svg_surface_create(PChar(Filename), WidthInPoints, HeightInPoints);
end;

constructor TCairoSVGSurface.Create(WriteFunc: cairo_write_func_t;
  Closure: Pointer; WidthInPoints, HeightInPoints: Double);
begin
  FHandle := cairo_svg_surface_create_for_stream(WriteFunc, Closure, WidthInPoints, HeightInPoints);
end;

procedure TCairoSVGSurface.RestrictToVersion(Version: cairo_svg_version_t);
begin
  cairo_svg_surface_restrict_to_version(FHandle, Version);
end;

end.

