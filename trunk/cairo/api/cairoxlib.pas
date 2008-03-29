unit CairoXlib;

interface

uses
  cairo14, x, xlib, xrender;
  
function  cairo_xlib_surface_create(dpy: PDisplay; drawable: TDrawable; visual: PVisual; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_for_bitmap(dpy: PDisplay; bitmap: TPixmap; screen: PScreen; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_with_xrender_format(dpy: PDisplay; drawable: TDrawable; screen: PScreen; format: PXRenderPictFormat; width, height:LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_depth(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_display(surface: Pcairo_surface_t): PDisplay; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_drawable(surface: Pcairo_surface_t): TDrawable; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_height(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_screen(surface: Pcairo_surface_t): PScreen; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_visual(surface: Pcairo_surface_t): PVisual; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_width(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_size(surface: Pcairo_surface_t; width, height: LongInt); cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_drawable(surface: Pcairo_surface_t; drawable: TDrawable; width, height: LongInt); cdecl; external LIB_CAIRO;

implementation

end.

