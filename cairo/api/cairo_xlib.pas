unit cairo_xlib;

interface

uses
  cairo, x, xlib, xrender;
  
function  cairo_xlib_surface_create(dpy:PDisplay; drawable:TDrawable; visual:PVisual; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_for_bitmap(dpy:PDisplay; bitmap:TPixmap; screen:PScreen; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_size(surface:Pcairo_surface_t; width:longint; height:longint); cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_drawable(surface:Pcairo_surface_t; drawable:TDrawable; width:longint; height:longint); cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_with_xrender_format(dpy:PDisplay; drawable:TDrawable; screen:PScreen; format:PXRenderPictFormat; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;


implementation

end.

