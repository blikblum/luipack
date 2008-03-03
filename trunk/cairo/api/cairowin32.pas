unit CairoWin32;

interface

uses
  cairo14, windows;

{
 Translation of cairo-win32.h version 1.4
 by Luiz Américo Pereira Câmara 2007
}


function cairo_win32_surface_create(hdc: HDC): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function cairo_win32_surface_create_with_ddb(hdc: HDC; format: cairo_format_t; width, height: longint): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function cairo_win32_surface_create_with_dib(format: cairo_format_t; width, height: longint): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function cairo_win32_surface_get_dc(surface: pcairo_surface_t): HDC; cdecl; external LIB_CAIRO;
function cairo_win32_surface_get_image(surface: pcairo_surface_t): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function cairo_win32_font_face_create_for_logfontw(logfont: pLOGFONTW): Pcairo_font_face_t; cdecl; external LIB_CAIRO;
function cairo_win32_font_face_create_for_hfont(font: HFONT): Pcairo_font_face_t; cdecl; external LIB_CAIRO;
function cairo_win32_scaled_font_select_font(scaled_font: pcairo_scaled_font_t; hdc: HDC): cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_win32_scaled_font_done_font(scaled_font: pcairo_scaled_font_t); cdecl; external LIB_CAIRO;
function cairo_win32_scaled_font_get_metrics_factor(scaled_font: pcairo_scaled_font_t): double; cdecl; external LIB_CAIRO;
procedure cairo_win32_scaled_font_get_logical_to_device(scaled_font: pcairo_scaled_font_t; logical_to_device: pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_win32_scaled_font_get_device_to_logical(scaled_font: pcairo_scaled_font_t; device_to_logical: pcairo_matrix_t); cdecl; external LIB_CAIRO;

implementation


end.
