unit cairo_ft; 

{$mode objfpc}

interface

uses
  cairo14, freetypeh;
  
  
function  cairo_ft_font_face_create_for_pattern(pattern:PFcPattern):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_ft_font_options_substitute(options:Pcairo_font_options_t; pattern:PFcPattern); cdecl; external LIB_CAIRO;
function  cairo_ft_font_face_create_for_ft_face(face:TFT_Face; load_flags:longint):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
function  cairo_ft_scaled_font_lock_face(scaled_font:Pcairo_scaled_font_t):TFT_Face; cdecl; external LIB_CAIRO;
procedure cairo_ft_scaled_font_unlock_face(scaled_font:Pcairo_scaled_font_t); cdecl; external LIB_CAIRO;


implementation

end.

