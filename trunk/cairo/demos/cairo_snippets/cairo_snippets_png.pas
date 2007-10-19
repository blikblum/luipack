program cairo_snippets_png;
{
 png frontend for cairo_snippets
 (c) Øyvind Kolås 2004, placed in the public domain
}
{$mode objfpc}
{$H+} 
//converted to pascal by Luiz Américo

uses
  snippets, cairo, SysUtils;

const  
  IMAGE_WIDTH = 256;
  IMAGE_HEIGHT = 256;
  LINE_WIDTH = 0.04;
	
(* process a snippet, writing it out to file *)

procedure snippet_do_png (no: Integer);
var
  cr: PCairo_t;
  surface: Pcairo_surface_t;
  filename: string;
begin
  WriteLn(Format('processing %s', [snippet_name[no]]));
	surface := cairo_image_surface_create (CAIRO_FORMAT_ARGB32, IMAGE_WIDTH, IMAGE_HEIGHT);
  cr := cairo_create (surface);
  
  cairo_save (cr);
  snippet_do (cr, no, IMAGE_WIDTH, IMAGE_HEIGHT);
  cairo_restore (cr);
  
  filename := Format('%s.png', [snippet_name [no]]);
  cairo_surface_write_to_png (surface, PChar(filename));
    
  cairo_destroy (cr);
	cairo_surface_destroy (surface);
end;

var
  i: Integer;
begin
	for i := 0 to snippet_count - 1 do
		snippet_do_png (i);
end.
