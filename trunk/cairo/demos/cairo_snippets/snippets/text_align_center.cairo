const
  utf8 = 'cairo';
var
  extents: cairo_text_extents_t;
  x,y: Double;
begin
  snippet_normalize (cr, width, height);

  cairo_select_font_face (cr, 'Sans',
      CAIRO_FONT_SLANT_NORMAL,
      CAIRO_FONT_WEIGHT_NORMAL);

  cairo_set_font_size (cr, 0.2);
  cairo_text_extents (cr, utf8, @extents);
  x := 0.5-(extents.width/2 + extents.x_bearing);
  y := 0.5-(extents.height/2 + extents.y_bearing);

  cairo_move_to (cr, x, y);
  cairo_show_text (cr, utf8);

  (* draw helping lines *)
  cairo_set_source_rgba (cr, 1,0.2,0.2, 0.6);
  cairo_arc (cr, x, y, 0.05, 0, 2*PI);
  cairo_fill (cr);
  cairo_move_to (cr, 0.5, 0);
  cairo_rel_line_to (cr, 0, 1);
  cairo_move_to (cr, 0, 0.5);
  cairo_rel_line_to (cr, 1, 0);
  cairo_stroke (cr);
end;
