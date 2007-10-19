(* BUG: Caps are added only to the last subpath in a complex path *)
begin
  snippet_normalize (cr, width, height);

  cairo_move_to (cr, 0.2, 0.3);
  cairo_line_to (cr, 0.8, 0.3);

  cairo_move_to (cr, 0.2, 0.5);
  cairo_line_to (cr, 0.8, 0.5);

  cairo_move_to (cr, 0.2, 0.7);
  cairo_line_to (cr, 0.8, 0.7);

  cairo_set_line_width (cr, 0.12);
  cairo_set_line_cap  (cr, CAIRO_LINE_CAP_ROUND);
  cairo_stroke (cr);
end;
