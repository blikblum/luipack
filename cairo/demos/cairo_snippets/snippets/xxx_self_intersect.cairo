(* the minimally different shade of the right part of the bar
   is an artifact of the self intersect bug described in BUGS *)
begin
  snippet_normalize (cr, width, height);

  cairo_move_to (cr, 0.3, 0.3);
  cairo_line_to (cr, 0.7, 0.3);

  cairo_line_to (cr, 0.5, 0.3);
  cairo_line_to (cr, 0.5, 0.7);

  cairo_set_line_width (cr, 0.22);
  cairo_set_line_cap  (cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_line_join  (cr, CAIRO_LINE_JOIN_ROUND);
  cairo_stroke (cr);
end;
