begin
  snippet_normalize (cr, width, height);
  cairo_set_line_width (cr, 0.16);
  cairo_move_to (cr, 0.3, 0.33);
  cairo_rel_line_to (cr, 0.2, -0.2);
  cairo_rel_line_to (cr, 0.2, 0.2);
  cairo_set_line_join (cr, CAIRO_LINE_JOIN_MITER); (* default *)
  cairo_stroke (cr);

  cairo_move_to (cr, 0.3, 0.63);
  cairo_rel_line_to (cr, 0.2, -0.2);
  cairo_rel_line_to (cr, 0.2, 0.2);
  cairo_set_line_join (cr, CAIRO_LINE_JOIN_BEVEL);
  cairo_stroke (cr);

  cairo_move_to (cr, 0.3, 0.93);
  cairo_rel_line_to (cr, 0.2, -0.2);
  cairo_rel_line_to (cr, 0.2, 0.2);
  cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
  cairo_stroke (cr);
end;
