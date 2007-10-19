begin
  snippet_normalize (cr, width, height);
  cairo_move_to (cr, 0.5, 0.1);
  cairo_line_to (cr, 0.9, 0.9);
  cairo_rel_line_to (cr, -0.4, 0.0);
  cairo_curve_to (cr, 0.2, 0.9, 0.2, 0.5, 0.5, 0.5);

  cairo_stroke (cr);
end;
