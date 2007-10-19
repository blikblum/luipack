var
  pat: Pcairo_pattern_t;
begin
  snippet_normalize (cr, width, height);

  pat := cairo_pattern_create_linear (0.0, 0.0,  0.0, 1.0);
  cairo_pattern_add_color_stop_rgba (pat, 1, 0, 0, 0, 1);
  cairo_pattern_add_color_stop_rgba (pat, 0, 1, 1, 1, 1);
  cairo_rectangle (cr, 0, 0, 1, 1);
  cairo_set_source (cr, pat);
  cairo_fill (cr);
  cairo_pattern_destroy (pat);

  pat := cairo_pattern_create_radial (0.45, 0.4, 0.1,
                                     0.4,  0.4, 0.5);
  cairo_pattern_add_color_stop_rgba (pat, 0, 1, 1, 1, 1);
  cairo_pattern_add_color_stop_rgba (pat, 1, 0, 0, 0, 1);
  cairo_set_source (cr, pat);
  cairo_arc (cr, 0.5, 0.5, 0.3, 0, 2 * PI);
  cairo_fill (cr);
  cairo_pattern_destroy (pat);
end;
