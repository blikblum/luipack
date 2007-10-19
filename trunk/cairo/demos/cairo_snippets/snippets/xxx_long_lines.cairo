begin
  snippet_normalize (cr, width, height);

  cairo_move_to (cr, 0.1, -50);
  cairo_line_to (cr, 0.1,  50);
  cairo_set_source_rgb (cr, 1, 0 ,0);
  cairo_stroke (cr);

  cairo_move_to (cr, 0.2, -60);
  cairo_line_to (cr, 0.2,  60);
  cairo_set_source_rgb (cr, 1, 1 ,0);
  cairo_stroke (cr);

  cairo_move_to (cr, 0.3, -70);
  cairo_line_to (cr, 0.3,  70);
  cairo_set_source_rgb (cr, 0, 1 ,0);
  cairo_stroke (cr);

  cairo_move_to (cr, 0.4, -80);
  cairo_line_to (cr, 0.4,  80);
  cairo_set_source_rgb (cr, 0, 0 ,1);
  cairo_stroke (cr);
end;
