begin
  snippet_normalize (cr, width, height);
  snippet_set_bg_svg (cr, 'data/freedesktop.svg');
  cairo_set_operator (cr, CAIRO_OPERATOR_DEST_ATOP);
  cairo_set_source_rgba (cr, 1,0,0,0.5);
  cairo_rectangle (cr, 0.2,0.2, 0.5,0.5);
  cairo_fill (cr);
  cairo_set_source_rgb (cr, 0,1,0);
  cairo_rectangle (cr, 0.4,0.4, 0.4,0.4);
  cairo_fill (cr);
  cairo_set_source_rgb (cr, 0,0,1);
  cairo_rectangle (cr, 0.6,0.6, 0.3,0.3);
  cairo_fill (cr);
end;
