var
  w, h: Integer;
  image: Pcairo_surface_t;
  pattern: Pcairo_pattern_t;
  matrix: cairo_matrix_t;

begin
  snippet_normalize (cr, width, height);

  image := cairo_image_surface_create_from_png ('data/romedalen.png');
  w := cairo_image_surface_get_width (image);
  h := cairo_image_surface_get_height (image);

  pattern := cairo_pattern_create_for_surface (image);
  cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);

  cairo_translate (cr, 0.5, 0.5);
  cairo_rotate (cr, PI / 4);
  cairo_scale (cr, 1 / sqrt (2), 1 / sqrt (2));
  cairo_translate (cr, - 0.5, - 0.5);

  //cairo_matrix_init_scale (@matrix, w * 5., h * 5.); <- original
  cairo_matrix_init_scale (@matrix, w * 5, h * 5);
  cairo_pattern_set_matrix (pattern, @matrix);

  cairo_set_source (cr, pattern);

  cairo_rectangle (cr, 0, 0, 1.0, 1.0);
  cairo_fill (cr);

  cairo_pattern_destroy (pattern);
  cairo_surface_destroy (image);
end;
