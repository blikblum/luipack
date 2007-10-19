var
  svgc: Psvg_cairo_t;
  w, h: LongWord;
begin
  snippet_normalize (cr, width, height);

  svg_cairo_create (@svgc);

  svg_cairo_parse (svgc, 'data/home.svg');
  svg_cairo_get_size (svgc, @w, @h);
  cairo_scale (cr, 1.0/w, 1.0/h);
  svg_cairo_render (svgc, cr);

  svg_cairo_destroy (svgc);
end;
