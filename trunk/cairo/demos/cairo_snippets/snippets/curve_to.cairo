var
  x, x1, x2, x3: Double;
  y, y1, y2, y3: Double;
begin
  x := 0.1;
  y := 0.5;
  x1 := 0.4;
  y1 := 0.9;
  x2 := 0.6;
  y2 := 0.1;
  x3 := 0.9;
  y3 := 0.5;

  snippet_normalize (cr, width, height);

  cairo_move_to (cr,  x, y);
  cairo_curve_to (cr, x1, y1, x2, y2, x3, y3);

  cairo_stroke (cr);

  cairo_set_source_rgba (cr, 1,0.2,0.2,0.6);
  cairo_set_line_width (cr, 0.03);
  cairo_move_to (cr,x,y);   cairo_line_to (cr,x1,y1);
  cairo_move_to (cr,x2,y2); cairo_line_to (cr,x3,y3);
  cairo_stroke (cr);
end;
