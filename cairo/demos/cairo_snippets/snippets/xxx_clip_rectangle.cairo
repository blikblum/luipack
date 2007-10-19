(* This is intended to test the rectangle-based clipping support in
 * cairo.  On 2004-08-03, we noticed a bug in which this clipping
 * wasn't happening at all, so we disabled it in
 * cairo_gstate.c:extract_transformed_rectangle.
 *
 * When that works again, and is re-enabled, this test can be renamed
 * without the xxx_.
 *)
begin
  snippet_normalize (cr, width, height);

  cairo_new_path (cr);
  {
  cairo_move_to (cr, .25, .25);
  cairo_line_to (cr, .25, .75);
  cairo_line_to (cr, .75, .75);
  cairo_line_to (cr, .75, .25);
  cairo_line_to (cr, .25, .25);
  }
  cairo_move_to (cr, 0.25, 0.25);
  cairo_line_to (cr, 0.25, 0.75);
  cairo_line_to (cr, 0.75, 0.75);
  cairo_line_to (cr, 0.75, 0.25);
  cairo_line_to (cr, 0.25, 0.25);
  cairo_close_path (cr);

  cairo_clip (cr);

  cairo_move_to (cr, 0, 0);
  cairo_line_to (cr, 1, 1);
  cairo_stroke (cr);
end;
