var
  xc: Double;
  yc: Double;
  radius: Double;
  angle1: Double;
  angle2: Double;
  OldMask: TFPUExceptionMask;
begin
  //workaround to a cairo bug (fixed post 1.6.4)
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exPrecision]);

  xc := 0.5;
  yc := 0.5;
  radius := 0.4;
  angle1 := 45.0  * (PI/180.0);  (* angles are specified *)
  angle2 := 180.0 * (PI/180.0);  (* in radians           *)

  snippet_normalize (cr, width, height);

  cairo_arc (cr, xc, yc, radius, angle1, angle2);
  cairo_stroke (cr);

  (* draw helping lines *)
  cairo_set_source_rgba (cr, 1,0.2,0.2,0.6);
  cairo_arc (cr, xc, yc, 0.05, 0, 2*PI);
  cairo_fill (cr);
  cairo_set_line_width (cr, 0.03);
  cairo_arc (cr, xc, yc, radius, angle1, angle1);
  cairo_line_to (cr, xc, yc);
  cairo_arc (cr, xc, yc, radius, angle2, angle2);
  cairo_line_to (cr, xc, yc);
  cairo_stroke (cr);

  SetExceptionMask(OldMask);
end;
