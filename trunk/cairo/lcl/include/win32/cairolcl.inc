
function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
begin
  Result := cairo_win32_surface_create(DC);
end;

function ColorToCairoColor(Color: TColor): TCairoColor;
begin
  Result := RGBToCairoColor(ColorToRGB(Color));
end;

