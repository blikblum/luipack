
function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
begin
  Result := cairo_win32_surface_create(DC);
end;

