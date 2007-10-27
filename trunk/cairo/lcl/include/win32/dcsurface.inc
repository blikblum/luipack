
{ TCairoDCSurface }

constructor TCairoDCSurface.Create(DC: HDC);
begin
  FHandle := cairo_win32_surface_create(DC);
end;

