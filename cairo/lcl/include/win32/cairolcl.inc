
function CreateFontFromLog(const LogFont: TLogFont): Pcairo_font_face_t;
begin
end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
begin
  Result := cairo_win32_surface_create(DC);
end;

