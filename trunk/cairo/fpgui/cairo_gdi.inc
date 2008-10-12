uses
  CairoWin32, fpg_gdi;

type

  TWin32CanvasAccess = class(TfpgCanvasImpl)
  end;
  
function CreateSurfaceFromCanvas(Canvas: TfpgCanvas): Pcairo_surface_t;
begin
  Result := cairo_win32_surface_create(TWin32CanvasAccess(Canvas).DCHandle);
end;
