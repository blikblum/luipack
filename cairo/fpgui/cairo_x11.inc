uses
  CairoXLib, fpg_x11;

type

  TX11CanvasAccess = class (TfpgCanvasImpl)
  end;
  
  TX11ApplicationAccess = class(TfpgApplicationImpl)
  end;
  
function CreateSurfaceFromCanvas(Canvas: TfpgCanvas): Pcairo_surface_t;
var
  R: TfpgRect;
begin
  Canvas.GetWinRect(R);
  Result := cairo_xlib_surface_create(
    fpgApplication.Display,
    TX11CanvasAccess(Canvas).DCHandle,
    TX11ApplicationAccess(fpgApplication).DefaultVisual,
    R.Width, R.Height
    );
end;
