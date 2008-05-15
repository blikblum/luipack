uses
  CairoXLib, gfx_x11, gfxbase;

type

  TX11CanvasAccess = class (TfpgCanvasImpl)
  end;
  
  TX11ApplicationAccess = class(TfpgApplicationImpl)
  end;
  
function CreateSurfaceFromCanvas(Canvas: TfpgCanvas): Pcairo_surface_t;
var
  Applicattion: TfpgApplication;
  R: TfpgRect;

begin
  Applicattion := TfpgApplication(GetApplicationAddress);
  Canvas.GetWinRect(R);
  Result := cairo_xlib_surface_create(
    Applicattion.Display,
    TX11CanvasAccess(Canvas).FDrawHandle,
    TX11ApplicationAccess(Applicattion).DefaultVisual,
    R.Width, R.Height
    );
end;
