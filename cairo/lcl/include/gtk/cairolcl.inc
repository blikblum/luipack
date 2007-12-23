
function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
var
  DeviceContext: TGtkDeviceContext absolute DC;
  Width, Height: Integer;
  Visual: PGdkVisual;
begin
  if GtkWidgetSet.IsValidDC(DC) then
  with DeviceContext do
  begin
    gdk_window_get_size(PGdkWindow(Drawable), @Width, @Height);
    Visual := gdk_visual_get_system;
    Result := cairo_xlib_surface_create(
      GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(Drawable)),
      GDK_WINDOW_XWINDOW(PGdkWindowPrivate(Drawable)),
      GDK_VISUAL_XVISUAL(PGdkVisualPrivate(Visual)),
      Width, Height
      );
  end;
end;

