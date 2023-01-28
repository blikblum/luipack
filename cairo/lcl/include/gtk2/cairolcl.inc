uses
  gtk2def, Gtk2Int, CairoXlib, gdk2, gdk2x, Gtk2WSControls, WSLCLClasses;
  
procedure TCairoLCLFont.CreateHandle;
begin
end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
var
  DeviceContext: TGtkDeviceContext absolute DC;
  Width, Height: Integer;
  Visual: PGdkVisual;
begin
  if GTK2WidgetSet.IsValidDC(DC) then
  with DeviceContext do
  begin
    gdk_window_get_size(PGdkWindow(Drawable), @Width, @Height);
    Visual := gdk_visual_get_system;
    Result := cairo_xlib_surface_create(
      gdk_x11_drawable_get_xdisplay(Drawable),
      gdk_x11_drawable_get_xid(Drawable),
      gdk_x11_visual_get_xvisual(Visual),
      Width, Height
      );
  end;
end;

