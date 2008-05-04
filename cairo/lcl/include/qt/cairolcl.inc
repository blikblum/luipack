
procedure TCairoLCLFont.CreateHandle;
begin
end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
var
  DeviceContext: TQtDeviceContext absolute DC;
  X11Info: QX11InfoH;
begin
  if QtWidgetSet.IsValidDC(DC) then
  with DeviceContext do
  begin
    X11Info := QWidget_x11Info(Parent);
    Result := cairo_xlib_surface_create(
      QX11Info_display,
      QWidget_winId(Parent),
      QX11Info_visual(X11Info),
      QWidget_width(Parent), QWidget_height(Parent)
      );
    QX11Info_destroy(X11Info);
  end;
end;
