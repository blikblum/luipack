
procedure TCairoLCLFont.CreateHandle;
begin
end;

function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
var
  DeviceContext: TQtDeviceContext absolute DC;
  X11Info: QX11InfoH;
  Format: cairo_format_t;
begin
  if QtWidgetSet.IsValidDC(DC) then
  with DeviceContext do
  begin
    //todo handle win32
    if Parent <> nil then
    begin
      X11Info := QWidget_x11Info(Parent);
      Result := cairo_xlib_surface_create(
        QX11Info_display,
        QWidget_winId(Parent),
        QX11Info_visual(X11Info),
        QWidget_width(Parent), QWidget_height(Parent)
        );
      QX11Info_destroy(X11Info);
    end
    else
    begin
      if vImage = nil then
        Exception.Create('CreateSurfaceFromDC (Qt): Unable to create a surface');
      case QImage_format(vImage.Handle) of
        QImageFormat_Mono:
          Format := CAIRO_FORMAT_A1;
        QImageFormat_Indexed8:
          Format := CAIRO_FORMAT_A8;
        QImageFormat_RGB32:
          Format := CAIRO_FORMAT_RGB24;
        QImageFormat_ARGB32, QImageFormat_ARGB32_Premultiplied:
          //todo: handle not premultiplied alpha format
          Format := CAIRO_FORMAT_ARGB32;
        else
           Exception.Create('CreateSurfaceFromDC (Qt): Unsupported image format');
      end;
      Result := cairo_image_surface_create_for_data(
        vImage.bits,
        Format,
        vImage.width,
        vImage.height,
        QImage_bytesPerLine(vImage.Handle));
    end;
  end;
end;
