unit rsvg;

{$mode objfpc}{$H+}

interface

uses
  glib2, gdk2pixbuf, Cairo;
  
const
  RSVG_LIBNAME = 'rsvg-2';

type
  GQuark = TGQuark;
  GObject = TGObject;
  GObjectClass = TGObjectClass;
  
  RsvgError = (RSVG_ERROR_FAILED);
  
  RsvgHandlePrivate = record end;
  
  RsvgHandleClass = record
    parent : GObjectClass;
    _abi_padding : array[0..14] of gpointer;
  end;

  RsvgHandle = record
    parent : GObject;
    priv : ^RsvgHandlePrivate;
    _abi_padding : array[0..14] of gpointer;
  end;
  PRsvgHandle = ^RsvgHandle;
  
  RsvgDimensionData = record
    width : longint;
    height : longint;
    em : gdouble;
    ex : gdouble;
  end;
  PRsvgDimensionData = ^RsvgDimensionData;
  

//todo:
{
#define RSVG_TYPE_HANDLE                  (rsvg_handle_get_type ())
#define RSVG_HANDLE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), RSVG_TYPE_HANDLE, RsvgHandle))
#define RSVG_HANDLE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), RSVG_TYPE_HANDLE, RsvgHandleClass))
#define RSVG_IS_HANDLE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), RSVG_TYPE_HANDLE))
#define RSVG_IS_HANDLE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), RSVG_TYPE_HANDLE))
#define RSVG_HANDLE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), RSVG_TYPE_HANDLE, RsvgHandleClass))
}
  
function rsvg_handle_get_type: GType; cdecl; external RSVG_LIBNAME;

function rsvg_error_quark: GQuark; cdecl; external RSVG_LIBNAME;

procedure rsvg_init; cdecl; external RSVG_LIBNAME;

procedure rsvg_term; cdecl; external RSVG_LIBNAME;

procedure rsvg_set_default_dpi(dpi: Double); cdecl; external RSVG_LIBNAME;

procedure rsvg_set_default_dpi_x_y(dpi_x, dpi_y: Double); cdecl; external RSVG_LIBNAME;

procedure rsvg_handle_set_dpi(handle: PRsvgHandle; dpi: Double); cdecl; external RSVG_LIBNAME;

procedure rsvg_handle_set_dpi_x_y(handle: PRsvgHandle; dpi_x, dpi_y: Double); cdecl; external RSVG_LIBNAME;

function rsvg_handle_new: PRsvgHandle; cdecl; external RSVG_LIBNAME;

function rsvg_handle_write(handle: PRsvgHandle; buf: pguchar; count: gsize; error: PPGError): gboolean; cdecl; external RSVG_LIBNAME;

function rsvg_handle_close(handle: PRsvgHandle; error: PPGError): gboolean; cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_pixbuf(handle: PRsvgHandle): PGdkPixbuf; cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_pixbuf_sub(handle: PRsvgHandle; id: PChar): PGdkPixbuf; cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_base_uri(handle: PRsvgHandle): PChar; cdecl; external RSVG_LIBNAME;

procedure rsvg_handle_set_base_uri(handle: PRsvgHandle; base_uri: PChar); cdecl; external RSVG_LIBNAME;

procedure rsvg_handle_get_dimensions(handle: PRsvgHandle; dimension_data: PRsvgDimensionData); cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_title(handle: PRsvgHandle): PChar; cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_desc(handle: PRsvgHandle): PChar; cdecl; external RSVG_LIBNAME;

function rsvg_handle_get_metadata(handle: PRsvgHandle): PChar; cdecl; external RSVG_LIBNAME;

function rsvg_handle_new_from_data(data: pguint8; data_len: gsize; error: PPGError): PRsvgHandle; cdecl; external RSVG_LIBNAME;

function rsvg_handle_new_from_file(file_name: pgchar; error: PPGError):PRsvgHandle; cdecl; external RSVG_LIBNAME;

//rsvg-cairo functions

procedure rsvg_handle_render_cairo(handle: PRsvgHandle; cr: PCairo_t); cdecl; external RSVG_LIBNAME;

procedure rsvg_handle_render_cairo_sub(handle: PRsvgHandle; cr: PCairo_t; id: PChar); cdecl; external RSVG_LIBNAME;


implementation

end.

