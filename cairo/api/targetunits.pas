unit targetunits;

{$mode objfpc}{$H+}

interface

uses
{$ifdef windows}
  cairo_win32;
{$endif}

{$ifdef unix}
  cairo_xlib;
{$endif}

implementation

end.

