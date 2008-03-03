unit TargetUnits;

{$mode objfpc}{$H+}

interface

uses
{$ifdef windows}
  CairoWin32;
{$endif}

{$ifdef unix}
  cairo_xlib;
{$endif}

implementation

end.

