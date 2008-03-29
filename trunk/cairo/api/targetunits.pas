unit TargetUnits;

{$mode objfpc}{$H+}

interface

uses
{$ifdef windows}
  CairoWin32;
{$endif}

{$ifdef unix}
  CairoXlib;
{$endif}

implementation

end.

