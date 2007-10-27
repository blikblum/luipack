{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit cairobase_package; 

interface

uses
  cairo, CairoClasses, cairo_win32, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('cairobase_package', @Register); 
end.
