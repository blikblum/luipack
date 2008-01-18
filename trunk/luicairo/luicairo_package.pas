{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit luicairo_package; 

interface

uses
  LuiBar, register_luicairo, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('register_luicairo', @register_luicairo.Register); 
end; 

initialization
  RegisterPackage('luicairo_package', @Register); 
end.
