{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit luicairo_extras; 

interface

uses
ControlSwitcher, register_extras, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_extras', @register_extras.Register); 
end; 

initialization
  RegisterPackage('luicairo_extras', @Register); 
end.
