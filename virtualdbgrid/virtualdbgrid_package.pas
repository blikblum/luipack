{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit virtualdbgrid_package; 

interface

uses
  VirtualDBGrid, registervirtualdbgrid, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registervirtualdbgrid', @registervirtualdbgrid.Register); 
end; 

initialization
  RegisterPackage('virtualdbgrid_package', @Register); 
end.
