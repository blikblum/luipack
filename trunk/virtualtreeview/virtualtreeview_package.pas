{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit virtualtreeview_package; 

interface

uses
  VirtualTrees, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('VirtualTrees', @VirtualTrees.Register); 
end; 

initialization
  RegisterPackage('virtualtreeview_package', @Register); 
end.
