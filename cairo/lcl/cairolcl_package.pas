{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit cairolcl_package; 

interface

uses
  CairoLCL, register_cairolcl, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('register_cairolcl', @register_cairolcl.Register); 
end; 

initialization
  RegisterPackage('cairolcl_package', @Register); 
end.
