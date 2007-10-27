{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit cairolcl_package; 

interface

uses
  register_cairolcl, CairoLCL, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('register_cairolcl', @register_cairolcl.Register); 
end; 

initialization
  RegisterPackage('cairolcl_package', @Register); 
end.
