{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit luiimage_package; 

interface

uses
  LuiImage, register_luiimage, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('register_luiimage', @register_luiimage.Register); 
end; 

initialization
  RegisterPackage('luiimage_package', @Register); 
end.
