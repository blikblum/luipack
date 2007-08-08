{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit misccontrols_package; 

interface

uses
  togglelabel, registermisccontrols, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registermisccontrols', @registermisccontrols.Register); 
end; 

initialization
  RegisterPackage('misccontrols_package', @Register); 
end.
