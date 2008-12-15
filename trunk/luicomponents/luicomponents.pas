{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit luicomponents; 

interface

uses
LuiConfig, IniConfigProvider, register_luicomponents, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_luicomponents', @register_luicomponents.Register); 
end; 

initialization
  RegisterPackage('luicomponents', @Register); 
end.
