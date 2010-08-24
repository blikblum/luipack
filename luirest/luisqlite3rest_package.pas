{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LuiSqlite3REST_package; 

interface

uses
  LuiSqlite3REST, register_luisqlite3rest, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_luisqlite3rest', @register_luisqlite3rest.Register); 
end; 

initialization
  RegisterPackage('LuiSqlite3REST_package', @Register); 
end.
