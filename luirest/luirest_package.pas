{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luirest_package;

interface

uses
  LuiRESTServer, register_luirest, LuiRESTFastCGI, LuiRESTCGI, LuiRESTSqldb, 
  LuiRESTClient, LuiRESTSwagger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_luirest', @register_luirest.Register);
end;

initialization
  RegisterPackage('luirest_package', @Register);
end.
