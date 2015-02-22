{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit handlebars_package;

interface

uses
  Handlebars, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('handlebars_package', @Register);
end.
