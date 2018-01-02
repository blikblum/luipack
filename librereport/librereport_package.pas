{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit librereport_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  LibreReport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('librereport_package', @Register);
end.
