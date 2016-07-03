{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit handlebars_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  Handlebars, HandlebarsScanner, HandlebarsParser, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('handlebars_package', @Register);
end.
