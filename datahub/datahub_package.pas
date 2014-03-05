{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit datahub_package;

interface

uses
  DataHubProject, DataModel, DataView, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('datahub_package', @Register);
end.
