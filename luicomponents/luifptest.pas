{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luifptest;

interface

uses
  LuiFPTestHelpers, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('luifptest', @Register);
end.
