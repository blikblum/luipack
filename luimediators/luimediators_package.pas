{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luimediators_package;

interface

uses
  JSONCtrlGridMediator, luimediator_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('luimediator_register', @luimediator_register.Register);
end;

initialization
  RegisterPackage('luimediators_package', @Register);
end.
