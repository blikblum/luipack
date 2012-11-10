{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LuiLCLComponents;

interface

uses
  LuiLCLMessages, LuiJSONLCLViews, register_luilclcomponents, LuiLCLInterfaces, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_luilclcomponents', @register_luilclcomponents.Register);
end;

initialization
  RegisterPackage('LuiLCLComponents', @Register);
end.
