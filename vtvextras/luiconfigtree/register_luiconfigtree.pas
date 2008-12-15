unit register_luiconfigtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, LuiConfigTree;

procedure Register;

implementation

procedure RegisterUnitLuiConfigTree;
begin
  RegisterComponents('Virtual Controls', [TLuiConfigTree]);
end;

procedure Register;
begin
  RegisterUnit('LuiConfigTree', @RegisterUnitLuiConfigTree);
end;

end.

