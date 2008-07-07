unit register_luicairo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, LuiBar;

procedure Register;

implementation

procedure RegisterUnitLuiBar;
begin
  RegisterComponents('Cairo', [TLuiBar]);
end;

procedure Register;
begin
  RegisterUnit('LuiBar', @RegisterUnitLuiBar);
end;

end.

