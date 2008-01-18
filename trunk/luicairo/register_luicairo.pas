unit register_luicairo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, LuiBar, LuiImage;

procedure Register;

implementation

procedure RegisterUnitLuiBar;
begin
  RegisterComponents('Cairo', [TLuiBar]);
end;

procedure RegisterUnitLuiImage;
begin
  RegisterComponents('Cairo', [TLuiImage]);
end;

procedure Register;
begin
  RegisterUnit('LuiBar', @RegisterUnitLuiBar);
  RegisterUnit('LuiImage', @RegisterUnitLuiImage);
end;

end.

