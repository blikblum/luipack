unit register_luicomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, LuiConfig, IniConfigProvider;

procedure Register;

implementation

procedure RegisterUnitLuiConfig;
begin
  RegisterComponents('Misc', [TLuiConfig, TIniConfigProvider]);
end;

procedure Register;
begin
  RegisterUnit('LuiConfig', @RegisterUnitLuiConfig);
end;

end.
