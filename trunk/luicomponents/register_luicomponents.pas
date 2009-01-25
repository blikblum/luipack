unit register_luicomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, LuiConfig, IniConfigProvider,
  LuiOrderedDataset;

procedure Register;

implementation

procedure RegisterUnitLuiConfig;
begin
  RegisterComponents('Misc', [TLuiConfig, TIniConfigProvider]);
end;

procedure RegisterUnitLuiOrderedDataset;
begin
  RegisterComponents('Data Access', [TLuiOrderedDataset]);
end;

procedure Register;
begin
  RegisterUnit('LuiConfig', @RegisterUnitLuiConfig);
  RegisterUnit('LuiOrderedDataset', @RegisterUnitLuiOrderedDataset);
end;

end.
