unit register_luicomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, LuiConfig, IniConfigProvider,
  LuiOrderedDataset, LuiRecordBuffer;

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

procedure RegisterUnitLuiRecordBuffer;
begin
  RegisterComponents('Data Access', [TLuiRecordBuffer]);
end;

procedure Register;
begin
  RegisterUnit('LuiConfig', @RegisterUnitLuiConfig);
  RegisterUnit('LuiOrderedDataset', @RegisterUnitLuiOrderedDataset);
  RegisterUnit('LuiRecordBuffer', @RegisterUnitLuiRecordBuffer);
end;

end.
