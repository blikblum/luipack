unit register_luicomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, LuiConfig, IniConfigProvider,
  LuiOrderedDataset, LuiRecordBuffer, ProjectIntf;

procedure Register;

implementation

uses
  JSONModelDescriptors, CustomMacros;

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

var
  JSONModelUnitDescriptor: TJSONModelUnitDescriptor;

procedure Register;
begin
  JSONModelUnitDescriptor := TJSONModelUnitDescriptor.Create;
  RegisterProjectFileDescriptor(JSONModelUnitDescriptor);
  RegisterUnit('LuiConfig', @RegisterUnitLuiConfig);
  RegisterUnit('LuiOrderedDataset', @RegisterUnitLuiOrderedDataset);
  RegisterUnit('LuiRecordBuffer', @RegisterUnitLuiRecordBuffer);
  CustomMacros.Register;
end;

end.
