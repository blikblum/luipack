{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LuiSqliteComponents;

interface

uses
  LuiDataCache, LuiSqlite3Client, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LuiSqliteComponents', @Register);
end.
