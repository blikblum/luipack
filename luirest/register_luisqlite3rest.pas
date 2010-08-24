unit register_luisqlite3rest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, LuiSqlite3REST;

procedure Register;

implementation

procedure RegisterUnitLuiSqlite3REST;
begin
  RegisterComponents('Data Access', [TSqlite3RESTDatabase]);
end;

procedure Register;
begin
  RegisterUnit('LuiSqlite3REST', @RegisterUnitLuiSqlite3REST);
end;

end.

