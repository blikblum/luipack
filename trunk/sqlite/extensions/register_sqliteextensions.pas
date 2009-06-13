unit register_sqliteextensions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, SqliteUtils;

procedure Register;

implementation

procedure RegisterUnitSqliteUtils;
begin
  RegisterComponents('Data Access', [TSqliteQueryBuilder]);
end;

procedure Register;
begin
  RegisterUnit('SqliteUtils', @RegisterUnitSqliteUtils);
end;

end.

