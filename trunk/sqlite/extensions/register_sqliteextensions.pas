unit register_sqliteextensions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, SqliteUtils, Sqlite3Wrapper;

procedure Register;

implementation

procedure RegisterUnitSqliteUtils;
begin
  RegisterComponents('Data Access', [TSqliteQueryBuilder]);
end;

procedure RegisterUnitSqlite3Wrapper;
begin
  RegisterComponents('Data Access', [TSqlite3Database]);
end;


procedure Register;
begin
  RegisterUnit('SqliteUtils', @RegisterUnitSqliteUtils);
  RegisterUnit('Sqlite3Wrapper', @RegisterUnitSqlite3Wrapper);
end;

end.

