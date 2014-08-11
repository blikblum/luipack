unit Sqlite3Ext;

{$mode objfpc}{$H+}

interface

uses
  ctypes, sqlite3;

procedure sqlite3_result_int(ctx: psqlite3_context; V: cint); cdecl; external Sqlite3Lib;
procedure sqlite3_result_int64(ctx: psqlite3_context; V: sqlite3_int64); cdecl; external Sqlite3Lib;

implementation

end.

