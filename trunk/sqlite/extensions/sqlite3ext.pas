unit Sqlite3Ext;

{$mode objfpc}{$H+}

interface

function sqlite3_enable_shared_cache(Enable: Boolean): LongInt; cdecl; external 'sqlite3';

implementation

end.

