{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit sqliteextensions_package; 

interface

uses
    SqliteTrees, Sqlite3Wrapper, Sqlite3Ext, SqliteUtils, 
  register_sqliteextensions, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_sqliteextensions', @register_sqliteextensions.Register
    ); 
end; 

initialization
  RegisterPackage('sqliteextensions_package', @Register); 
end.
