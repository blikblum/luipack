{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit sqliteextensions_package; 

interface

uses
  SqliteTrees, Sqlite3Wrapper, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('sqliteextensions_package', @Register); 
end.
