{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit chronolog_package; 

interface

uses
  ChronoLog, chrono2db, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('chronolog_package', @Register); 
end.
