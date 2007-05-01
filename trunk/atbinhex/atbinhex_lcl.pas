{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit atbinhex_lcl; 

interface

uses
  ATBinHex, registeratbinhex, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registeratbinhex', @registeratbinhex.Register); 
end; 

initialization
  RegisterPackage('atbinhex_lcl', @Register); 
end.
