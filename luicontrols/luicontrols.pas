{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit luicontrols; 

interface

uses
  ToggleLabel, MenuButton, registermisccontrols, SearchEdit, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registermisccontrols', @registermisccontrols.Register); 
end; 

initialization
  RegisterPackage('luicontrols', @Register); 
end.
