{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit luicontrols; 

interface

uses
    ToggleLabel, MenuButton, registermisccontrols, SearchEdit, ValidateEdit, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('registermisccontrols', @registermisccontrols.Register); 
end; 

initialization
  RegisterPackage('luicontrols', @Register); 
end.
