{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luicontrols; 

interface

uses
  ToggleLabel, MenuButton, register_luicontrols, SearchEdit, ValidateEdit, 
  WizardTypes, WizardControls, DropDownManager, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_luicontrols', @register_luicontrols.Register); 
end; 

initialization
  RegisterPackage('luicontrols', @Register); 
end.
