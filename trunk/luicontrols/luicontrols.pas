{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luicontrols;

interface

uses
  ToggleLabel, register_luicontrols, SearchEdit, ValidateEdit, WizardControls, DropDownManager, 
  DropDownButton, AdvancedLabel, DropDownBaseButtons, MenuButton, VirtualPages, JSONMediators, 
  JSONBooleanRadioButtonView, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_luicontrols', @register_luicontrols.Register);
end;

initialization
  RegisterPackage('luicontrols', @Register);
end.
