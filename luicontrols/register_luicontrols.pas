unit register_luicontrols;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  ToggleLabel, MenuButton, SearchEdit, ValidateEdit, WizardControls,
  DropDownManager, DropDownButton, AdvancedLabel, VirtualPages, JSONMediators,
  JSONFormMediator;
  
procedure Register;

implementation

uses
  PresentationDescriptors, ProjectIntf;

const
  ComponentPage = 'Lui Controls';

procedure RegisterUnitToggleLabel;
begin
  RegisterComponents(ComponentPage, [TToggleLabel]);
end;  

procedure RegisterUnitMenuButton;
begin
  RegisterComponents(ComponentPage, [TMenuButton]);
end;

procedure RegisterUnitSearchEdit;
begin
  RegisterComponents(ComponentPage, [TSearchEdit]);
end;

procedure RegisterUnitValidateEdit;
begin
  RegisterComponents(ComponentPage, [TDBDateMaskEdit, TDBValidateEdit]);
end;

procedure RegisterUnitWizardControls;
begin
  RegisterComponents(ComponentPage, [TWizardManager, TWizardButtonPanel]);
end;

procedure RegisterUnitDropDownManager;
begin
  RegisterComponents(ComponentPage, [TDropDownManager]);
end;

procedure RegisterUnitDropDownButton;
begin
  RegisterComponents(ComponentPage, [TDropDownButton]);
end;

procedure RegisterUnitAdvancedLabel;
begin
  RegisterComponents(ComponentPage, [TAdvancedLabel]);
end;

procedure RegisterUnitVirtualPages;
begin
  RegisterComponents(ComponentPage, [TVirtualPageManager]);
end;

procedure RegisterUnitJSONMediators;
begin
  RegisterComponents(ComponentPage, [TJSONBooleanGroupMediator]);
end;

procedure RegisterUnitJSONFormMediator;
begin
  RegisterComponents(ComponentPage, [TJSONFormMediator]);
end;

procedure Register;

begin
  RegisterProjectFileDescriptor(TPresenterViewDescriptor.Create);
  RegisterUnit('ToggleLabel',@RegisterUnitToggleLabel);
  RegisterUnit('MenuButton',@RegisterUnitMenuButton);
  RegisterUnit('SearchEdit',@RegisterUnitSearchEdit);
  RegisterUnit('ValidateEdit',@RegisterUnitValidateEdit);
  RegisterUnit('WizardControls',@RegisterUnitWizardControls);
  RegisterUnit('DropDownManager',@RegisterUnitDropDownManager);
  RegisterUnit('DropDownButton',@RegisterUnitDropDownButton);
  RegisterUnit('AdvancedLabel',@RegisterUnitAdvancedLabel);
  RegisterUnit('VirtualPages',@RegisterUnitVirtualPages);
  RegisterUnit('JSONMediators',@RegisterUnitJSONMediators);
  RegisterUnit('JSONFormMediator',@RegisterUnitJSONFormMediator);
end; 

initialization
{.$i ideicons.lrs}
 
end.
