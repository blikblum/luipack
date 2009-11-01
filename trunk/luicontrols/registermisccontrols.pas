unit registermisccontrols; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  ToggleLabel, MenuButton, SearchEdit, DateEdit;
  
procedure Register;

implementation

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

procedure RegisterUnitDateEdit;
begin
  RegisterComponents(ComponentPage, [TDBDateMaskEdit]);
end;



procedure Register;

begin
  RegisterUnit('ToggleLabel',@RegisterUnitToggleLabel);
  RegisterUnit('MenuButton',@RegisterUnitMenuButton);
  RegisterUnit('SearchEdit',@RegisterUnitSearchEdit);
  RegisterUnit('DateEdit',@RegisterUnitDateEdit);
end; 

initialization
{.$i ideicons.lrs}
 
end.
