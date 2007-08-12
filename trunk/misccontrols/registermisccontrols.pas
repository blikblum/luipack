unit registermisccontrols; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  ToggleLabel, MenuButton;
  
procedure Register;

implementation

procedure RegisterUnitToggleLabel;
begin
  RegisterComponents('Misc', [TToggleLabel]);
end;  

procedure RegisterUnitMenuButton;
begin
  RegisterComponents('Misc', [TMenuButton]);
end;


procedure Register;

begin
  RegisterUnit('ToggleLabel',@RegisterUnitToggleLabel);
  RegisterUnit('MenuButton',@RegisterUnitMenuButton);
end; 

initialization
{.$i ideicons.lrs}
 
end.
