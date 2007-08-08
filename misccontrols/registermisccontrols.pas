unit registermisccontrols; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  togglelabel;
  
procedure Register;

implementation

procedure RegisterUnitToggleLabel;
begin
  RegisterComponents('Misc', [TToggleLabel]);
end;  

procedure Register;

begin
  RegisterUnit('togglelabel',@RegisterUnitToggleLabel);
end; 

initialization
{.$i ideicons.lrs}
 
end.