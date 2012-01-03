unit registervirtualdbgrid; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  VirtualDBGrid, VirtualDBCheckGroup;
  
procedure Register;

implementation

procedure RegisterUnitVirtualDbGrid;
begin
  RegisterComponents('Virtual Controls', [TVirtualDbGrid]);
end;  
procedure RegisterUnitVirtualDbCheckGroup;
begin
  RegisterComponents('Virtual Controls', [TVirtualDBCheckGroup]);
end;

procedure Register;

begin
  RegisterUnit('VirtualDbGrid',@RegisterUnitVirtualDbGrid);
  RegisterUnit('VirtualDBCheckGroup',@RegisterUnitVirtualDbCheckGroup);
end; 

initialization
{.$i ideicons.lrs}
 
end.
