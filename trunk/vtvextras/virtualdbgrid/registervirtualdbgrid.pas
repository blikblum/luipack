unit registervirtualdbgrid; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  VirtualDBGrid;
  
procedure Register;

implementation

procedure RegisterUnitVirtualDbGrid;
begin
  RegisterComponents('Virtual Controls', [TVirtualDbGrid]);
end;  

procedure Register;

begin
  RegisterUnit('VirtualDbGrid',@RegisterUnitVirtualDbGrid);
end; 

initialization
{.$i ideicons.lrs}
 
end.
