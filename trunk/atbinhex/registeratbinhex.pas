unit registeratbinhex; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  ATBinHex;
  
procedure Register;

implementation

procedure RegisterUnitATBinHex;
begin
  RegisterComponents('ATBinHex', [TATBinHex]);
end;  

procedure Register;

begin
  RegisterUnit('ATBinHex',@RegisterUnitATBinHex);
end; 

initialization
{$i ideicons.lrs}
 
end.
