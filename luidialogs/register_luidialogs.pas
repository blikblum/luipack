unit register_luidialogs;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  LuiDialogs;
  
procedure Register;

implementation

procedure RegisterUnitLuiDialogs;
begin
  RegisterComponents('Dialogs', [TExportDatasetDialog]);
end;  

procedure Register;

begin
  RegisterUnit('LuiDialogs', @RegisterUnitLuiDialogs);
end; 

initialization
{.$i ideicons.lrs}
 
end.
