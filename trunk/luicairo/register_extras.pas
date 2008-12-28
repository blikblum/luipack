unit register_extras; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  LResources, LazarusPackageIntf, ControlSwitcher;

procedure Register;

implementation

procedure RegisterUnitControlSwitcher;
begin
  RegisterComponents('Cairo', [TControlSwitcher]);
end;

procedure Register;
begin
  RegisterUnit('ControlSwitcher', @RegisterUnitControlSwitcher);
end;

end.

