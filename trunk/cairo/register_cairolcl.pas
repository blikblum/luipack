unit register_cairolcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, CairoLCL;

procedure Register;

implementation

procedure RegisterUnitCairoLCL;
begin
  RegisterComponents('Cairo', [TCairoPaintBox]);
end;

procedure Register;

begin
  RegisterUnit('CairoLCL', @RegisterUnitCairoLCL);
end;

end.

