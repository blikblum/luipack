unit register_vtvutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazarusPackageIntf, VTJSON;

procedure Register;

implementation

procedure RegisterUnitVTJSON;
begin
  RegisterComponents('Virtual Controls', [TVirtualJSONInspector]);
end;

procedure Register;
begin
  RegisterUnit('VTJSON', @RegisterUnitVTJSON);
end;

end.

