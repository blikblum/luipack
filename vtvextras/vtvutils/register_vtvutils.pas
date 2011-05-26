unit register_vtvutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazarusPackageIntf, VTJSON;

procedure Register;

implementation

procedure RegisterUnitVTJSON;
begin
  RegisterComponents('Virtual Controls', [TVirtualJSONInspector, TVirtualJSONListView]);
end;

procedure Register;
begin
  RegisterUnit('VTJSON', @RegisterUnitVTJSON);
end;

end.

