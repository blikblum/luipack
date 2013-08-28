unit register_vtvutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazarusPackageIntf, VTJSON, VTJSONEx;

procedure Register;

implementation

procedure RegisterUnitVTJSON;
begin
  RegisterComponents('Virtual Controls', [TVirtualJSONInspector, TVirtualJSONListView, TVirtualJSONTreeView]);
end;

procedure RegisterUnitVTJSONEx;
begin
  RegisterComponents('Virtual Controls', [TJSONQuestionTreeView]);
end;

procedure Register;
begin
  RegisterUnit('VTJSON', @RegisterUnitVTJSON);
  RegisterUnit('VTJSONEx', @RegisterUnitVTJSONEx);
end;

end.

