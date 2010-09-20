unit register_luilclcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, PropEdits, LuiJSONLCLViews;

procedure Register;

implementation

procedure RegisterUnitLuiJSONLCLViews;
begin
  RegisterComponents('Data Controls', [TJSONObjectViewManager]);
end;

procedure Register;
begin
  RegisterUnit('LuiJSONLCLViews', @RegisterUnitLuiJSONLCLViews);
  RegisterPropertyEditor(TypeInfo(String), TJSONObjectPropertyView,
    'Options', TStringMultilinePropertyEditor);
end;

end.

