unit register_luiimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf, LuiImage;

procedure Register;

implementation

procedure RegisterUnitLuiImage;
begin
  RegisterComponents('Cairo', [TLuiImage]);
end;

procedure Register;
begin
  RegisterUnit('LuiImage', @RegisterUnitLuiImage);
end;

end.

