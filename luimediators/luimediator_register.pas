  unit luimediator_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, JSONCtrlGridMediator;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mediators', [TJSONCtrlGridMediator]);
end;


end.

