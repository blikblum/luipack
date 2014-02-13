unit LuiServices;

{$mode objfpc}{$H+}

interface

uses
  LuiIoCContainer;

function Services: TIoCContainer;

implementation

var
  _Services: TIoCContainer;

function Services: TIoCContainer;
begin
  if _Services = nil then
    _Services := TIoCContainer.Create;
  Result := _Services;
end;

finalization
  _Services.Free;

end.


