unit MyServiceDummyImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyService;

type

  { TMyServiceDummyImplementation }

  TMyServiceDummyImplementation = class(TInterfacedObject, IMyService)
  public
    function GetDescription: String;
  end;

  { TMySecondServiceDummyComponent }

  TMySecondServiceDummyComponent = class(TComponent, IMySecondService)
  public
    function GetDescription: String;
  end;


implementation

{ TMySecondServiceDummyComponent }

function TMySecondServiceDummyComponent.GetDescription: String;
var
  IntfObj: TInterfacedObject;
begin
  IntfObj := ComObject as TInterfacedObject;
  Result := Format('Implemented by TMySecondServiceDummyComponent - RefCount: %d', [IntfObj.RefCount]);
end;

{ TMyServiceDummyImplementation }

function TMyServiceDummyImplementation.GetDescription: String;
begin
  Result := Format('Implemented by TMyServiceDummyImplementation - RefCount: %d', [RefCount]);
end;

end.

