unit EventResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTSqldb, HTTPDefs;

type

  { TEventsResource }

  TEventsResource = class(TSqldbCollectionResource)
  protected
    class function GetItemParam: String; override;
  public
    constructor Create; override;
  end;

  { TActiveEventsResource }

  TActiveEventsResource = class(TEventsResource)
  public
    constructor Create; override;
  end;


implementation

{ TActiveEventsResource }

constructor TActiveEventsResource.Create;
begin
  inherited Create;
  ConditionsSQL := 'where Active = 1 order by Date DESC';
end;

{ TEventsResource }

class function TEventsResource.GetItemParam: String;
begin
  Result := 'eventid';
end;

constructor TEventsResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select * from Event';
  ConditionsSQL := 'order by Date DESC';
  RegisterSubPath('active', TActiveEventsResource, 0);
end;

end.

