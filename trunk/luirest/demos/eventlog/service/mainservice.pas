unit MainService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, sqlite3slimconn,
  sqldb, LuiRESTSqldb;

type

  { TLogServiceModule }

  TLogServiceModule = class(TRESTServiceModule)
    Connection: TSQLite3Connection;
    Transaction: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  LogServiceModule: TLogServiceModule;

implementation

uses
  EventResources;

{$R *.lfm}

{ TLogServiceModule }

constructor TLogServiceModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TSqldbJSONResource.DefaultConnection := Connection;
  Resources.Register('events', TEventsResource, 0);
end;

end.

