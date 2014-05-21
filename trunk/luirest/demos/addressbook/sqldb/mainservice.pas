unit MainService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, sqlite3slimconn,
  sqldb, AddressBookResources;

type

  { TMainServiceModule }

  TMainServiceModule = class(TRESTServiceModule)
    SQLConnector: TSQLConnector;
    SQLTransaction: TSQLTransaction;
  private
    FResourceFactory: TAddressBookResourceFactory;
    procedure InitConnection;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainServiceModule: TMainServiceModule;

implementation

uses
  LuiRESTSqldb, IniFiles;

{$R *.lfm}

{ TMainServiceModule }

procedure TMainServiceModule.InitConnection;
var
  Config: TIniFile;
  DatabaseType: String;
begin
  Config := TIniFile.Create('addressbookdb.ini');
  DatabaseType := Config.ReadString('database', 'type', 'sqlite3slim');
  SQLConnector.ConnectorType := DatabaseType;
  SQLConnector.DatabaseName := Config.ReadString(DatabaseType, 'databasename', 'addressbookdata.db');
  SQLConnector.HostName := Config.ReadString(DatabaseType, 'hostname', 'localhost');
  SQLConnector.Password := Config.ReadString(DatabaseType, 'password', '');
  SQLConnector.UserName := Config.ReadString(DatabaseType, 'username', '');
  Config.Destroy;
end;

constructor TMainServiceModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitConnection;
  TSqldbJSONResource.DefaultConnection := SQLConnector;
  FResourceFactory := TAddressBookResourceFactory.Create(Self);
  Resources.Register('contacts', @FResourceFactory.CreateResource, RES_CONTACTS);
  Resources.Register('categories', @FResourceFactory.CreateResource, RES_CATEGORIES);
  Resources.Register('info', TServiceInfoResource, 0);
end;

end.

