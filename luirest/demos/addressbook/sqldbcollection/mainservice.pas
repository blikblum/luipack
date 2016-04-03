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
    procedure DataModuleResourceLoad(Resource: TRESTResource; ResourceTag: PtrInt);
    procedure SQLConnectorAfterConnect(Sender: TObject);
  private
    procedure InitConnection;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainServiceModule: TMainServiceModule;

implementation

uses
  LuiRESTSqldb, IniFiles, LuiRESTSwagger, sqlite3, fpjsonrtti;

{$R *.lfm}

type
  TSQLConnectorAccess = class(TSQLConnector)

  end;

  { TFieldDefsResource }

  TFieldDefsResource = class(TRESTResource)
  private
    FSqldbResource: TSqldbResource;
  protected
    procedure Loaded(Tag: PtrInt); override;
  public
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

{ TFieldDefsResource }

procedure TFieldDefsResource.Loaded(Tag: PtrInt);
begin
  inherited Loaded(Tag);
  FSqldbResource := TObject(Tag) as TSqldbResource;
end;

procedure TFieldDefsResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  Query: TSQLQuery;
  Streamer: TJSONStreamer;
  FieldDefsStr: String;
begin
  FieldDefsStr := '';
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FSqldbResource.Connection;
    Query.SQL.Add(FSqldbResource.SelectSQL);
    Query.SQL.Add('where 1 <> 1');
    Query.Open;
    Streamer := TJSONStreamer.Create(nil);
    try
      FieldDefsStr := Streamer.CollectionToJSON(Query.FieldDefs);
    finally
      Streamer.Destroy;
    end;
  finally
    Query.Destroy;
  end;
  if FieldDefsStr <> '' then
  begin
    AResponse.Contents.Add(FieldDefsStr);
    AResponse.Code := 200;
  end
  else
    SetResponseStatus(AResponse, 500, 'Unable to stream fielddefs', []);
end;

{ TMainServiceModule }

procedure TMainServiceModule.SQLConnectorAfterConnect(Sender: TObject);
begin
  if TSQLConnectorAccess(SQLConnector).Proxy is TSQLite3Connection then
    sqlite3_busy_timeout(TSQLite3Connection(TSQLConnectorAccess(SQLConnector).Proxy).Handle, 3000);
end;

procedure TMainServiceModule.DataModuleResourceLoad(Resource: TRESTResource; ResourceTag: PtrInt);
begin
  if Resource is TSqldbResource then
    Resource.RegisterSubPath('fielddefs', TFieldDefsResource, PtrInt(Resource));
end;

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
  TSqldbResource.DefaultConnection := SQLConnector;
  Resources.Register('contacts', TContactsResource, 0);
  Resources.Register('categories', TCategoriesResource, 0);
  Resources.Register('contactdetails', TContactDetailsResource, 0);
  Resources.Register('info', TServiceInfoResource, 0);
  Resources.Register('systemdata', TSystemDataResource, 0);
  Resources.Register('swagger', TSwaggerDefinitionResource, PtrInt(Resources));
end;

end.

