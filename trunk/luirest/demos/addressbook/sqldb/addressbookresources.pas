unit AddressBookResources;

{$mode objfpc}{$H+}
{$define USE_SQLITE3_SLIM}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqldb,
  {$ifdef USE_SQLITE3_SLIM}
  sqlite3slimconn
  {$else}
  sqlite3conn
  {$endif};

const
  RES_CONTACTS = 1;
  RES_CONTACT = 2;
  RES_CONTACTPHONES = 3;
  RES_CONTACTPHONE = 4;


type

  { TAddressBookResourceFactory }

  TAddressBookResourceFactory = class(TComponent)
  private
    FConnection: TSQLite3Connection;
    FDatabase: String;
    FTransaction: TSQLTransaction;
    procedure SetDatabase(const AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateResource(out Resource: TRESTResource; ResourceTag: PtrInt);
    property Connection: TSQLite3Connection read FConnection;
    property Database: String read FDatabase write SetDatabase;
  end;

implementation

{ TAddressBookResourceFactory }

procedure TAddressBookResourceFactory.SetDatabase(const AValue: String);
begin
  FDatabase := AValue;
  FConnection.DatabaseName := AValue;
end;

constructor TAddressBookResourceFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := TSQLite3Connection.Create(Self);
  FTransaction := TSQLTransaction.Create(Self);
  FTransaction.DataBase := FConnection;
end;

procedure TAddressBookResourceFactory.CreateResource(out Resource: TRESTResource;
  ResourceTag: PtrInt);
var
  SqldbResource: TSqldbJSONResource absolute Resource;
begin
  SqldbResource := TSqldbJSONResource.Create;
  SqldbResource.Connection := FConnection;
  case ResourceTag of
    RES_CONTACTS:
    begin
      SqldbResource.IsCollection := True;
      SqldbResource.SelectSQL := 'Select Id, Name from contacts';
      SqldbResource.PrimaryKey := 'Id';
      SqldbResource.SetDefaultSubPath('contactid', @CreateResource, RES_CONTACT);
    end;
    RES_CONTACT:
    begin
      SqldbResource.SelectSQL := 'Select Id, Name from Contacts';
      SqldbResource.ConditionsSQL := 'where Id = :contactid';
      SqldbResource.PrimaryKey := 'Id';
      SqldbResource.PrimaryKeyParam := 'contactid';
      SqldbResource.RegisterSubPath('phones', @CreateResource, RES_CONTACTPHONES);
    end;
    RES_CONTACTPHONES:
    begin
      SqldbResource.IsCollection := True;
      SqldbResource.SelectSQL := 'Select Id, ContactId, Number from Phones';
      SqldbResource.ConditionsSQL := 'where ContactId = :contactid';
      SqldbResource.PrimaryKey := 'Id';
      SqldbResource.OutputFields := '["id", "number"]';
      SqldbResource.InputFields := '["number", "contactid"]';
      SqldbResource.SetDefaultSubPath('phoneid', @CreateResource, RES_CONTACTPHONE);
    end;
    RES_CONTACTPHONE:
    begin
      SqldbResource.SelectSQL := 'Select Id, Number From Phones';
      SqldbResource.ConditionsSQL := 'Where Id = :phoneid';
      SqldbResource.PrimaryKey := 'Id';
      SqldbResource.PrimaryKeyParam := 'phoneid';
      SqldbResource.InputFields := '["number"]';
    end;
  end;
end;

end.

