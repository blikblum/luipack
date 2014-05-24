unit AddressBookResources;

{$mode objfpc}{$H+}
{$define USE_SQLITE3_SLIM}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqldb, HTTPDefs,
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
  RES_CATEGORIES = 10;
  RES_CATEGORY = 11;


type

  { TServiceInfoResource }

  TServiceInfoResource = class(TRESTResource)
  public
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TAddressBookResourceFactory }

  TAddressBookResourceFactory = class(TComponent)
  public
    procedure CreateResource(out Resource: TRESTResource; ResourceTag: PtrInt);
  end;

implementation

{ TServiceInfoResource }

procedure TServiceInfoResource.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
begin
  AResponse.Contents.Add('{"version":"0.1"}');
end;

{ TAddressBookResourceFactory }

procedure TAddressBookResourceFactory.CreateResource(out Resource: TRESTResource;
  ResourceTag: PtrInt);
var
  SqldbResource: TSqldbJSONResource absolute Resource;
begin
  SqldbResource := TSqldbJSONResource.Create;
  case ResourceTag of
    RES_CONTACTS:
    begin
      SqldbResource.IsCollection := True;
      SqldbResource.SelectSQL := 'Select Id, CategoryId, Name from contacts';
      SqldbResource.SetDefaultSubPath('contactid', @CreateResource, RES_CONTACT);
    end;
    RES_CONTACT:
    begin
      SqldbResource.SelectSQL := 'Select Id, CategoryId, Name from Contacts';
      SqldbResource.PrimaryKeyParam := 'contactid';
      SqldbResource.RegisterSubPath('phones', @CreateResource, RES_CONTACTPHONES);
    end;
    RES_CONTACTPHONES:
    begin
      SqldbResource.IsCollection := True;
      SqldbResource.SelectSQL := 'Select Id, ContactId, Number from Phones';
      SqldbResource.ConditionsSQL := 'where ContactId = :contactid';
      SqldbResource.OutputFields := '["id", "number"]';
      SqldbResource.InputFields := '["number", "contactid"]';
      SqldbResource.SetDefaultSubPath('phoneid', @CreateResource, RES_CONTACTPHONE);
    end;
    RES_CONTACTPHONE:
    begin
      SqldbResource.SelectSQL := 'Select Id, Number From Phones';
      SqldbResource.PrimaryKeyParam := 'phoneid';
      SqldbResource.InputFields := '["number"]';
    end;
    RES_CATEGORIES:
    begin
      SqldbResource.IsCollection := True;
      SqldbResource.SelectSQL := 'Select Id, Name from Categories';
      SqldbResource.SetDefaultSubPath('categoryid', @CreateResource, RES_CATEGORY);
    end;
    RES_CATEGORY:
    begin
      SqldbResource.SelectSQL := 'Select Id, Name from Categories';
      SqldbResource.PrimaryKeyParam := 'categoryid';
    end;
  end;
end;

end.

