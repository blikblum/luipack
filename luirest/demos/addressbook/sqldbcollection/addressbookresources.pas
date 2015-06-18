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

type

  { TServiceInfoResource }

  TServiceInfoResource = class(TRESTResource)
  public
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TCategoriesResource }

  TCategoriesResource = class(TSqldbCollectionResource)
  protected
    class function GetItemParam: String; override;
  public
    constructor Create; override;
  end;

  { TContactsResource }

  TContactsResource = class(TSqldbCollectionResource)
  protected
    class function GetItemParam: String; override;
    procedure PrepareItem(ItemResource: TSqldbResource); override;
  public
    constructor Create; override;
  end;

  { TContactPhonesResource }

  TContactPhonesResource = class(TSqldbCollectionResource)
  protected
    class function GetItemParam: String; override;
    procedure PrepareItem(ItemResource: TSqldbResource); override;
  public
    constructor Create; override;
  end;

implementation

{ TContactPhonesResource }

class function TContactPhonesResource.GetItemParam: String;
begin
  Result := 'phoneid';
end;

procedure TContactPhonesResource.PrepareItem(ItemResource: TSqldbResource);
begin
  inherited PrepareItem(ItemResource);
  ItemResource.InputFields := '["number"]';
end;

constructor TContactPhonesResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select Id, ContactId, Number from Phones';
  ConditionsSQL := 'where ContactId = :contactid';
  OutputFields := '["id", "number"]';
  InputFields := '["number", "contactid"]';
end;

{ TCategoriesResource }

class function TCategoriesResource.GetItemParam: String;
begin
  Result := 'categoryid';
end;

constructor TCategoriesResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select Id, Name from Categories';
end;

{ TContactsResource }

class function TContactsResource.GetItemParam: String;
begin
  Result := 'contactid';
end;

procedure TContactsResource.PrepareItem(ItemResource: TSqldbResource);
begin
  inherited PrepareItem(ItemResource);
  ItemResource.RegisterSubPath('phones', TContactPhonesResource, 0);
end;

constructor TContactsResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select Id, CategoryId, Name from contacts';
end;

{ TServiceInfoResource }

procedure TServiceInfoResource.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
begin
  AResponse.Contents.Add('{"version":"0.1"}');
end;

end.

