unit AddressBookResources;

{$mode objfpc}{$H+}
{$define USE_SQLITE3_SLIM}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqldb, HTTPDefs, fpjson,
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

  { TContactDetailsResource }

  TContactDetailsResource = class(TSqldbCollectionResource)
  protected
    class function GetItemParam: String; override;
  public
    constructor Create; override;
  end;

  { TSystemDataResource }

  TSystemDataResource = class(TSqldbResource)
  public
    constructor Create; override;
  end;

  { TSystemDataItemResource }

  TSystemDataItemResource = class(TSqldbResource)
  public
    constructor Create; override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

uses
  LuiJSONUtils;

{ TContactDetailsResource }

class function TContactDetailsResource.GetItemParam: String;
begin
  Result := 'contactid';
end;

constructor TContactDetailsResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select * from ContactDetails';
  PrimaryKey := 'ContactId';
  JSONFields := '[{"name": "arraydata", "type": "array"}, {"name": "objectdata", "type": "object"}]';
  IgnoreNotFound := True;
end;

{ TSystemDataResource }

constructor TSystemDataResource.Create;
begin
  inherited Create;
  SetDefaultSubPath('key', TSystemDataItemResource, 0);
end;

{ TSystemDataItemResource }

constructor TSystemDataItemResource.Create;
begin
  inherited Create;
  SelectSQL := 'Select * from SystemData';
  JSONFields := Format('[{"name":"%s","type":"object"}]', ['value']);
  PrimaryKeyParam := 'key';
  PrimaryKey := 'key';
  IgnoreNotFound := True;
end;

type
  TRequestAccess = class(TRequest)
  end;


procedure TSystemDataItemResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
var
  RequestData, FieldData: TJSONObject;
begin
  if TryStrToJSON(ARequest.Content, FieldData) then
  begin
    RequestData := TJSONObject.Create(['value', FieldData]);
    TRequestAccess(ARequest).FContent := RequestData.AsJSON;
    RequestData.Destroy;
  end
  else
    ARequest.Content := Format('{"%s":null}', ['value']);
  inherited HandlePut(ARequest, AResponse);
end;

procedure TSystemDataItemResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  ResponseData, FieldData: TJSONObject;
begin
  inherited HandleGet(ARequest, AResponse);
  if AResponse.Code < 300 then
  begin
    if TryStrToJSON(AResponse.Content, ResponseData) then
    begin
      if FindJSONProp(ResponseData, 'value', FieldData) then
        AResponse.Content := FieldData.AsJSON
      else
        AResponse.Content := '{}';
      ResponseData.Destroy;
    end
    else
      AResponse.Content := '{}';
  end;
end;

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
  IgnoreNotFound := True;
end;

{ TServiceInfoResource }

procedure TServiceInfoResource.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
begin
  AResponse.Contents.Add('{"version":"0.1"}');
end;

end.

