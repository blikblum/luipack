unit AddressBookResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, HTTPDefs, fphttp, gh_SQL;

const
  RES_CONTACTS = 1;
  RES_CONTACT = 2;
  RES_CONTACTPHONES = 3;
  RES_CONTACTPHONE = 4;

type

  { TAddressBookResourceFactory }

  TAddressBookResourceFactory = class(TComponent)
  private
    FConnector: TghSQLConnector;
    FDatabase: String;
    procedure SetDatabase(AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateResource(out Resource: TRESTResource; ResourceTag: PtrInt);
    property Connector: TghSQLConnector read FConnector;
    property Database: String read FDatabase write SetDatabase;
  end;


implementation

uses
  LuiJSONUtils, fpjson, gh_SQLdbLib, gh_Data;

type

  { TGHTableResource }

  TGHTableResource = class(TRESTResource)
  private
    FResultColumns: String;
    FSelectColumns: String;
    FConditions: String;
    FConnector: TghSQLConnector;
    FIsCollection: Boolean;
    FOrderBy: String;
    FPrimaryKey: String;
    FPrimaryKeyParam: String;
    FTableName: String;
    FUpdateColumns: TStringList;
    procedure SetUpdateColumns(const AValue: String);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
    property Conditions: String read FConditions write FConditions;
    property Connector: TghSQLConnector read FConnector write FConnector;
    property IsCollection: Boolean read FIsCollection write FIsCollection;
    property OrderBy: String read FOrderBy write FOrderBy;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property PrimaryKeyParam: String read FPrimaryKeyParam write FPrimaryKeyParam;
    property ResultColumns: String read FResultColumns write FResultColumns;
    property SelectColumns: String read FSelectColumns write FSelectColumns;
    property TableName: String read FTableName write FTableName;
    property UpdateColumns: String write SetUpdateColumns;
  end;

procedure JSONDataToParams(JSONObj: TJSONObject; Params: TghDataParams);
var
  i: Integer;
begin
  Params.Clear;
  for i := 0 to JSONObj.Count -1 do
    Params[JSONObj.Names[i]].Value := JSONObj.Items[i].Value;
end;

procedure SetTableData(Table: TghSQLTable; Obj1, Obj2: TJSONObject; Columns: TStrings);
var
  i: Integer;
  FieldName: String;
  PropData: TJSONData;
begin
  for i := 0 to Columns.Count -1 do
  begin
    FieldName := Columns[i];
    PropData := Obj1.Find(FieldName);
    if PropData = nil then
      PropData := Obj2.Find(FieldName);
    if PropData <> nil then
      Table.GetColumns.FieldByName(FieldName).Value := PropData.Value;
  end;
end;

{ TAddressBookResourceFactory }

procedure TAddressBookResourceFactory.SetDatabase(AValue: String);
var
  SQL: TghSQLClient;
begin
  FDatabase := AValue;
  FConnector.Database := AValue;
  SQL := TghSQLClient.Create(FConnector);
  SQL.Script.Add('PRAGMA foreign_keys = ON');
  SQL.Execute;
  SQL.Destroy;
end;

constructor TAddressBookResourceFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnector := TghSQLConnector.Create(TghSQLite3Lib);
end;

destructor TAddressBookResourceFactory.Destroy;
begin
  FConnector.Destroy;
  inherited Destroy;
end;

procedure TAddressBookResourceFactory.CreateResource(out Resource: TRESTResource; ResourceTag: PtrInt);
var
  GHResource: TGHTableResource absolute Resource;
begin
  GHResource := TGHTableResource.Create;
  GHResource.Connector := FConnector;
  case ResourceTag of
    RES_CONTACTS:
    begin
      GHResource.IsCollection := True;
      GHResource.TableName := 'Contacts';
      GHResource.SelectColumns := 'Id, Name';
      GHResource.PrimaryKey := 'Id';
      GHResource.UpdateColumns := 'name';
      GHResource.SetDefaultSubPath('contactid', @CreateResource, RES_CONTACT);
    end;
    RES_CONTACT:
    begin
      GHResource.TableName := 'Contacts';
      GHResource.SelectColumns := 'Id, Name';
      GHResource.Conditions := 'Id = :contactid';
      GHResource.PrimaryKey := 'Id';
      GHResource.PrimaryKeyParam := 'contactid';
      GHResource.UpdateColumns := 'name';
      GHResource.RegisterSubPath('phones', @CreateResource, RES_CONTACTPHONES);
    end;
    RES_CONTACTPHONES:
    begin
      GHResource.IsCollection := True;
      GHResource.TableName := 'Phones';
      GHResource.SelectColumns := 'Id, ContactId, Number';
      GHResource.Conditions := 'ContactId = :contactid';
      GHResource.PrimaryKey := 'Id';
      GHResource.ResultColumns := '["id", "number"]';
      GHResource.UpdateColumns := 'number;contactid';
      GHResource.SetDefaultSubPath('phoneid', @CreateResource, RES_CONTACTPHONE);
    end;
    RES_CONTACTPHONE:
    begin
      GHResource.TableName := 'Phones';
      GHResource.SelectColumns := 'Id, Number';
      GHResource.Conditions := 'Id = :phoneid';
      GHResource.PrimaryKey := 'Id';
      GHResource.PrimaryKeyParam := 'phoneid';
      GHResource.UpdateColumns := 'number';
    end;
  end;
end;

procedure TGHTableResource.SetUpdateColumns(const AValue: String);
begin
  FUpdateColumns.DelimitedText := AValue;
end;

destructor TGHTableResource.Destroy;
begin
  FUpdateColumns.Destroy;
  inherited Destroy;
end;

procedure TGHTableResource.AfterConstruction;
begin
  inherited AfterConstruction;
  FUpdateColumns := TStringList.Create;
  FUpdateColumns.Delimiter := ';';
end;

procedure TGHTableResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  Table: TghSQLTable;
  ResponseData: TJSONData;
begin
  Table := FConnector.Tables[FTableName];
  Table.Select(FSelectColumns).Where(FConditions).OrderBy(FOrderBy);
  JSONDataToParams(URIParams, Table.Params);
  Table.Open;
  if FIsCollection then
  begin
    ResponseData := DatasetToJSON(Table.Dataset, [djoSetNull], '');
    try
      AResponse.Contents.Add(ResponseData.AsJSON);
    finally
      ResponseData.Free;
    end;
  end
  else
  begin
    if (Table.RecordCount > 0) then
    begin
      ResponseData := DatasetToJSON(Table.Dataset, [djoCurrentRecord, djoSetNull], '');
      try
        AResponse.Contents.Add(ResponseData.AsJSON);
      finally
        ResponseData.Free;
      end;
    end
    else
    begin
      SetResponseStatus(AResponse, 404, 'Resource "%s" not found', [ARequest.PathInfo]);
    end;
  end;
  Table.Close;
end;

procedure TGHTableResource.HandleDelete(ARequest: TRequest; AResponse: TResponse);
var
  Client: TghSQLClient;
begin
  if not FIsCollection then
  begin
    Client := TghSQLClient.Create(FConnector);
    Client.Script.Add(Format('Delete from %s where %s = %s', [FTableName, FPrimaryKey, URIParams.Strings[FPrimaryKeyParam]]));
    Client.Execute;
    Client.Destroy;
  end
  else
    inherited HandleDelete(ARequest, AResponse);
end;

procedure TGHTableResource.HandlePost(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  ResponseData: TJSONData;
  Table: TghSQLTable;
begin
  if FIsCollection then
  begin
    Table := FConnector.Tables[FTableName];
    //don't get all data
    Table.Select(FSelectColumns).Where('1 = 2');
    RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
    try
      Table.Open;
      Table.Append;
      SetTableData(Table, RequestData, URIParams, FUpdateColumns);
      Table.Commit;
      if Table.HasErrors then
      begin
        SetResponseStatus(AResponse, 400, 'Error posting to %s', [FTableName]);
      end
      else
      begin
        Table.Close;
        Table.Select(FSelectColumns).Where(Format('%s = %d', [FPrimaryKey, FConnector.Lib.GetLastAutoIncValue]));
        Table.Open;
        ResponseData := DatasetToJSON(Table.Dataset, [djoCurrentRecord, djoSetNull], FResultColumns);
        try
          AResponse.Contents.Add(ResponseData.AsJSON);
        finally
          ResponseData.Free;
        end;
      end;
      Table.Close;
    finally
      RequestData.Free;
    end;
  end
  else
    inherited HandlePost(ARequest, AResponse);
end;

procedure TGHTableResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  ResponseData: TJSONData;
  Table: TghSQLTable;
begin
  if not FIsCollection then
  begin
    Table := FConnector.Tables[FTableName];
    Table.Select(FSelectColumns).Where(FConditions);
    JSONDataToParams(URIParams, Table.Params);
    Table.Open;
    RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
    try
      Table.Edit;
      SetTableData(Table, RequestData, URIParams, FUpdateColumns);
      Table.Post;
      Table.Commit;
      if Table.HasErrors then
      begin
        SetResponseStatus(AResponse, 500, 'Error updating %s', [ARequest.PathInfo]);
      end
      else
      begin
        ResponseData := DatasetToJSON(Table.Dataset, [djoCurrentRecord, djoSetNull], FResultColumns);
        try
          AResponse.Contents.Add(ResponseData.AsJSON);
        finally
          ResponseData.Free;
        end;
      end;
    finally
      RequestData.Free;
    end;
  end
  else
    inherited HandlePut(ARequest, AResponse);
end;

end.

