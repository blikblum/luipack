unit LuiRESTSqldb;

{$mode objfpc}{$H+}
{$define USE_SQLITE3_SLIM}

interface

uses
  Classes, SysUtils, sqldb, db, LuiRESTServer, HTTPDefs, fphttp, fpjson,
  pqconnection, IBConnection, mysql51conn,
  {$ifdef USE_SQLITE3_SLIM}
  sqlite3slimconn
  {$else}
  sqlite3conn
  {$endif}
  ;

type

  THTTPMethodType = (hmtGet, hmtPost, hmtPut, hmtPatch, hmtDelete);

  { TSqldbResource }

  TSqldbResource = class(TRESTResource)
  private
    FConditionsSQL: String;
    FConnection: TSQLConnection;
    FInputFields: String;
    FJSONFields: String;
    FOutputFields: String;
    FPrimaryKey: String;
    FPrimaryKeyParam: String;
    FQueryParams: String;
    FSelectSQL: String;
    FInputFieldsData: TJSONData;
    FJSONFieldsData: TJSONArray;
    FQueryParamsData: TJSONArray;
    FIgnoreNotFound: Boolean;
    FIsCollection: Boolean;
    FPreserveCase: Boolean;
    FPutAsPatch: Boolean;
    FReadOnly: Boolean;
    class var FDefaultConnection: TSQLConnection;
    class procedure SetDefaultConnection(Value: TSQLConnection); static;
  protected
    procedure DecodeJSONFields(ResponseData: TJSONArray);
    procedure DecodeJSONFields(ResponseData: TJSONObject);
    procedure EncodeJSONFields(RequestData: TJSONObject);
    function GetQuery(AOwner: TComponent): TSQLQuery;
    function GetResourceIdentifierSQL: String;
    class function GetSelectSQL: String; virtual;
    function InsertRecord(Query: TSQLQuery): String; virtual;
    procedure Loaded(Tag: PtrInt); override;
    procedure PrepareParams(Params: TParams; ARequest: TRequest);
    procedure PrepareQuery(Query: TSQLQuery; MethodType: THTTPMethodType;
      const ASelectSQL, AConditionsSQL: String); virtual;
    procedure PrepareResponse(ResponseData: TJSONData); virtual;
    procedure SetPrimaryKeyData(Query: TSQLQuery; Params: TJSONObject);
    procedure SetQueryData(Query: TSQLQuery; RequestData, Params: TJSONObject; DoPatch: Boolean = False); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePatch(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
    property ConditionsSQL: String read FConditionsSQL write FConditionsSQL;
    property Connection: TSQLConnection read FConnection write FConnection;
    class property DefaultConnection: TSQLConnection read FDefaultConnection write SetDefaultConnection;
    property IgnoreNotFound: Boolean read FIgnoreNotFound write FIgnoreNotFound;
    property InputFields: String read FInputFields write FInputFields;
    property IsCollection: Boolean read FIsCollection write FIsCollection;
    property JSONFields: String read FJSONFields write FJSONFields;
    property OutputFields: String read FOutputFields write FOutputFields;
    property PreserveCase: Boolean read FPreserveCase write FPreserveCase;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property PrimaryKeyParam: String read FPrimaryKeyParam write FPrimaryKeyParam;
    //todo: remove when Patch support is added
    property PutAsPatch: Boolean read FPutAsPatch write FPutAsPatch;
    property QueryParams: String read FQueryParams write FQueryParams;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property SelectSQL: String read FSelectSQL write FSelectSQL;
  end;

  TSqldbResourceClass = class of TSqldbResource;

  { TSqldbCollectionResource }

  TSqldbCollectionResource = class(TSqldbResource)
  private
    procedure CreateItemResource(out Resource: TRESTResource; ResourceTag: PtrInt);
  protected
    class function IsItemIdValid(const ItemId: String): Boolean; virtual;
    class function GetItemClass: TSqldbResourceClass; virtual;
    class function GetItemParam: String; virtual;
    class function GetItemParamType: String; virtual;
    procedure Loaded(Tag: PtrInt); override;
    procedure PrepareItem(ItemResource: TSqldbResource); virtual;
  public
    constructor Create; override;
    procedure HandleSubPath(const SubPath: String;
      var SubPathResourceDef: TRESTResourceDef); override;
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

  //for compatibility with old code
  TSqldbJSONResource = TSqldbResource;

  TSqldbJSONResourceClass = TSqldbResourceClass;

procedure JSONDataToParams(JSONObj: TJSONObject; Params: TParams);
procedure QueryFieldsToParams(QueryFields: TStrings; QueryParamsData: TJSONArray; Params: TParams);

implementation

uses
  LuiJSONUtils, fpjsonrtti, dbconst;

type
  TSQLConnectionAccess = class(TSQLConnection)

  end;

  TSQLConnectorAccess = class(TSQLConnector)

  end;

const
  ReadOnlyAccessError = '%s not allowed in read only resources';
  InvalidCollectionMethodError = '%s not allowed for collection resources';

procedure JSONDataToParams(JSONObj: TJSONObject; Params: TParams);
var
  i: Integer;
  Param: TParam;
  PropName: String;
begin
  for i := 0 to JSONObj.Count -1 do
  begin
    PropName := JSONObj.Names[i];
    Param := Params.FindParam(PropName);
    if Param = nil then
      Param := Params.CreateParam(ftString, PropName, ptInput);
    Param.Value := JSONObj.Items[i].Value;
  end;
end;

procedure QueryFieldsToParams(QueryFields: TStrings; QueryParamsData: TJSONArray; Params: TParams);
var
  i, FieldIndex: Integer;
  QueryParamData: TJSONData;
  ParamName, ParamType, ParamFormat, ParamValue: String;
  ParamRequired: Boolean;
  Int64Value: int64;
  DoubleValue: Extended;
  Param: TParam;
begin
  for i := 0 to QueryParamsData.Count - 1 do
  begin
    QueryParamData := QueryParamsData.Items[i];
    ParamType := '';
    ParamFormat := '';
    ParamRequired := False;
    case QueryParamData.JSONType of
      jtString:
        begin
          ParamName := QueryParamData.AsString;
        end;
      jtObject:
        begin
          ParamName := TJSONObject(QueryParamData).Get('name', '');
          ParamRequired := TJSONObject(QueryParamData).Get('required', False);
          ParamType := TJSONObject(QueryParamData).Get('type', '');
          ParamFormat := TJSONObject(QueryParamData).Get('format', '');
        end;
    end;
    if ParamName <> '' then
    begin
      Param := Params.FindParam(ParamName);
      if Param = nil then
        Param := Params.CreateParam(ftString, ParamName, ptInput);

      FieldIndex := QueryFields.IndexOfName(ParamName);
      if FieldIndex <> -1 then
      begin
        ParamValue := QueryFields.ValueFromIndex[FieldIndex];
        if ParamType = 'string' then
          Param.AsString := ParamValue
        else
        begin
          if TryStrToInt64(ParamValue, Int64Value) then
          begin
            case ParamFormat of
              'date': Param.AsDate := Int64Value;
              'time': Param.AsTime := Int64Value;
              'datetime': Param.AsDateTime := Int64Value;
            else
              Param.AsLargeInt := Int64Value
            end;
          end
          else if TryStrToFloat(ParamValue, DoubleValue) then
          begin
            case ParamFormat of
              'date': Param.AsDate := Trunc(DoubleValue);
              'time': Param.AsTime := Frac(DoubleValue);
              'datetime': Param.AsDateTime := DoubleValue;
            else
              Param.AsFloat := DoubleValue
            end;
          end
          else
            Param.AsString := ParamValue;
        end;
      end
      else
      begin
        if ParamRequired then
          raise Exception.CreateFmt('Param "%s" is required', [ParamName]);
      end;
    end;
  end;
end;

function GetFieldName(FieldsData: TJSONArray; FieldIndex: Integer; out DBFieldName: String): String;
var
  FieldData: TJSONData;
begin
  FieldData := FieldsData.Items[FieldIndex];
  if FieldData.JSONType = jtString then
  begin
    Result := FieldData.AsString;
    DBFieldName := Result;
  end
  else if FieldData.JSONType = jtObject then
  begin
    Result := TJSONObject(FieldData).Get('name', '');
    DBFieldName := TJSONObject(FieldData).Get('mapping', Result);
  end
  else
    Result := '';
end;


procedure TSqldbResource.SetQueryData(Query: TSQLQuery; RequestData, Params: TJSONObject;
  DoPatch: Boolean);
var
  i: Integer;
  FieldName, DBFieldName: String;
  PropData, FieldNameData: TJSONData;
  Field: TField;
  ExcludeFieldsData: TJSONArray;
begin
  EncodeJSONFields(RequestData);
  if (FInputFieldsData <> nil) and (FInputFieldsData.JSONType = jtArray) then
  begin
    for i := 0 to Query.Fields.Count - 1 do
      Query.Fields[i].ReadOnly := True;
    for i := 0 to FInputFieldsData.Count - 1 do
    begin
      FieldName := GetFieldName(TJSONArray(FInputFieldsData), i, DBFieldName);
      Field := Query.FieldByName(DBFieldName);
      Field.ReadOnly := False;
      PropData := Params.Find(FieldName);
      if PropData = nil then
        PropData := RequestData.Find(FieldName);
      if (PropData <> nil) and not (PropData.JSONType in [jtObject, jtArray]) then
        Field.Value := PropData.Value
      else
      begin
        if not DoPatch then
          Field.Value := Null;
      end;
    end;
  end
  else
  begin
    // no specific columns set
    for i := 0 to Query.Fields.Count -1 do
    begin
      Field := Query.Fields[i];
      FieldName := LowerCase(Field.FieldName);
      if SameText(FieldName, FPrimaryKey) then
        continue;
      PropData := Params.Find(FieldName);
      if PropData = nil then
        PropData := RequestData.Find(FieldName);
      if (PropData <> nil) and not (PropData.JSONType in [jtObject, jtArray]) then
        Field.Value := PropData.Value
      else
      begin
        if not DoPatch then
          Field.Value := Null;
      end;
    end;
    //check for exclusions
    if (FInputFieldsData <> nil) and (FInputFieldsData.JSONType = jtObject) and
      FindJSONProp(TJSONObject(FInputFieldsData), 'exclude', ExcludeFieldsData) then
    begin
      for i := 0 to ExcludeFieldsData.Count - 1 do
      begin
        FieldNameData := ExcludeFieldsData.Items[i];
        if FieldNameData.JSONType = jtString then
        begin
          Field := Query.FindField(FieldNameData.AsString);
          if Field <> nil then
            Field.ProviderFlags := Field.ProviderFlags - [pfInUpdate];
        end;
      end;
    end;
  end;
end;

constructor TSqldbResource.Create;
begin
  inherited Create;
  FPrimaryKey := 'Id';
  FConnection := FDefaultConnection;
end;

{ TSqldbResource }

class procedure TSqldbResource.SetDefaultConnection(Value: TSQLConnection);
begin
  if (FDefaultConnection <> nil) and (Value <> nil) then
    raise Exception.Create('Default connection already set');
  FDefaultConnection := Value;
end;

procedure DoDecodeJSONFields(RecordData: TJSONObject; JSONFieldsData: TJSONArray);
var
  i: Integer;
  FieldDefData, PropData, DecodedData: TJSONData;
  DecodedArrayData: TJSONArray absolute DecodedData;
  DecodedObjectData: TJSONObject absolute DecodedData;
  PropName, PropTypeName: String;
  PropType: TJSONtype;
begin
  //todo: move this to DatasetToJSON / OutputFields?? -> Create a TJSONToDatasetClass
  for i := 0 to JSONFieldsData.Count - 1 do
  begin
    //todo parse fielddef once (move out of here)
    FieldDefData := JSONFieldsData.Items[i];
    PropType := jtUnknown;
    case FieldDefData.JSONType of
      jtObject:
        begin
          PropName := TJSONObject(FieldDefData).Get('name', '');
          PropTypeName := TJSONObject(FieldDefData).Get('type', '');
          if PropTypeName = 'array' then
            PropType := jtArray
          else if PropTypeName = 'object' then
            PropType := jtObject;
        end;
      jtString:
        begin
          PropName := FieldDefData.AsString;
        end;
    end;
    PropData := RecordData.Find(PropName);
    if PropData = nil then
      raise Exception.CreateFmt('Error parsing JSON field: field "%s" not found', [PropName]);
    if PropData.JSONType = jtString then
    begin
      case PropType of
        jtArray:
          if not TryStrToJSON(PropData.AsString, DecodedArrayData) then
            DecodedArrayData := TJSONArray.Create;
        jtObject:
          if not TryStrToJSON(PropData.AsString, DecodedObjectData) then
            DecodedObjectData := TJSONObject.Create;
        else
          if not TryStrToJSON(PropData.AsString, DecodedData) then
            DecodedData := TJSONNull.Create;
      end;
    end
    else
    begin
      //initialize field with empty data
      case PropType of
        jtArray:
          DecodedData := TJSONArray.Create;
        jtObject:
          DecodedData := TJSONObject.Create;
        else
          DecodedData := TJSONNull.Create;
      end;
    end;
    RecordData.Elements[PropName] := DecodedData;
  end;
end;

procedure TSqldbResource.DecodeJSONFields(ResponseData: TJSONArray);
var
  i: Integer;
begin
  if FJSONFieldsData = nil then
    Exit;
  for i := 0 to ResponseData.Count - 1 do
    DoDecodeJSONFields(ResponseData.Objects[i], FJSONFieldsData);
end;

procedure TSqldbResource.DecodeJSONFields(ResponseData: TJSONObject);
begin
  if FJSONFieldsData = nil then
    Exit;
  DoDecodeJSONFields(ResponseData, FJSONFieldsData);
end;

procedure TSqldbResource.EncodeJSONFields(RequestData: TJSONObject);
var
  FieldDefData: TJSONData;
  PropType: TJSONtype;
  PropName: String;
  PropData: TJSONData;
  PropTypeName: TJSONStringType;
  i: Integer;
begin
  if FJSONFieldsData = nil then
    Exit;
  for i := 0 to FJSONFieldsData.Count - 1 do
  begin
    //todo parse fielddef once (move out of here)
    FieldDefData := FJSONFieldsData.Items[i];
    PropType := jtUnknown;
    case FieldDefData.JSONType of
      jtObject:
        begin
          PropName := TJSONObject(FieldDefData).Get('name', '');
          PropTypeName := TJSONObject(FieldDefData).Get('type', '');
          if PropTypeName = 'array' then
            PropType := jtArray
          else if PropTypeName = 'object' then
            PropType := jtObject;
        end;
      jtString:
        begin
          PropName := FieldDefData.AsString;
        end;
    end;
    PropData := RequestData.Find(PropName);
    if PropData <> nil then
    begin
      //nullify if type does not match
      if (PropType <> jtUnknown) and (PropData.JSONType <> PropType) then
        RequestData.Nulls[PropName] := True
      else
        RequestData.Strings[PropName] := PropData.AsJSON;
    end;
  end;
end;

function TSqldbResource.GetQuery(AOwner: TComponent): TSQLQuery;
begin
  Result := TSQLQuery.Create(AOwner);
  Result.DataBase := FConnection;
end;

function TSqldbResource.GetResourceIdentifierSQL: String;
begin
  if FConditionsSQL <> '' then
    Result := FConditionsSQL
  else if (FPrimaryKeyParam <> '') and (FPrimaryKey <> '') then
    Result := Format('where %s = :%s', [FPrimaryKey, FPrimaryKeyParam])
  else
  begin
    Result := '';
    if not ReadOnly then
      raise Exception.Create('Unable to resolve resource identifier SQL query');
  end;
end;

//adapted from sqldb
function CreateInsertQuery(Query: TSQLQuery; const TableName: String; FieldNamesQuoteChars : TQuoteChars): TSQLQuery;
var
  x          : integer;
  sql_fields : string;
  sql_values : string;
  Field: TField;
  Param: TParam;
begin
  sql_fields := '';
  sql_values := '';
  for x := 0 to Query.Fields.Count -1 do
  begin
    Field := Query.Fields[x];
    if (not Field.IsNull) and (pfInUpdate in Field.ProviderFlags) and (not Field.ReadOnly) then
    begin
      sql_fields := sql_fields + FieldNamesQuoteChars[0] + Field.FieldName + FieldNamesQuoteChars[1] + ',';
      sql_values := sql_values + ':"' + Field.FieldName + '",';
    end;
  end;
  if length(sql_fields) = 0 then
    DatabaseErrorFmt(sNoUpdateFields,['insert'],Query);
  Result := TSQLQuery.Create(nil);
  Result.DataBase := Query.DataBase;
  Result.Transaction := Query.Transaction;
  Result.ParseSQL := False;
  setlength(sql_fields,length(sql_fields)-1);
  setlength(sql_values,length(sql_values)-1);
  Result.SQL.Add('insert into ' + TableName + ' (' + sql_fields + ') values (' + sql_values + ')');
  for x := 0 to Result.Params.Count - 1 do
  begin
    Param := Result.Params[x];
    Field := Query.FieldByName(Param.Name);
    Param.AssignFieldValue(Field, Field.Value);
  end;
end;

function TSqldbResource.InsertRecord(Query: TSQLQuery): String;
var
  Info: TSQLStatementInfo;
  ActualConnection: TSQLConnection;
  InsertQuery: TSQLQuery;
begin
  Result := '';
  if (FConnection is TSQLConnector) then
    ActualConnection := TSQLConnectorAccess(FConnection).Proxy
  else
    ActualConnection := FConnection;
  {$IF FPC_FULLVERSION >= 30000}
  Info := TSQLConnectionAccess(ActualConnection).GetStatementInfo(Query.SQL.Text);
  {$ELSE}
  Info := TSQLConnectionAccess(ActualConnection).GetStatementInfo(Query.SQL.Text, True, stNoSchema);
  {$ENDIF}
  InsertQuery := CreateInsertQuery(Query, Info.TableName, ActualConnection.FieldNameQuoteChars);
  try
    if (ActualConnection is TPQConnection) or (ActualConnection is TIBConnection) then
    begin
      InsertQuery.SQL.Add(Format('Returning %s', [FPrimaryKey]));
      InsertQuery.Open;
      if InsertQuery.RecordCount > 0 then
        Result := InsertQuery.Fields[0].AsString;
      ActualConnection.Transaction.CommitRetaining;
    end
    else
    begin
      InsertQuery.ExecSQL;
      ActualConnection.Transaction.CommitRetaining;
      if (ActualConnection is TSQLite3Connection) then
        Result := IntToStr(TSQLite3Connection(ActualConnection).GetInsertID)
      else if (ActualConnection is TConnectionName{MySql}) then
        Result := IntToStr(TConnectionName(ActualConnection).GetInsertID);
    end;
  finally
    InsertQuery.Destroy;
  end;
end;

procedure TSqldbResource.Loaded(Tag: PtrInt);
begin
  inherited Loaded(Tag);
  if FSelectSQL = '' then
    FSelectSQL := GetSelectSQL;
  if FInputFields <> '' then
  begin
    if not TryStrToJSON(FInputFields, FInputFieldsData) or not (FInputFieldsData.JSONType in [jtArray, jtObject]) then
      raise Exception.CreateFmt('Invalid InputFields: "%s"', [FInputFields])
  end;
  if FJSONFields <> '' then
  begin
    if not TryStrToJSON(FJSONFields, FJSONFieldsData) then
      raise Exception.CreateFmt('Invalid JSONFields: "%s"', [FJSONFields]);
  end;
  if FQueryParams <> '' then
  begin
    if not TryStrToJSON(FQueryParams, FQueryParamsData) then
      raise Exception.CreateFmt('Invalid QueryParams: "%s"', [FQueryParams]);
  end;
end;

procedure TSqldbResource.PrepareParams(Params: TParams; ARequest: TRequest);
begin
  Params.Clear;
  if FQueryParamsData <> nil then
    QueryFieldsToParams(ARequest.QueryFields, FQueryParamsData, Params);
  JSONDataToParams(URIParams, Params);
end;

procedure TSqldbResource.PrepareQuery(Query: TSQLQuery;
  MethodType: THTTPMethodType; const ASelectSQL, AConditionsSQL: String);
begin
  Query.SQL.Add(ASelectSQL);
  //IsCollection = true only in GET or POST
  if FIsCollection or (AConditionsSQL <> '') then
    Query.SQL.Add(AConditionsSQL)
  else
    Query.SQL.Add(GetResourceIdentifierSQL);
end;

procedure TSqldbResource.PrepareResponse(ResponseData: TJSONData);
begin
  //
end;

procedure TSqldbResource.SetPrimaryKeyData(Query: TSQLQuery; Params: TJSONObject);
var
  ParamData: TJSONData;
  PKField: TField;
begin
  if FPrimaryKeyParam <> '' then
  begin
    PKField := Query.FindField(FPrimaryKey);
    if PKField = nil then
      raise Exception.CreateFmt('Field "%s" (PrimaryKey) not found', [FPrimaryKey]);
    ParamData := Params.Find(FPrimaryKeyParam);
    if ParamData <> nil then
      PKField.Value := ParamData.Value
    else
      raise Exception.CreateFmt('Param "%s" (PrimaryKeyParam) not specified', [FPrimaryKeyParam]);
  end;
end;

destructor TSqldbResource.Destroy;
begin
  FJSONFieldsData.Free;
  FInputFieldsData.Free;
  inherited Destroy;
end;

class function TSqldbResource.GetSelectSQL: String;
begin
  Result := '';
end;

procedure TSqldbResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  Query: TSQLQuery;
  ResponseData: TJSONData;
  ConvertOptions: TDatasetToJSONOptions;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    PrepareQuery(Query, hmtGet, FSelectSQL, FConditionsSQL);
    PrepareParams(Query.Params, ARequest);
    try
      Query.Open;
    except
      on E: Exception do
      begin
        SetResponseStatus(AResponse, 500, 'An exception ocurred opening a query: %s - SQL: %s', [E.Message, Query.SQL.Text] );
        raise;
      end;
    end;
    if FIsCollection then
    begin
      ConvertOptions := [djoSetNull];
      if FPreserveCase then
        Include(ConvertOptions, djoPreserveCase);
      ResponseData := TJSONArray.Create;
      try
        DatasetToJSON(Query, TJSONArray(ResponseData), ConvertOptions, FOutputFields);
        DecodeJSONFields(TJSONArray(ResponseData));
        PrepareResponse(ResponseData);
        AResponse.Contents.Add(ResponseData.AsJSON);
      finally
        ResponseData.Free;
      end;
    end
    else
    begin
      if (Query.RecordCount > 0) then
      begin
        ConvertOptions := [djoSetNull];
        if FPreserveCase then
          Include(ConvertOptions, djoPreserveCase);
        ResponseData := TJSONObject.Create;
        try
          DatasetToJSON(Query, TJSONObject(ResponseData), ConvertOptions, FOutputFields);
          DecodeJSONFields(TJSONObject(ResponseData));
          PrepareResponse(ResponseData);
          AResponse.Contents.Add(ResponseData.AsJSON);
        finally
          ResponseData.Free;
        end;
      end
      else
      begin
        if FIgnoreNotFound then
          AResponse.Contents.Add('{}')
        else
          SetResponseStatus(AResponse, 404, 'Resource "%s" not found', [TRESTRequest.ResourcePath]);
      end;
    end;
  finally
    Query.Destroy;
  end;
end;

procedure TSqldbResource.HandleDelete(ARequest: TRequest; AResponse: TResponse);
var
  Query: TSQLQuery;
begin
  if FIsCollection then
  begin
    SetResponseStatus(AResponse, 405, InvalidCollectionMethodError, ['DELETE']);
    Exit;
  end;
  if FReadOnly then
  begin
    SetResponseStatus(AResponse, 405, ReadOnlyAccessError, ['PUT']);
    Exit;
  end;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    PrepareQuery(Query, hmtDelete, FSelectSQL, FConditionsSQL);
    PrepareParams(Query.Params, ARequest);
    try
      Query.Open;
    except
      on E: Exception do
      begin
        SetResponseStatus(AResponse, 500, 'An exception ocurred opening a query:  %s - SQL: %s', [E.Message, Query.SQL.Text]);
        raise;
      end;
    end;
    if not Query.IsEmpty then
    begin
      Query.Delete;
      Query.ApplyUpdates;
      FConnection.Transaction.Commit;
    end;
  finally
    Query.Destroy;
  end;
end;

procedure TSqldbResource.HandlePatch(ARequest: TRequest;
  AResponse: TResponse);
var
  SavedPutAsPatch: Boolean;
begin
  //todo: refactor to get a cleaner implementation
  SavedPutAsPatch := PutAsPatch;
  PutAsPatch := True;
  try
    HandlePut(ARequest, AResponse);
  finally
    PutAsPatch := SavedPutAsPatch;
  end;
end;

procedure TSqldbResource.HandlePost(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  Query: TSQLQuery;
  PKField: TField;
  NewResourcePath, NewResourceId: String;
begin
  if not FIsCollection then
  begin
    SetResponseStatus(AResponse, 405, 'POST allowed only for collection resources', []);
    Exit;
  end;
  if FReadOnly then
  begin
    SetResponseStatus(AResponse, 405, ReadOnlyAccessError, ['POST']);
    Exit;
  end;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    PrepareQuery(Query, hmtPost, FSelectSQL, 'where 1 <> 1');
    PrepareParams(Query.Params, ARequest);
    if TryStrToJSON(ARequest.Content, RequestData) then
    begin
      try
        Query.Open;
        //workaround to an issue with firebird that sets Required in PK PKField
        PKField := Query.FindField(FPrimaryKey);
        if PKField <> nil then
          PKField.Required := False;
        Query.Append;
        SetQueryData(Query, RequestData, URIParams);
        Query.Post;
        NewResourceId := InsertRecord(Query);
      except
        on E: Exception do
        begin
          SetResponseStatus(AResponse, 500, 'Error posting to %s: %s - SQL: %s', [ARequest.PathInfo, E.Message, Query.SQL.Text]);
          raise;
        end;
      end;
      RequestData.Free;
    end
    else
    begin
      SetResponseStatus(AResponse, 400, 'Error posting to %s. Unable to decode request payload: "%s"', [ARequest.PathInfo, ARequest.Content]);
      Exit;
    end;
    if NewResourceId <> '' then
    begin
      NewResourcePath := ARequest.PathInfo;
      if NewResourcePath[Length(NewResourcePath)] <> '/' then
        NewResourcePath := NewResourcePath + '/';
      NewResourcePath := NewResourcePath + NewResourceId;
      RedirectRequest(ARequest, AResponse, 'GET', NewResourcePath, False);
    end
    else
      SetResponseStatus(AResponse, 400, '"%s" - Unable to get resource id', [ARequest.PathInfo]);
  finally
    Query.Destroy;
  end;
end;

procedure TSqldbResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  Query: TSQLQuery;
begin
  if FIsCollection then
  begin
    SetResponseStatus(AResponse, 405, InvalidCollectionMethodError, ['PUT']);
    Exit;
  end;
  if FReadOnly then
  begin
    SetResponseStatus(AResponse, 405, ReadOnlyAccessError, ['PUT']);
    Exit;
  end;
  Query := TSQLQuery.Create(nil);
  RequestData := nil;
  try
    Query.DataBase := FConnection;
    PrepareQuery(Query, hmtPut, FSelectSQL, FConditionsSQL);
    PrepareParams(Query.Params, ARequest);
    Query.Open;
    if TryStrToJSON(ARequest.Content, RequestData) then
    try
      if Query.RecordCount > 0 then
        Query.Edit
      else
      begin
        //todo: replace PutAsPatch by local check for patch SameText(ARequest.Method, 'PATCH')
        if not FPutAsPatch then
        begin
          Query.Append;
          SetPrimaryKeyData(Query, URIParams);
        end
        else
        begin
          SetResponseStatus(AResponse, 404, 'Resource does not exists', []);
          Exit;
        end;
      end;
      SetQueryData(Query, RequestData, URIParams, FPutAsPatch);
      Query.Post;
      Query.ApplyUpdates;
      FConnection.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        SetResponseStatus(AResponse, 500, 'An exception ocurred updating: %s - SQL: %s', [E.Message, Query.SQL.Text]);
        raise;
      end;
    end;
    RedirectRequest(ARequest, AResponse, 'GET', ARequest.PathInfo, False);
  finally
    RequestData.Free;
    Query.Destroy;
  end;
end;

{ TSqldbCollectionResource }

procedure TSqldbCollectionResource.CreateItemResource(out
  Resource: TRESTResource; ResourceTag: PtrInt);
var
  ResourceClass: TSqldbResourceClass;
  ItemResource: TSqldbResource absolute Resource;
begin
  ResourceClass := GetItemClass;
  if ResourceClass = nil then
    raise Exception.CreateFmt('ItemClass not defined for %s', [ClassName]);
  ItemResource := ResourceClass.Create;
  if ItemResource.SelectSQL = '' then
  begin
    ItemResource.SelectSQL := SelectSQL;
    ItemResource.PrimaryKey := PrimaryKey;
  end;
  if ItemResource.PrimaryKeyParam = '' then
    ItemResource.PrimaryKeyParam := GetItemParam;
  if ItemResource.InputFields = '' then
    ItemResource.InputFields := InputFields;
  if ItemResource.JSONFields = '' then
    ItemResource.JSONFields := JSONFields;
  ItemResource.IgnoreNotFound := IgnoreNotFound;
  PrepareItem(ItemResource);
end;

class function TSqldbCollectionResource.IsItemIdValid(const ItemId: String): Boolean;
var
  Num: Double;
begin
  Result := TryStrToFloat(ItemId, Num);
end;

class function TSqldbCollectionResource.GetItemClass: TSqldbResourceClass;
begin
  Result := TSqldbResource;
end;

class function TSqldbCollectionResource.GetItemParam: String;
begin
  Result := '';
  raise Exception.CreateFmt('GetItemParam not set. Class: %s Path: %s', [ClassName, TRESTRequest.ResourcePath]);
end;

class function TSqldbCollectionResource.GetItemParamType: String;
begin
  Result := '';
end;

procedure TSqldbCollectionResource.HandleSubPath(const SubPath: String;
  var SubPathResourceDef: TRESTResourceDef);
begin
  if SubPathResources <> nil then
  begin
    SubPathResourceDef := SubPathResources.Find(SubPath);
    if (SubPathResourceDef = nil) and IsItemIdValid(SubPath) then
    begin
      SubPathResourceDef := SubPathResources.DefaultResourceDef;
      SetURIParam(SubPathParamName, SubPath);
    end;
  end;
end;

procedure TSqldbCollectionResource.Loaded(Tag: PtrInt);
var
  ItemClass: TSqldbResourceClass;
begin
  inherited Loaded(Tag);
  if FSelectSQL = '' then
  begin
    ItemClass := GetItemClass;
    if ItemClass = nil then
      raise Exception.CreateFmt('ItemClass not defined for %s', [ClassName]);
    FSelectSQL := ItemClass.GetSelectSQL;
  end;
  SetDefaultSubPath(GetItemParam, @CreateItemResource, 0, GetItemParamType);
end;

procedure TSqldbCollectionResource.PrepareItem(ItemResource: TSqldbResource);
begin
  //
end;

constructor TSqldbCollectionResource.Create;
begin
  inherited Create;
  IsCollection := True;
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
  //poor implementation will fail if have params. must be incorporated into get through http header
  //alternative is to add all params. detect using regex
  FieldDefsStr := '';
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FSqldbResource.Connection;
    Query.SQL.Add(FSqldbResource.SelectSQL);
    Query.SQL.Add(FSqldbResource.ConditionsSQL);
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


end.

