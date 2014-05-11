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

  { TSqldbJSONResource }

  TSqldbJSONResource = class(TRESTResource)
  private
    FConditionsSQL: String;
    FConnection: TSQLConnection;
    FInputFields: String;
    FJSONFields: String;
    FOutputFields: String;
    FPrimaryKey: String;
    FPrimaryKeyParam: String;
    FSelectSQL: String;
    FInputFieldsData: TJSONArray;
    FIgnoreNotFound: Boolean;
    FIsCollection: Boolean;
    FJSONFieldsData: TJSONArray;
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
    function InsertRecord(Query: TSQLQuery): String; virtual;
    procedure Loaded(Tag: PtrInt); override;
    procedure PrepareQuery(Query: TSQLQuery); virtual;
    procedure SetPrimaryKeyData(Query: TSQLQuery; Params: TJSONObject);
    procedure SetQueryData(Query: TSQLQuery; RequestData, Params: TJSONObject; DoPatch: Boolean = False); virtual;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
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
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property SelectSQL: String read FSelectSQL write FSelectSQL;
  end;

procedure JSONDataToParams(JSONObj: TJSONObject; Params: TParams);

implementation

uses
  LuiJSONUtils, dbconst;

type
  TSQLConnectionAccess = class(TSQLConnection)

  end;

  TSQLConnectorAccess = class(TSQLConnector)

  end;

procedure JSONDataToParams(JSONObj: TJSONObject; Params: TParams);
var
  i: Integer;
  Param: TParam;
  PropName: String;
begin
  Params.Clear;
  for i := 0 to JSONObj.Count -1 do
  begin
    PropName := JSONObj.Names[i];
    Param := Params.FindParam(PropName);
    if Param = nil then
    begin
      Param := TParam.Create(Params, ptInput);
      Param.Name := PropName;
    end;
    Param.Value := JSONObj.Items[i].Value;
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


procedure TSqldbJSONResource.SetQueryData(Query: TSQLQuery; RequestData, Params: TJSONObject;
  DoPatch: Boolean);
var
  i: Integer;
  FieldName, DBFieldName: String;
  PropData: TJSONData;
  Field: TField;
begin
  EncodeJSONFields(RequestData);
  if FInputFieldsData <> nil then
  begin
    for i := 0 to FInputFieldsData.Count - 1 do
    begin
      FieldName := GetFieldName(FInputFieldsData, i, DBFieldName);
      Field := Query.FieldByName(DBFieldName);
      PropData := RequestData.Find(FieldName);
      if PropData = nil then
        PropData := Params.Find(FieldName);
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
      PropData := RequestData.Find(FieldName);
      if PropData = nil then
        PropData := Params.Find(FieldName);
      if (PropData <> nil) and not (PropData.JSONType in [jtObject, jtArray]) then
        Field.Value := PropData.Value
      else
      begin
        if not DoPatch then
          Field.Value := Null;
      end;
    end;
  end;
end;

{ TSqldbJSONResource }

class procedure TSqldbJSONResource.SetDefaultConnection(Value: TSQLConnection);
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

procedure TSqldbJSONResource.DecodeJSONFields(ResponseData: TJSONArray);
var
  i: Integer;
begin
  if FJSONFieldsData = nil then
    Exit;
  for i := 0 to ResponseData.Count - 1 do
    DoDecodeJSONFields(ResponseData.Objects[i], FJSONFieldsData);
end;

procedure TSqldbJSONResource.DecodeJSONFields(ResponseData: TJSONObject);
begin
  if FJSONFieldsData = nil then
    Exit;
  DoDecodeJSONFields(ResponseData, FJSONFieldsData);
end;

procedure TSqldbJSONResource.EncodeJSONFields(RequestData: TJSONObject);
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

function TSqldbJSONResource.GetQuery(AOwner: TComponent): TSQLQuery;
begin
  Result := TSQLQuery.Create(AOwner);
  Result.DataBase := FConnection;
end;

function TSqldbJSONResource.GetResourceIdentifierSQL: String;
begin
  if FConditionsSQL <> '' then
    Result := FConditionsSQL
  else if (FPrimaryKeyParam <> '') and (FPrimaryKey <> '') then
    Result := Format('where %s = :%s', [FPrimaryKey, FPrimaryKeyParam])
  else
    raise Exception.Create('Unable to resolve resource identifier SQL query');
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

function TSqldbJSONResource.InsertRecord(Query: TSQLQuery): String;
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
  Info := TSQLConnectionAccess(ActualConnection).GetStatementInfo(Query.SQL.Text, True, stNoSchema);
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

procedure TSqldbJSONResource.Loaded(Tag: PtrInt);
begin
  inherited Loaded(Tag);
  if FInputFields <> '' then
  begin
    if not TryStrToJSON(FInputFields, FInputFieldsData) then
      raise Exception.CreateFmt('Invalid InputFields: "%s"', [FInputFields]);
  end;
  if FJSONFields <> '' then
  begin
    if not TryStrToJSON(FJSONFields, FJSONFieldsData) then
      raise Exception.CreateFmt('Invalid JSONFields: "%s"', [FJSONFields]);
  end;
end;

procedure TSqldbJSONResource.PrepareQuery(Query: TSQLQuery);
begin
  //
end;

procedure TSqldbJSONResource.SetPrimaryKeyData(Query: TSQLQuery; Params: TJSONObject);
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

destructor TSqldbJSONResource.Destroy;
begin
  FJSONFieldsData.Free;
  FInputFieldsData.Free;
  inherited Destroy;
end;

procedure TSqldbJSONResource.AfterConstruction;
begin
  inherited AfterConstruction;
  FPrimaryKey := 'Id';
  FConnection := FDefaultConnection;
end;

procedure TSqldbJSONResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  Query: TSQLQuery;
  ResponseData: TJSONData;
  ConvertOptions: TDatasetToJSONOptions;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.SQL.Add(FSelectSQL);
    if FIsCollection then
      Query.SQL.Add(FConditionsSQL)
    else
      Query.SQL.Add(GetResourceIdentifierSQL);
    JSONDataToParams(URIParams, Query.Params);
    try
      Query.Open;
    except
      on E: Exception do
      begin
        SetResponseStatus(AResponse, 500, 'An exception ocurred opening a query: %s', [E.Message] );
        Exit;
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
          SetResponseStatus(AResponse, 404, 'Resource "%s" not found', [ARequest.PathInfo]);
      end;
    end;
  finally
    Query.Destroy;
  end;
end;

procedure TSqldbJSONResource.HandleDelete(ARequest: TRequest; AResponse: TResponse);
var
  Query: TSQLQuery;
begin
  if not FIsCollection and not FReadOnly then
  begin
    Query := TSQLQuery.Create(nil);
    try
      Query.DataBase := FConnection;
      Query.SQL.Add(FSelectSQL);
      Query.SQL.Add(GetResourceIdentifierSQL);
      PrepareQuery(Query);
      JSONDataToParams(URIParams, Query.Params);
      try
        Query.Open;
      except
        on E: Exception do
        begin
          SetResponseStatus(AResponse, 500, 'An exception ocurred opening a query: %s', [E.Message] );
          Exit;
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
  end
  else
    inherited HandleDelete(ARequest, AResponse);
end;

procedure TSqldbJSONResource.HandlePost(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  Query: TSQLQuery;
  PKField: TField;
  NewResourcePath, NewResourceId: String;
begin
  if FIsCollection and not FReadOnly then
  begin
    Query := TSQLQuery.Create(nil);
    try
      Query.DataBase := FConnection;
      Query.SQL.Add(FSelectSQL);
      Query.SQL.Add('where 1 <> 1');
      PrepareQuery(Query);
      JSONDataToParams(URIParams, Query.Params);
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
            //todo: return the effective Path instead of PathInfo
            AResponse.Contents.Add(E.Message);
            Exit;
          end;
        end;
        RequestData.Free;
      end
      else
      begin
        //todo: improve error handling
        SetResponseStatus(AResponse, 400, 'Error posting to %s', [ARequest.PathInfo]);
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
  end
  else
    inherited HandlePost(ARequest, AResponse);
end;

procedure TSqldbJSONResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  Query: TSQLQuery;
begin
  if not FIsCollection and not FReadOnly then
  begin
    Query := TSQLQuery.Create(nil);
    try
      Query.DataBase := FConnection;
      Query.SQL.Add(FSelectSQL);
      Query.SQL.Add(GetResourceIdentifierSQL);
      JSONDataToParams(URIParams, Query.Params);
      PrepareQuery(Query);
      Query.Open;
      if TryStrToJSON(ARequest.Content, RequestData) then
      try
        if Query.RecordCount > 0 then
          Query.Edit
        else
        begin
          Query.Append;
          SetPrimaryKeyData(Query, URIParams);
        end;
        SetQueryData(Query, RequestData, URIParams, FPutAsPatch);
        Query.Post;
        Query.ApplyUpdates;
        FConnection.Transaction.CommitRetaining;
      finally
        RequestData.Free;
      end;
      RedirectRequest(ARequest, AResponse, 'GET', ARequest.PathInfo, False);
    finally
      Query.Destroy;
    end;
  end
  else
    inherited HandlePut(ARequest, AResponse);
end;

end.

