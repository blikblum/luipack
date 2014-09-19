unit LuiSqlite3Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, LuiDataClasses, fpjson, db, contnrs;

type
   //todo: unify cache mode with RESTClient
   TSqlite3CacheMode = (cmNone, cmSession, cmLocal);

   TSqlite3ResourceClient = class;

  { TSqlite3ResourceModelDef }

   TSqlite3ResourceModelDef = class(TCollectionItem)
   private
     FCacheMode: TSqlite3CacheMode;
     FConditionsSQL: String;
     FInputFields: String;
     FInputFieldsData: TJSONArray;
     FJSONFields: String;
     FJSONFieldsData: TJSONArray;
     FName: String;
     FParams: TParams;
     FPrimaryKey: String;
     FSelectSQL: String;
     FTableName: String;
     procedure SetDatasetData(Dataset: TDataset; JSONObj: TJSONObject; Params: TParams; DoPatch: Boolean);
     function GetFieldName(FieldIndex: Integer; out DBFieldName: String): String;
     function GetUpdateSQL(const ResourceId: Variant): String;
     procedure SetInputFields(const AValue: String);
     procedure SetJSONFields(const Value: String);
     procedure SetParams(AValue: TParams);
   protected
     function GetDisplayName: string; override;
   public
     constructor Create(ACollection: TCollection); override;
     destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
   published
     property CacheMode: TSqlite3CacheMode read FCacheMode write FCacheMode default cmNone;
     property ConditionsSQL: String read FConditionsSQL write FConditionsSQL;
     property InputFields: String read FInputFields write SetInputFields;
     property JSONFields: String read FJSONFields write SetJSONFields;
     property Name: String read FName write FName;
     property Params: TParams read FParams write SetParams;
     property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
     property SelectSQL: String read FSelectSQL write FSelectSQL;
     property TableName: String read FTableName write FTableName;
   end;

   { TSqlite3ResourceModelDefs }

   TSqlite3ResourceModelDefs = class(TCollection)
   private
     FOwner: TSqlite3ResourceClient;
   protected
     function GetOwner: TPersistent; override;
     procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
   public
     constructor Create(AOwner: TSqlite3ResourceClient);
   end;

   { TSqlite3DataResource }

   TSqlite3DataResource = class(TInterfacedObject)
   private
     FDataset: TSqlite3Dataset;
     FModelDef: TSqlite3ResourceModelDef;
     FResourceClient: TSqlite3ResourceClient;
     FParams: TParams;
   protected
     function BindParams(const SQLTemplate: String): String;
     property ModelDef: TSqlite3ResourceModelDef read FModelDef;
   public
     constructor Create(AModelDef: TSqlite3ResourceModelDef; ResourceClient: TSqlite3ResourceClient); virtual;
     destructor Destroy; override;
     function GetParams: TParams;
     function ParamByName(const ParamName: String): TParam;
     property Params: TParams read GetParams;
   end;

   //todo: abstract cache handler so it can be plugged another implementation

   { TSqlite3CacheHandler }

   TSqlite3CacheHandler = class
   private
     FModelCacheList: TFPHashObjectList;
   public
     constructor Create;
     destructor Destroy; override;
     function GetCacheData(const ModelName, Path: String): TStream;
     procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
     procedure Invalidate(const ModelName: String);
   end;

   { TSqlite3ResourceClient }

   TSqlite3ResourceClient = class(TComponent, IResourceClient)
   private
     FCacheHandler: TSqlite3CacheHandler;
     FDatabase: String;
     FModelDefs: TSqlite3ResourceModelDefs;
     FModelDefLookup: TFPHashObjectList;
     procedure BuildModelDefLookup;
     procedure CacheHandlerNeeded;
     function FindModelDef(const ModelName: String): TSqlite3ResourceModelDef;
     function GetCacheData(const ModelName, ResourcePath: String;
       DataResource: TSqlite3DataResource): Boolean;
     procedure SetModelDefs(AValue: TSqlite3ResourceModelDefs);
     procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
   protected
     procedure ModelDefsChanged;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     function GetJSONArray(const ModelName: String): IJSONArrayResource;
     function GetJSONObject(const ModelName: String): IJSONObjectResource;
     procedure InvalidateCache(const ModelName: String);
   published
     property Database: String read FDatabase write FDatabase;
     property ModelDefs: TSqlite3ResourceModelDefs read FModelDefs write SetModelDefs;
   end;


implementation

uses
  LuiJSONUtils, variants;

type
  { TSqlite3JSONArrayResource }

  TSqlite3JSONArrayResource = class(TSqlite3DataResource, IJSONArrayResource)
  private
    //todo: implement save through dirty checking
    //FSnapshot/FReference: TJSONArray;
    FData: TJSONArray;
    procedure DecodeJSONFields(ResponseData: TJSONArray);
  protected
  public
    constructor Create(AModelDef: TSqlite3ResourceModelDef; ResourceClient: TSqlite3ResourceClient); override;
    destructor Destroy; override;
    function Fetch: Boolean;
    function GetData: TJSONArray;
    function Save: Boolean;
    property Data: TJSONArray read GetData;
  end;

  { TSqlite3JSONObjectResource }

  TSqlite3JSONObjectResource = class(TSqlite3DataResource, IJSONObjectResource)
  private
    //todo: implement save through dirty checking
    //FSnapshot/FReference: TJSONObject;
    FData: TJSONObject;
    FIdValue: Variant;
    FOwnsData: Boolean;
    procedure DecodeJSONFields(ResponseData: TJSONObject);
    function DoFetch(const Id: Variant): Boolean;
    function DoSave(const Id: Variant): Boolean;
    procedure EncodeJSONFields(RequestData: TJSONObject);
    procedure SetPrimaryKeyValue(const IdValue: Variant);
    procedure SetSQL(const Id: Variant);
  protected
  public
    constructor Create(AModelDef: TSqlite3ResourceModelDef; ResourceClient: TSqlite3ResourceClient); override;
    destructor Destroy; override;
    function Delete: Boolean;
    function Fetch: Boolean;
    function Fetch(IdValue: Variant): Boolean;
    function GetData: TJSONObject;
    function Save: Boolean;
    function Save(IdValue: Variant): Boolean;
    procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
    property Data: TJSONObject read GetData;
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

{ TSqlite3CacheHandler }

constructor TSqlite3CacheHandler.Create;
begin
  FModelCacheList := TFPHashObjectList.Create(True);
end;

destructor TSqlite3CacheHandler.Destroy;
begin
  FModelCacheList.Destroy;
  inherited Destroy;
end;

function TSqlite3CacheHandler.GetCacheData(const ModelName, Path: String): TStream;
var
  ModelCache: TFPHashObjectList;
begin
  //todo: convert path to a hash to avoid the 255 size limit
  ModelCache := TFPHashObjectList(FModelCacheList.Find(ModelName));
  if ModelCache <> nil then
    Result := TMemoryStream(ModelCache.Find(Path))
  else
    Result := nil;
end;

procedure TSqlite3CacheHandler.UpdateCache(const ModelName, Path: String;
  Stream: TStream);
var
  ModelCache: TFPHashObjectList;
  CacheData: TMemoryStream;
begin
  //todo: convert path to a hash to avoid the 255 size limit
  ModelCache := TFPHashObjectList(FModelCacheList.Find(ModelName));
  if ModelCache = nil then
  begin
    ModelCache := TFPHashObjectList.Create(True);
    FModelCacheList.Add(ModelName, ModelCache);
    CacheData := nil;
  end
  else
  begin
    CacheData := TMemoryStream(ModelCache.Find(Path));
  end;
  if CacheData = nil then
  begin
    CacheData := TMemoryStream.Create;
    ModelCache.Add(Path, CacheData);
  end
  else
    CacheData.Clear;
  Stream.Position := 0;
  CacheData.CopyFrom(Stream, Stream.Size);
  Stream.Position := 0;
end;

procedure TSqlite3CacheHandler.Invalidate(const ModelName: String);
var
  ModelCache: TObject;
begin
  ModelCache := FModelCacheList.Find(ModelName);
  if ModelCache <> nil then
    FModelCacheList.Remove(ModelCache);
end;

{ TRESTJSONObjectResource }

procedure TSqlite3JSONObjectResource.DecodeJSONFields(ResponseData: TJSONObject);
begin
  if FModelDef.FJSONFieldsData = nil then
    Exit;
  DoDecodeJSONFields(ResponseData, FModelDef.FJSONFieldsData);
end;

function TSqlite3JSONObjectResource.DoFetch(const Id: Variant): Boolean;
begin
  Result := True;
  try
    SetSQL(Id);
    FDataset.Open;
    try
      FData.Clear;
      DatasetToJSON(FDataset, FData, [djoSetNull], '');
      DecodeJSONFields(FData);
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

function TSqlite3JSONObjectResource.DoSave(const Id: Variant): Boolean;
var
  SQL: String;
  TempData: TJSONObject;
  IsAppend: Boolean;
begin
  Result := True;
  try
    SQL := FModelDef.GetUpdateSQL(Id);
    FDataset.SQL := BindParams(SQL);
    FDataset.Open;
    try
      IsAppend := FDataset.RecordCount = 0;
      if IsAppend then
      begin
        FDataset.Append;
        if not (VarIsNull(Id) or VarIsEmpty(Id)) then
          SetPrimaryKeyValue(Id);
      end
      else
        FDataset.Edit;
      //quick hack to save properties in correct format
      //todo: optimize to avoid data copy. Normalize FJSONFieldData as a hash to allow quick lookup
      if FModelDef.FJSONFieldsData <> nil then
      begin
        TempData := TJSONObject(FData.Clone);
        try
          EncodeJSONFields(TempData);
          FModelDef.SetDatasetData(FDataset, TempData, FParams, False);
        finally
          TempData.Destroy;
        end;
      end
      else
        FModelDef.SetDatasetData(FDataset, FData, FParams, False);
      FDataset.Post;
      Result := FDataset.ApplyUpdates;
      if IsAppend and Result then
        FData.Int64s[FModelDef.PrimaryKey] := FDataset.LastInsertRowId;
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

procedure TSqlite3JSONObjectResource.EncodeJSONFields(RequestData: TJSONObject);
var
  FieldDefData: TJSONData;
  PropType: TJSONtype;
  PropName: String;
  PropData: TJSONData;
  PropTypeName: TJSONStringType;
  i: Integer;
begin
  if FModelDef.FJSONFieldsData = nil then
    Exit;
  for i := 0 to FModelDef.FJSONFieldsData.Count - 1 do
  begin
    //todo parse fielddef once (move out of here)
    FieldDefData := FModelDef.FJSONFieldsData.Items[i];
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

procedure TSqlite3JSONObjectResource.SetPrimaryKeyValue(const IdValue: Variant);
var
  PKField: TField;
begin
  PKField := FDataset.FindField(FModelDef.PrimaryKey);
  if PKField = nil then
    raise Exception.CreateFmt('Field "%s" (PrimaryKey) not found', [FModelDef.PrimaryKey]);
  if not VarIsNull(IdValue) then
    PKField.Value := IdValue
  else
    raise Exception.CreateFmt('PrimaryKey ("%s") value not specified', [FModelDef.PrimaryKey]);
end;

procedure TSqlite3JSONObjectResource.SetSQL(const Id: Variant);
var
  SQL: String;
  IdValue: String;
begin
  SQL := FModelDef.SelectSQL;
  if not (VarIsEmpty(Id) or VarIsNull(Id)) then
  begin
    if VarIsStr(Id) then
      IdValue := '''' + VarToStr(Id) + ''''
    else
      IdValue := VarToStr(Id);
    SQL := SQL + Format(' Where %s = %s', [FModelDef.PrimaryKey, IdValue]);
  end
  else
    SQL := SQL + ' ' + FModelDef.FConditionsSQL;
  FDataset.SQL := BindParams(SQL);
end;

constructor TSqlite3JSONObjectResource.Create(AModelDef: TSqlite3ResourceModelDef;
  ResourceClient: TSqlite3ResourceClient);
begin
  inherited Create(AModelDef, ResourceClient);
  FOwnsData := True;
  FData := TJSONObject.Create;
end;

destructor TSqlite3JSONObjectResource.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

function TSqlite3JSONObjectResource.Delete: Boolean;
var
  IdFieldData: TJSONData;
  Id: String;
begin
  Result := False;
  if VarIsEmpty(FIdValue) or VarIsNull(FIdValue) then
  begin
    if FData = nil then
    begin
      //FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Data not set');
      Exit;
    end;
    IdFieldData := FData.Find(FDataset.PrimaryKey);
    if IdFieldData <> nil then
    begin
      if (IdFieldData.JSONType in [jtString, jtNumber]) then
        Id := IdFieldData.AsString
      else
      begin
        //FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field must be string or number');
        Exit;
      end
    end
    else
    begin
      //FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field not set');
      Exit;
    end;
  end
  else
    Id := VarToStr(FIdValue);

  Result := True;
  try
    SetSQL(Id);
    FDataset.Open;
    try
      if not FDataset.IsEmpty then
      begin
        FDataset.Delete;
        FDataset.ApplyUpdates;
      end;
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

function TSqlite3JSONObjectResource.Fetch: Boolean;
var
  IdFieldData: TJSONData;
  IdField, Id: String;
begin
  IdField := FModelDef.PrimaryKey;
  if IdField <> '' then
  begin
    if (FData = nil) then
    begin
      Result := False;
      Exit;
    end;
    IdFieldData := FData.Find(IdField);
    if (IdFieldData = nil) or not (IdFieldData.JSONType in [jtString, jtNumber]) then
    begin
      //todo error handling
      Result := False;
      Exit;
    end;
    Id := IdFieldData.AsString;
  end
  else
    Id := '';
  FIdValue := Unassigned;
  Result := DoFetch(Id);
end;

function TSqlite3JSONObjectResource.Fetch(IdValue: Variant): Boolean;
begin
  FIdValue := IdValue;
  Result := DoFetch(IdValue);
end;

function TSqlite3JSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

function TSqlite3JSONObjectResource.Save: Boolean;
var
  IdFieldData: TJSONData;
  Id: Variant;
begin
  Result := False;
  if VarIsEmpty(FIdValue) or VarIsNull(FIdValue) then
  begin
    if FData = nil then
    begin
      //true or false??
      Result := True;
      Exit;
    end
    else
    begin
      IdFieldData := FData.Find(FDataset.PrimaryKey);
      if (IdFieldData <> nil) then
      begin
        if (IdFieldData.JSONType in [jtString, jtNumber]) then
          Id := IdFieldData.Value
        else
        begin
          //FResourceClient.DoError(GetResourcePath, reRequest, 0, 'Save: Id field must be string or number');
          Exit;
        end
      end
      else
        Id := Null;
    end;
  end
  else
    Id := FIdValue;
  Result := DoSave(Id);
end;

function TSqlite3JSONObjectResource.Save(IdValue: Variant): Boolean;
begin
  FIdValue := IdValue;
  Result := DoSave(IdValue);
end;

procedure TSqlite3JSONObjectResource.SetData(JSONObj: TJSONObject; OwnsData: Boolean);
begin
  if FOwnsData then
    FData.Free;
  FData := JSONObj;
  FOwnsData := OwnsData;
end;

{ TSqlite3ResourceModelDefs }

function TSqlite3ResourceModelDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSqlite3ResourceModelDefs.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action = cnAdded then
    TSqlite3ResourceModelDef(Item).FPrimaryKey := 'id';
  if not (csDestroying in FOwner.ComponentState) then
    FOwner.ModelDefsChanged;
end;

constructor TSqlite3ResourceModelDefs.Create(AOwner: TSqlite3ResourceClient);
begin
  inherited Create(TSqlite3ResourceModelDef);
  FOwner := AOwner;
end;

{ TSqlite3DataResource }

constructor TSqlite3DataResource.Create(AModelDef: TSqlite3ResourceModelDef;
  ResourceClient: TSqlite3ResourceClient);
begin
  FDataset := TSqlite3Dataset.Create(nil);
  FDataset.PrimaryKey := AModelDef.PrimaryKey;
  FDataset.TableName := AModelDef.TableName;
  FDataset.FileName := ResourceClient.Database;
  FParams := TParams.Create(TParam);
  FParams.Assign(AModelDef.Params);
  FModelDef := AModelDef;
  FResourceClient := ResourceClient;
end;

destructor TSqlite3DataResource.Destroy;
begin
  FDataset.Destroy;
  FParams.Destroy;
  inherited Destroy;
end;

function TSqlite3DataResource.BindParams(const SQLTemplate: String): String;
var
  Param: TParam;
  i: Integer;
begin
  Result := SQLTemplate;
  for i := 0 to FParams.Count - 1 do
  begin
    //todo: handle InputFields
    Param := FParams.Items[i];
    Result := StringReplace(Result, ':' + Param.Name, Param.AsString,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TSqlite3DataResource.GetParams: TParams;
begin
  Result := FParams;
end;

function TSqlite3DataResource.ParamByName(const ParamName: String): TParam;
begin
  Result := FParams.ParamByName(ParamName);
end;

{ TRESTJSONArrayResource }

function TSqlite3JSONArrayResource.GetData: TJSONArray;
begin
  Result := FData;
end;

constructor TSqlite3JSONArrayResource.Create(AModelDef: TSqlite3ResourceModelDef;
  ResourceClient: TSqlite3ResourceClient);
begin
  inherited Create(AModelDef, ResourceClient);
  FData := TJSONArray.Create;
end;

function TSqlite3JSONArrayResource.Save: Boolean;
begin
  Result := False;
end;

destructor TSqlite3JSONArrayResource.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure TSqlite3JSONArrayResource.DecodeJSONFields(ResponseData: TJSONArray);
var
  i: Integer;
begin
  if FModelDef.FJSONFieldsData = nil then
    Exit;
  for i := 0 to ResponseData.Count - 1 do
    DoDecodeJSONFields(ResponseData.Objects[i], FModelDef.FJSONFieldsData);
end;

function TSqlite3JSONArrayResource.Fetch: Boolean;
var
  SQL: String;
begin
  Result := True;
  try
    SQL := FModelDef.SelectSQL;
    if FModelDef.ConditionsSQL <> '' then
      SQL := SQL + ' ' + FModelDef.ConditionsSQL;
    FDataset.SQL := BindParams(SQL);
    FDataset.Open;
    try
      FData.Clear;
      DatasetToJSON(FDataset, FData, [djoSetNull], '');
      DecodeJSONFields(FData);
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

{ TSqlite3ResourceModelDef }

procedure TSqlite3ResourceModelDef.SetParams(AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TSqlite3ResourceModelDef.SetDatasetData(Dataset: TDataset;
  JSONObj: TJSONObject; Params: TParams; DoPatch: Boolean);
var
  i: Integer;
  FieldName, DBFieldName: String;
  PropData: TJSONData;
  Field: TField;
  Fields: TFields;
  Param: TParam;
begin
  if FInputFieldsData <> nil then
  begin
    for i := 0 to FInputFieldsData.Count - 1 do
    begin
      FieldName := GetFieldName(i, DBFieldName);
      Field := Dataset.FieldByName(DBFieldName);
      PropData := JSONObj.Find(FieldName);
      if PropData = nil then
      begin
        Param := Params.FindParam(FieldName);
        if (Param <> nil) and not Param.IsNull then
          Field.Value := Param.Value
        else
        begin
          if not DoPatch then
            Field.Value := Null;
        end;
      end
      else
        Field.Value := PropData.Value;
    end;
  end
  else
  begin
    // no input fields defined
    Fields := Dataset.Fields;
    for i := 0 to Fields.Count -1 do
    begin
      Field := Fields[i];
      FieldName := LowerCase(Field.FieldName);
      if SameText(FieldName, FPrimaryKey) then
        continue;
      PropData := JSONObj.Find(FieldName);
      if PropData <> nil then
        Field.Value := PropData.Value
      else
      begin
        if not DoPatch then
          Field.Value := Null;
      end;
    end;
  end;
end;

function TSqlite3ResourceModelDef.GetFieldName(FieldIndex: Integer; out DBFieldName: String): String;
var
  FieldData: TJSONData;
begin
  FieldData := FInputFieldsData.Items[FieldIndex];
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
  if Trim(Result) = '' then
    raise Exception.CreateFmt('Invalid input field name - model "%s" index "%d"', [FName, FieldIndex]);
end;

function TSqlite3ResourceModelDef.GetUpdateSQL(const ResourceId: Variant): String;
var
  i: Integer;
  DBFieldName: String;
  IdValue: String;
begin
  if FInputFieldsData = nil then
    Result := SelectSQL
  else
  begin
    Result := 'Select';
    for i := 0 to FInputFieldsData.Count - 1 do
    begin
      GetFieldName(i, DBFieldName);
      Result := Result + ' ' + DBFieldName;
      if i < (FInputFieldsData.Count - 1) then
        Result := Result + ',';
    end;
    Result := Result + ' from ' + FTableName;
  end;
  if VarIsNull(ResourceId) or VarIsEmpty(ResourceId) then
  begin
    Result := Result + ' Where 1 = -1';
  end
  else
  begin
    if VarIsStr(ResourceId) then
      IdValue := '''' + VarToStr(ResourceId) + ''''
    else
      IdValue := VarToStr(ResourceId);
    Result := Result + Format(' Where %s = %s', [PrimaryKey, IdValue]);
  end;
end;

procedure TSqlite3ResourceModelDef.SetInputFields(const AValue: String);
begin
  if FInputFields = AValue then Exit;
  FInputFields := AValue;
  FreeAndNil(FInputFieldsData);
  TryStrToJSON(AValue, FInputFieldsData);
end;

procedure TSqlite3ResourceModelDef.SetJSONFields(const Value: String);
begin
  if FJSONFields = Value then Exit;
  FJSONFields := Value;
  FreeAndNil(FJSONFieldsData);
  TryStrToJSON(Value, FJSONFieldsData);
end;

function TSqlite3ResourceModelDef.GetDisplayName: string;
begin
  Result := FName;
end;

constructor TSqlite3ResourceModelDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TParams.Create(TParam);
  FCacheMode := cmNone;
end;

destructor TSqlite3ResourceModelDef.Destroy;
begin
  FJSONFieldsData.Free;
  FInputFieldsData.Free;
  FParams.Destroy;
  inherited Destroy;
end;

procedure TSqlite3ResourceModelDef.Assign(Source: TPersistent);
begin
  if Source is TSqlite3ResourceModelDef then
  begin
     FPrimaryKey := TSqlite3ResourceModelDef(Source).FPrimaryKey;
     FName := TSqlite3ResourceModelDef(Source).FName;
     Params := TSqlite3ResourceModelDef(Source).Params;
     FSelectSQL := TSqlite3ResourceModelDef(Source).FSelectSQL;
     FJSONFields := TSqlite3ResourceModelDef(Source).FJSONFields;
  end
  else
    inherited Assign(Source);
end;

{ TSqlite3ResourceClient }

procedure TSqlite3ResourceClient.BuildModelDefLookup;
var
  i: Integer;
  ModelDef: TSqlite3ResourceModelDef;
begin
  for i := 0 to FModelDefs.Count - 1 do
  begin
    ModelDef := TSqlite3ResourceModelDef(FModelDefs.Items[i]);
    FModelDefLookup.Add(ModelDef.Name, ModelDef);
  end;
end;

procedure TSqlite3ResourceClient.CacheHandlerNeeded;
begin
  if FCacheHandler = nil then
    FCacheHandler := TSqlite3CacheHandler.Create;
end;

function TSqlite3ResourceClient.FindModelDef(const ModelName: String): TSqlite3ResourceModelDef;
begin
  if FModelDefLookup = nil then
  begin
    FModelDefLookup := TFPHashObjectList.Create(False);
    BuildModelDefLookup;
  end;
  Result := TSqlite3ResourceModelDef(FModelDefLookup.Find(ModelName));
  if Result = nil then
    raise Exception.CreateFmt('Unable to find resource model "%s"', [ModelName]);
end;

function TSqlite3ResourceClient.GetCacheData(const ModelName, ResourcePath: String;
  DataResource: TSqlite3DataResource): Boolean;
var
  CacheData: TStream;
begin
  CacheHandlerNeeded;
  CacheData := FCacheHandler.GetCacheData(ModelName, ResourcePath);
  Result := CacheData <> nil;
  if Result then
  begin
    CacheData.Position := 0;
    //Result := DataResource.ParseResponse(ResourcePath, hmtGet, CacheData);
  end;
end;

procedure TSqlite3ResourceClient.SetModelDefs(AValue: TSqlite3ResourceModelDefs);
begin
  FModelDefs.Assign(AValue);
end;

procedure TSqlite3ResourceClient.UpdateCache(const ModelName, Path: String;
  Stream: TStream);
begin
  CacheHandlerNeeded;
  FCacheHandler.UpdateCache(ModelName, Path, Stream);
end;

procedure TSqlite3ResourceClient.ModelDefsChanged;
begin
  FreeAndNil(FModelDefLookup);
end;

constructor TSqlite3ResourceClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModelDefs := TSqlite3ResourceModelDefs.Create(Self);
end;

destructor TSqlite3ResourceClient.Destroy;
begin
  FCacheHandler.Free;
  FModelDefLookup.Free;
  FModelDefs.Destroy;
  inherited Destroy;
end;

function TSqlite3ResourceClient.GetJSONArray(const ModelName: String): IJSONArrayResource;
var
  ModelDef: TSqlite3ResourceModelDef;
begin
  ModelDef := FindModelDef(ModelName);
  Result := TSqlite3JSONArrayResource.Create(ModelDef, Self);
end;

function TSqlite3ResourceClient.GetJSONObject(const ModelName: String): IJSONObjectResource;
var
  ModelDef: TSqlite3ResourceModelDef;
begin
  ModelDef := FindModelDef(ModelName);
  Result := TSqlite3JSONObjectResource.Create(ModelDef, Self);
end;

procedure TSqlite3ResourceClient.InvalidateCache(const ModelName: String);
begin
  CacheHandlerNeeded;
  FCacheHandler.Invalidate(ModelName);
end;

end.

