unit SQLClientBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, fpjson, db, contnrs, typinfo;

type
   //todo: unify cache mode with RESTClient
   TSQLCacheMode = (cmNone, cmSession, cmLocal);

   TSQLResourceClient = class;

   { TSQLModelDefParam }

   TSQLModelDefParam = class(TParam)
   public
     constructor Create(ACollection: TCollection); overload; override;
   end;


  { TSQLModelDef }

   TSQLModelDef = class(TCollectionItem)
   private
     FCacheMode: TSQLCacheMode;
     FConditionsSQL: String;
     FDataField: String;
     FInputFields: TJSONArray;
     FJSONFields: TJSONArray;
     FName: String;
     FParams: TParams;
     FPrimaryKey: String;
     FSelectSQL: String;
     FTableName: String;
     FInParams: TStringList;
     procedure InParamsNeeded;
     procedure SetDatasetData(Dataset: TDataset; JSONObj: TJSONObject; Params: TParams; DoPatch: Boolean);
     function GetFieldName(FieldIndex: Integer; out DBFieldName: String): String;
     function GetUpdateSQL(const ResourceId: Variant): String;
     procedure SetInputFields(Value: TJSONArray);
     procedure SetJSONFields(Value: TJSONArray);
     procedure SetParams(AValue: TParams);
   protected
     function GetDisplayName: string; override;
   public
     constructor Create(ACollection: TCollection); override;
     destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
   published
     property CacheMode: TSQLCacheMode read FCacheMode write FCacheMode default cmNone;
     property ConditionsSQL: String read FConditionsSQL write FConditionsSQL;
     property DataField: String read FDataField write FDataField;
     property InputFields: TJSONArray read FInputFields write SetInputFields;
     property JSONFields: TJSONArray read FJSONFields write SetJSONFields;
     property Name: String read FName write FName;
     property Params: TParams read FParams write SetParams;
     property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
     property SelectSQL: String read FSelectSQL write FSelectSQL;
     property TableName: String read FTableName write FTableName;
   end;

   { TSQLResourceModelDefs }

   TSQLResourceModelDefs = class(TCollection)
   private
     FOwner: TSQLResourceClient;
     procedure RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
       AValue: TJSONData; var Handled: Boolean);
   protected
     function GetOwner: TPersistent; override;
     procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
   public
     constructor Create(AOwner: TSQLResourceClient);
     procedure LoadFromFile(const FileName: String);
   end;

   { TDatasetAdapter }

   TDatasetAdapter = class
   public
     class function ApplyUpdates(Dataset: TDataSet): Boolean; virtual; abstract;
     class function BindParams(const SQL: String; Params: TParams): String; virtual;
     class function CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet; virtual; abstract;
     class function CreateParams(Dataset: TDataSet): TParams; virtual; abstract;
     class procedure DestroyParams(Params: TParams); virtual;
     class function InsertRecord(Dataset: TDataSet; ModelDef: TSQLModelDef): Int64; virtual; abstract;
     class procedure SetSQL(Dataset: TDataSet; const SQL: String); virtual; abstract;
   end;

   TDatasetAdapterClass = class of TDatasetAdapter;

   { TSQLDataResource }

   TSQLDataResource = class(TInterfacedObject)
   private
     FDataset: TDataSet;
     FModelDef: TSQLModelDef;
     FAdapter: TDatasetAdapterClass;
     FParams: TParams;
   protected
     function BindParams(const SQL: String): String;
     property ModelDef: TSQLModelDef read FModelDef;
   public
     constructor Create(AModelDef: TSQLModelDef; Client: TSQLResourceClient); virtual;
     destructor Destroy; override;
     function GetParams: TParams;
     function ParamByName(const ParamName: String): TParam;
     property Params: TParams read GetParams;
   end;

   { TSQLJSONArrayResource }

   TSQLJSONArrayResource = class(TSQLDataResource, IJSONArrayResource)
   private
     //todo: implement save through dirty checking
     //FSnapshot/FReference: TJSONArray;
     FData: TJSONArray;
     procedure DecodeJSONFields(ResponseData: TJSONArray);
   protected
   public
     constructor Create(AModelDef: TSQLModelDef; Client: TSQLResourceClient); override;
     destructor Destroy; override;
     function Fetch: Boolean;
     function GetData: TJSONArray;
     function Save(Options: TSaveOptions = []): Boolean;
     property Data: TJSONArray read GetData;
   end;

   { TSQLJSONObjectResource }

   TSQLJSONObjectResource = class(TSQLDataResource, IJSONObjectResource)
   private
     //todo: implement save through dirty checking
     //FSnapshot/FReference: TJSONObject;
     FData: TJSONObject;
     FIdValue: Variant;
     FOwnsData: Boolean;
     procedure DecodeJSONFields(ResponseData: TJSONObject);
     function DoDelete(const Id: Variant): Boolean;
     function DoFetch(const Id: Variant): Boolean;
     function DoSave(const Id: Variant; Options: TSaveOptions = []): Boolean;
     procedure EncodeJSONFields(RequestData: TJSONObject);
   protected
   public
     constructor Create(AModelDef: TSQLModelDef; Client: TSQLResourceClient); override;
     destructor Destroy; override;
     function Delete: Boolean;
     function Delete(IdValue: Variant): Boolean;
     function Fetch: Boolean;
     function Fetch(IdValue: Variant): Boolean;
     function GetData: TJSONObject;
     function Save(Options: TSaveOptions = []): Boolean;
     function Save(IdValue: Variant; Options: TSaveOptions = []): Boolean;
     procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
     property Data: TJSONObject read GetData;
   end;

   { TSQLDatasetResource }

   TSQLDatasetResource = class(TSQLDataResource, IDatasetResource)
   private
     FIdValue: Variant;
     function DoFetch(const IdValue: Variant): Boolean;
   public
     function Fetch: Boolean;
     function Fetch(IdValue: Variant): Boolean;
     function GetDataset: TDataSet;
     function Save(Options: TSaveOptions = []): Boolean;
   end;

   //todo: abstract cache handler so it can be plugged another implementation

   { TSQLCacheHandler }

   TSQLCacheHandler = class
   private
     FModelCacheList: TFPHashObjectList;
   public
     constructor Create;
     destructor Destroy; override;
     function GetCacheData(const ModelName, Path: String): TStream;
     procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
     procedure Invalidate(const ModelName: String);
   end;


   { TSQLResourceClient }

   TSQLResourceClient = class(TComponent, IResourceClient)
   private
     FCacheHandler: TSQLCacheHandler;
     FDatabase: String;
     FModelDefs: TSQLResourceModelDefs;
     FModelDefLookup: TFPHashObjectList;
     procedure BuildModelDefLookup;
     procedure CacheHandlerNeeded;
     function FindModelDef(const ModelName: String): TSQLModelDef;
     function GetModelDef(const ModelName: String): TSQLModelDef;
     function GetCacheData(const ModelName, ResourcePath: String;
       DataResource: TSQLDataResource): Boolean;
     procedure SetModelDefs(AValue: TSQLResourceModelDefs);
     procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
   protected
     function GetAdapter: TDatasetAdapterClass; virtual; abstract;
     procedure ModelDefsChanged;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property Adapter: TDatasetAdapterClass read GetAdapter;
     function Connected: Boolean; virtual;
     function GetDataset(const ModelName: String): IDatasetResource;
     function GetJSONArray(const ModelName: String): IJSONArrayResource;
     function GetJSONObject(const ModelName: String): IJSONObjectResource;
     procedure InvalidateCache(const ModelName: String);
     function HasModel(const ModelName: String): Boolean;
   published
     property Database: String read FDatabase write FDatabase;
     property ModelDefs: TSQLResourceModelDefs read FModelDefs write SetModelDefs;
   end;


implementation

uses
  LuiJSONUtils, variants, RegExpr, fpjsonrtti;

procedure DoDecodeJSONFields(RecordData: TJSONObject; JSONFields: TJSONArray);
var
  i: Integer;
  FieldDefData, PropData, DecodedData: TJSONData;
  DecodedArrayData: TJSONArray absolute DecodedData;
  DecodedObjectData: TJSONObject absolute DecodedData;
  PropName, PropTypeName: String;
  PropType: TJSONtype;
begin
  for i := 0 to JSONFields.Count - 1 do
  begin
    //todo parse fielddef once (move out of here)
    FieldDefData := JSONFields.Items[i];
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

function GetItemSQL(const Id: Variant; ModelDef: TSQLModelDef): String;
var
  IdValue: String;
begin
  Result := ModelDef.SelectSQL;
  if not (VarIsEmpty(Id) or VarIsNull(Id)) then
  begin
    if VarIsStr(Id) then
      IdValue := '''' + VarToStr(Id) + ''''
    else
      IdValue := VarToStr(Id);
    Result := Result + Format(' Where %s = %s', [ModelDef.PrimaryKey, IdValue]);
  end
  else
    Result := Result + ' ' + ModelDef.FConditionsSQL;
end;

procedure SetPrimaryKeyValue(Dataset: TDataSet; ModelDef: TSQLModelDef; const IdValue: Variant);
var
  PKField: TField;
begin
  PKField := Dataset.FindField(ModelDef.PrimaryKey);
  if PKField = nil then
    raise Exception.CreateFmt('Field "%s" (PrimaryKey) not found', [ModelDef.PrimaryKey]);
  if not VarIsNull(IdValue) then
    PKField.Value := IdValue
  else
    raise Exception.CreateFmt('PrimaryKey ("%s") value not specified', [ModelDef.PrimaryKey]);
end;

{ TSQLModelDefParam }

constructor TSQLModelDefParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  ParamType := ptInput;
  DataType := ftInteger;
end;

{ TDatasetAdapter }

class function TDatasetAdapter.BindParams(const SQL: String; Params: TParams): String;
begin
  Result := SQL;
end;

class procedure TDatasetAdapter.DestroyParams(Params: TParams);
begin
  //
end;

{ TSqlite3DatasetResource }

function TSQLDatasetResource.DoFetch(const IdValue: Variant): Boolean;
begin
  Result := True;
  try
    FDataset.Close;
    FAdapter.SetSQL(FDataset, BindParams(GetItemSQL(IdValue, FModelDef)));
    FDataset.Open;
  except
    Result := False;
  end;
end;

function TSQLDatasetResource.GetDataset: TDataSet;
begin
  Result := FDataset;
end;

function TSQLDatasetResource.Fetch: Boolean;
begin
  FIdValue := Unassigned;
  Result := DoFetch(Null);
end;

function TSQLDatasetResource.Fetch(IdValue: Variant): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    FIdValue := IdValue;
    Result := DoFetch(IdValue);
  end
  else
    Result := Fetch();
end;

function TSQLDatasetResource.Save(Options: TSaveOptions): Boolean;
var
  Param: TParam;
  Field: TField;
  i: Integer;
begin
  if (FDataset.RecordCount > 0) or (FDataset.State = dsInsert) then
  begin
    FDataset.Edit;
    try
      //todo: move this code to descendant TDataset
      if not (VarIsNull(FIdValue) or VarIsEmpty(FIdValue)) then
        SetPrimaryKeyValue(FDataset, FModelDef, FIdValue);
      //todo: handle dataset with multiple records
      for i := 0 to Params.Count -1 do
      begin
        Param := Params[i];
        if Param.IsNull then
          continue;
        Field := FDataset.FindField(Param.Name);
        if Field <> nil then
          Field.Value := Param.Value;
      end;
    finally
      FDataset.Post;
    end;
  end;
  Result := FAdapter.ApplyUpdates(FDataset);
end;

{ TSQLCacheHandler }

constructor TSQLCacheHandler.Create;
begin
  FModelCacheList := TFPHashObjectList.Create(True);
end;

destructor TSQLCacheHandler.Destroy;
begin
  FModelCacheList.Destroy;
  inherited Destroy;
end;

function TSQLCacheHandler.GetCacheData(const ModelName, Path: String): TStream;
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

procedure TSQLCacheHandler.UpdateCache(const ModelName, Path: String;
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

procedure TSQLCacheHandler.Invalidate(const ModelName: String);
var
  ModelCache: TObject;
begin
  ModelCache := FModelCacheList.Find(ModelName);
  if ModelCache <> nil then
    FModelCacheList.Remove(ModelCache);
end;

{ TRESTJSONObjectResource }

procedure TSQLJSONObjectResource.DecodeJSONFields(ResponseData: TJSONObject);
begin
  if FModelDef.FJSONFields = nil then
    Exit;
  DoDecodeJSONFields(ResponseData, FModelDef.FJSONFields);
end;

function TSQLJSONObjectResource.DoDelete(const Id: Variant): Boolean;
begin
  Result := True;
  try
    FDataset.Close;
    FAdapter.SetSQL(FDataset, BindParams(GetItemSQL(Id, FModelDef)));
    FDataset.Open;
    try
      if not FDataset.IsEmpty then
      begin
        FDataset.Delete;
        FAdapter.ApplyUpdates(FDataset);
      end;
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

procedure LoadDataField(Dataset: TDataSet; Data: TJSONObject; const FieldName: String);
var
  Field: TField;
  FieldData: TJSONObject;
begin
  Field := Dataset.FieldByName(FieldName);
  if TryStrToJSON(Field.AsString, FieldData) then
  begin
    CopyJSONObject(FieldData, Data);
    FieldData.Free;
  end;
end;

procedure SaveDataField(Dataset: TDataSet; Data: TJSONObject; const FieldName: String);
begin
  Dataset.FieldByName(FieldName).AsString := Data.AsJSON;
end;

function TSQLJSONObjectResource.DoFetch(const Id: Variant): Boolean;
begin
  Result := True;
  try
    FAdapter.SetSQL(FDataset, BindParams(GetItemSQL(Id, FModelDef)));
    FDataset.Open;
    try
      FData.Clear;
      if FDataset.RecordCount > 0 then
      begin
        if FModelDef.DataField = '' then
        begin
          DatasetToJSON(FDataset, FData, [djoSetNull], '');
          DecodeJSONFields(FData);
        end
        else
        begin
          LoadDataField(FDataset, FData, FModelDef.DataField);
          if (VarIsNull(FIdValue) or VarIsEmpty(FIdValue)) and (FModelDef.PrimaryKey <> '') then
            FIdValue := FDataset.FieldByName(FModelDef.PrimaryKey).Value;
        end;
      end;
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

function TSQLJSONObjectResource.DoSave(const Id: Variant; Options: TSaveOptions): Boolean;
var
  SQL: String;
  TempData: TJSONObject;
  LastInsertId: Int64;
  IsAppend: Boolean;
begin
  Result := True;
  try
    SQL := FModelDef.GetUpdateSQL(Id);
    FAdapter.SetSQL(FDataset, BindParams(SQL));
    FDataset.Open;
    try
      IsAppend := FDataset.RecordCount = 0;
      if IsAppend then
      begin
        FDataset.Append;
        if not (VarIsNull(Id) or VarIsEmpty(Id)) then
          SetPrimaryKeyValue(FDataset, FModelDef, Id);
      end
      else
        FDataset.Edit;
      if FModelDef.DataField = '' then
      begin
        //quick hack to save properties in correct format
        //todo: optimize to avoid data copy. Normalize FJSONFieldData as a hash to allow quick lookup
        if FModelDef.FJSONFields <> nil then
        begin
          TempData := TJSONObject(FData.Clone);
          try
            EncodeJSONFields(TempData);
            FModelDef.SetDatasetData(FDataset, TempData, FParams, soPatch in Options);
          finally
            TempData.Destroy;
          end;
        end
        else
          FModelDef.SetDatasetData(FDataset, FData, FParams, soPatch in Options);
      end
      else
      begin
        TempData := TJSONObject.Create([FModelDef.DataField, FData.AsJSON]);
        try
          FModelDef.SetDatasetData(FDataset, TempData, FParams, True);
        finally
          TempData.Destroy;
        end;
      end;
      FDataset.Post;
      if IsAppend then
      begin
        LastInsertId := FAdapter.InsertRecord(FDataset, FModelDef);
        Result := LastInsertId <> -1;
        if FModelDef.DataField = '' then
          FData.Int64s[FModelDef.PrimaryKey] := LastInsertId
        else
          FIdValue := LastInsertId;
      end
      else
        Result := FAdapter.ApplyUpdates(FDataset);
    finally
      FDataset.Close;
    end;
  except
    Result := False;
  end;
end;

procedure TSQLJSONObjectResource.EncodeJSONFields(RequestData: TJSONObject);
var
  FieldDefData: TJSONData;
  PropType: TJSONtype;
  PropName: String;
  PropData: TJSONData;
  PropTypeName: TJSONStringType;
  i: Integer;
begin
  if FModelDef.FJSONFields = nil then
    Exit;
  for i := 0 to FModelDef.FJSONFields.Count - 1 do
  begin
    //todo parse fielddef once (move out of here)
    FieldDefData := FModelDef.FJSONFields.Items[i];
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

constructor TSQLJSONObjectResource.Create(AModelDef: TSQLModelDef;
  Client: TSQLResourceClient);
begin
  inherited Create(AModelDef, Client);
  FOwnsData := True;
  FData := TJSONObject.Create;
end;

destructor TSQLJSONObjectResource.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

function TSQLJSONObjectResource.Delete: Boolean;
var
  IdFieldData: TJSONData;
  Id: String;
begin
  Result := False;
  if VarIsEmpty(FIdValue) or VarIsNull(FIdValue) then
  begin
    if FData = nil then
    begin
      //FClient.DoError(ResourcePath, reRequest, 0, 'Delete: Data not set');
      Exit;
    end;
    IdFieldData := FData.Find(FModelDef.PrimaryKey);
    if IdFieldData <> nil then
    begin
      if (IdFieldData.JSONType in [jtString, jtNumber]) then
        Id := IdFieldData.AsString
      else
      begin
        //FClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field must be string or number');
        Exit;
      end
    end
    else
    begin
      //FClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field not set');
      Exit;
    end;
  end
  else
    Id := VarToStr(FIdValue);
  Result := DoDelete(Id);
end;

function TSQLJSONObjectResource.Delete(IdValue: Variant): Boolean;
begin
  if not VarIsEmpty(IdValue) then
    Result := DoDelete(IdValue)
  else
  begin
    //calling Delete without parentesis does not work
    //the compiler thinks is refering to the function result
    Result := Delete();
  end;
end;

function TSQLJSONObjectResource.Fetch: Boolean;
var
  IdFieldData: TJSONData;
  IdField: String;
  Id: Variant;
begin
  FIdValue := Unassigned;
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
    Id := IdFieldData.Value;
  end
  else
    Id := Null;
  Result := DoFetch(Id);
end;

function TSQLJSONObjectResource.Fetch(IdValue: Variant): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    FIdValue := IdValue;
    Result := DoFetch(IdValue);
  end
  else
    Result := Fetch();
end;

function TSQLJSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

function TSQLJSONObjectResource.Save(Options: TSaveOptions): Boolean;
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
      IdFieldData := FData.Find(FModelDef.PrimaryKey);
      if (IdFieldData <> nil) then
      begin
        if (IdFieldData.JSONType in [jtString, jtNumber, jtNull]) then
          Id := IdFieldData.Value
        else
        begin
          //FClient.DoError(GetResourcePath, reRequest, 0, 'Save: Id field must be string or number');
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

function TSQLJSONObjectResource.Save(IdValue: Variant; Options: TSaveOptions): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    Result := DoSave(IdValue, Options);
    if Result then
      FIdValue := IdValue;
  end
  else
  begin
    //calling Save without parentesis does not work
    //the compiler thinks is refering to the function result
    Result := Save();
  end;
end;

procedure TSQLJSONObjectResource.SetData(JSONObj: TJSONObject; OwnsData: Boolean);
begin
  if FOwnsData then
    FData.Free;
  FData := JSONObj;
  FOwnsData := OwnsData;
end;

{ TSQLResourceModelDefs }

procedure TSQLResourceModelDefs.RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
  AValue: TJSONData; var Handled: Boolean);
var
  TypeData: PTypeData;
begin
  if Info^.PropType^.Kind = tkClass then
  begin
    TypeData := GetTypeData(Info^.PropType);
    if TypeData^.ClassType.InheritsFrom(AValue.ClassType) then
    begin
      Handled := True;
      SetObjectProp(AObject, Info, AValue.Clone);
    end;
  end;
end;

function TSQLResourceModelDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSQLResourceModelDefs.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action = cnAdded then
    TSQLModelDef(Item).FPrimaryKey := 'id';
  if not (csDestroying in FOwner.ComponentState) then
    FOwner.ModelDefsChanged;
end;

constructor TSQLResourceModelDefs.Create(AOwner: TSQLResourceClient);
begin
  inherited Create(TSQLModelDef);
  FOwner := AOwner;
end;

procedure TSQLResourceModelDefs.LoadFromFile(const FileName: String);
var
  JSONDestreamer: TJSONDeStreamer;
  ModelDefsData: TJSONArray;
begin
  ModelDefsData := nil;
  JSONDestreamer := TJSONDeStreamer.Create(nil);
  JSONDestreamer.OnRestoreProperty := @RestoreProperty;
  try
    if TryReadJSONFile(FileName, ModelDefsData) then
    begin
      JSONDestreamer.JSONToCollection(ModelDefsData, Self);
      ModelDefsData.Destroy;
    end
    else
      raise Exception.CreateFmt('Unable to load modeldefs from "%s". Expected a JSON array file', [FileName]);
  finally
    JSONDestreamer.Destroy;
  end;
end;

{ TSQLDataResource }

constructor TSQLDataResource.Create(AModelDef: TSQLModelDef;
  Client: TSQLResourceClient);
begin
  FModelDef := AModelDef;
  FAdapter := Client.Adapter;
  FDataset := FAdapter.CreateDataset(Client, AModelDef);
  FParams := FAdapter.CreateParams(FDataset);
  FParams.Assign(AModelDef.Params);
end;

destructor TSQLDataResource.Destroy;
begin
  FDataset.Destroy;
  FAdapter.DestroyParams(FParams);
  inherited Destroy;
end;

function ReplaceInParams(const SQL: String; InParams: TStrings; ResourceParams: TParams): String;
var
  i: Integer;
  ParamName: String;
begin
  Result := SQL;
  for i := 0 to InParams.Count - 1 do
  begin
    ParamName := InParams[i];
    Result := StringReplace(Result, ':' + ParamName, ResourceParams.ParamByName(ParamName).AsString,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TSQLDataResource.BindParams(const SQL: String): String;
begin
  if FModelDef.FInParams = nil then
    Result := FAdapter.BindParams(SQL, FParams)
  else
    Result := FAdapter.BindParams(ReplaceInParams(SQL, FModelDef.FInParams, FParams), FParams);
end;

function TSQLDataResource.GetParams: TParams;
begin
  Result := FParams;
end;

function TSQLDataResource.ParamByName(const ParamName: String): TParam;
begin
  Result := FParams.ParamByName(ParamName);
end;

{ TRESTJSONArrayResource }

function TSQLJSONArrayResource.GetData: TJSONArray;
begin
  Result := FData;
end;

constructor TSQLJSONArrayResource.Create(AModelDef: TSQLModelDef;
  Client: TSQLResourceClient);
begin
  inherited Create(AModelDef, Client);
  FData := TJSONArray.Create;
end;

function TSQLJSONArrayResource.Save(Options: TSaveOptions): Boolean;
begin
  Result := False;
end;

destructor TSQLJSONArrayResource.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure TSQLJSONArrayResource.DecodeJSONFields(ResponseData: TJSONArray);
var
  i: Integer;
begin
  if FModelDef.FJSONFields = nil then
    Exit;
  for i := 0 to ResponseData.Count - 1 do
    DoDecodeJSONFields(ResponseData.Objects[i], FModelDef.FJSONFields);
end;

function TSQLJSONArrayResource.Fetch: Boolean;
var
  SQL: String;
begin
  Result := True;
  try
    SQL := FModelDef.SelectSQL;
    if FModelDef.ConditionsSQL <> '' then
      SQL := SQL + ' ' + FModelDef.ConditionsSQL;
    FAdapter.SetSQL(FDataset, BindParams(SQL));
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

{ TSQLModelDef }

procedure TSQLModelDef.SetParams(AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TSQLModelDef.InParamsNeeded;
begin
  if FInParams = nil then
  begin
    FInParams := TStringList.Create;
    FInParams.Duplicates := dupIgnore;
  end;
end;

procedure TSQLModelDef.SetDatasetData(Dataset: TDataset;
  JSONObj: TJSONObject; Params: TParams; DoPatch: Boolean);
var
  i: Integer;
  FieldName, DBFieldName: String;
  PropData: TJSONData;
  Field: TField;
  Fields: TFields;
  Param: TParam;
begin
  if FInputFields <> nil then
  begin
    for i := 0 to FInputFields.Count - 1 do
    begin
      FieldName := GetFieldName(i, DBFieldName);
      Field := Dataset.FieldByName(DBFieldName);
      Param := Params.FindParam(FieldName);
      if (Param <> nil) and not Param.IsNull then
        Field.Value := Param.Value
      else
      begin
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
      Param := Params.FindParam(FieldName);
      if (Param <> nil) and not Param.IsNull then
        Field.Value := Param.Value
      else
      begin
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
end;

function TSQLModelDef.GetFieldName(FieldIndex: Integer; out DBFieldName: String): String;
var
  FieldData: TJSONData;
begin
  FieldData := FInputFields.Items[FieldIndex];
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

function TSQLModelDef.GetUpdateSQL(const ResourceId: Variant): String;
var
  i: Integer;
  DBFieldName: String;
  IdValue: String;
begin
  if FInputFields = nil then
    Result := SelectSQL
  else
  begin
    if FTableName = '' then
      raise Exception.CreateFmt('Model "%s": TableName not defined', [Name]);
    Result := 'Select';
    for i := 0 to FInputFields.Count - 1 do
    begin
      GetFieldName(i, DBFieldName);
      Result := Result + ' ' + DBFieldName;
      if i < (FInputFields.Count - 1) then
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

procedure TSQLModelDef.SetInputFields(Value: TJSONArray);
begin
  FInputFields.Free;
  FInputFields := Value;
end;

procedure TSQLModelDef.SetJSONFields(Value: TJSONArray);
begin
  FJSONFields.Free;
  FJSONFields := Value;
end;

function TSQLModelDef.GetDisplayName: string;
begin
  Result := FName;
end;

constructor TSQLModelDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TParams.Create(TSQLModelDefParam);
  FCacheMode := cmNone;
end;

destructor TSQLModelDef.Destroy;
begin
  FInParams.Free;
  FJSONFields.Free;
  FInputFields.Free;
  FParams.Destroy;
  inherited Destroy;
end;

procedure TSQLModelDef.Assign(Source: TPersistent);
begin
  if Source is TSQLModelDef then
  begin
     FPrimaryKey := TSQLModelDef(Source).FPrimaryKey;
     FName := TSQLModelDef(Source).FName;
     Params := TSQLModelDef(Source).Params;
     FSelectSQL := TSQLModelDef(Source).FSelectSQL;
     JSONFields := TSQLModelDef(Source).FJSONFields;
     InputFields := TSQLModelDef(Source).FInputFields;
     FDataField := TSQLModelDef(Source).FDataField;
  end
  else
    inherited Assign(Source);
end;

{ TSQLResourceClient }

procedure TSQLResourceClient.BuildModelDefLookup;
var
  i: Integer;
  ModelDef: TSQLModelDef;
  TableNameExpr, InParamExpr: TRegExpr;
begin
  TableNameExpr := TRegExpr.Create;
  TableNameExpr.ModifierI := True;
  TableNameExpr.Expression := '\sFROM\s+(\w+)';
  InParamExpr := TRegExpr.Create;
  InParamExpr.ModifierI := True;
  InParamExpr.ModifierG := True;
  InParamExpr.Expression := '\s+IN\s*\(\s*:(\w+)\s*\)';
  for i := 0 to FModelDefs.Count - 1 do
  begin
    ModelDef := TSQLModelDef(FModelDefs.Items[i]);
    if (ModelDef.TableName = '') and TableNameExpr.Exec(ModelDef.SelectSQL) then
      ModelDef.TableName := TableNameExpr.Match[1];
    if ModelDef.FInParams <> nil then
      ModelDef.FInParams.Clear;
    if InParamExpr.Exec(ModelDef.SelectSQL) then
    repeat
      ModelDef.InParamsNeeded;
      ModelDef.FInParams.Add(InParamExpr.Match[1]);
    until not InParamExpr.ExecNext;
    if InParamExpr.Exec(ModelDef.ConditionsSQL) then
    repeat
      ModelDef.InParamsNeeded;
      ModelDef.FInParams.Add(InParamExpr.Match[1]);
    until not InParamExpr.ExecNext;
    FModelDefLookup.Add(ModelDef.Name, ModelDef);
  end;
  TableNameExpr.Destroy;
  InParamExpr.Destroy;
end;

procedure TSQLResourceClient.CacheHandlerNeeded;
begin
  if FCacheHandler = nil then
    FCacheHandler := TSQLCacheHandler.Create;
end;

function TSQLResourceClient.FindModelDef(const ModelName: String): TSQLModelDef;
begin
  if FModelDefLookup = nil then
  begin
    FModelDefLookup := TFPHashObjectList.Create(False);
    BuildModelDefLookup;
  end;
  Result := TSQLModelDef(FModelDefLookup.Find(ModelName));
end;

function TSQLResourceClient.GetModelDef(const ModelName: String): TSQLModelDef;
begin
  Result := FindModelDef(ModelName);
  if Result = nil then
    raise Exception.CreateFmt('Unable to find resource model "%s"', [ModelName]);
end;

function TSQLResourceClient.GetCacheData(const ModelName, ResourcePath: String;
  DataResource: TSQLDataResource): Boolean;
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

procedure TSQLResourceClient.SetModelDefs(AValue: TSQLResourceModelDefs);
begin
  FModelDefs.Assign(AValue);
end;

procedure TSQLResourceClient.UpdateCache(const ModelName, Path: String;
  Stream: TStream);
begin
  CacheHandlerNeeded;
  FCacheHandler.UpdateCache(ModelName, Path, Stream);
end;

procedure TSQLResourceClient.ModelDefsChanged;
begin
  FreeAndNil(FModelDefLookup);
end;

constructor TSQLResourceClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModelDefs := TSQLResourceModelDefs.Create(Self);
end;

destructor TSQLResourceClient.Destroy;
begin
  FCacheHandler.Free;
  FModelDefLookup.Free;
  FModelDefs.Destroy;
  inherited Destroy;
end;

function TSQLResourceClient.Connected: Boolean;
begin
  Result := True;
end;

function TSQLResourceClient.GetDataset(const ModelName: String): IDatasetResource;
var
  ModelDef: TSQLModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TSQLDatasetResource.Create(ModelDef, Self);
end;

function TSQLResourceClient.GetJSONArray(const ModelName: String): IJSONArrayResource;
var
  ModelDef: TSQLModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TSQLJSONArrayResource.Create(ModelDef, Self);
end;

function TSQLResourceClient.GetJSONObject(const ModelName: String): IJSONObjectResource;
var
  ModelDef: TSQLModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TSQLJSONObjectResource.Create(ModelDef, Self);
end;

procedure TSQLResourceClient.InvalidateCache(const ModelName: String);
begin
  CacheHandlerNeeded;
  FCacheHandler.Invalidate(ModelName);
end;

function TSQLResourceClient.HasModel(const ModelName: String): Boolean;
begin
  Result := FindModelDef(ModelName) <> nil;
end;

end.
