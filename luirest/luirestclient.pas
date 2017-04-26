unit LuiRESTClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, httpsend, contnrs, fpjson, db;

type

  TRESTResourceClient = class;

  THTTPMethodType = (hmtGet, hmtPost, hmtPut, hmtPatch, hmtDelete);

  TRESTCacheMode = (cmNone, cmSession, cmLocal);

  TRESTErrorType = (reService, reResponse, reRequest, reSocket);

  TRESTResponseEvent = procedure(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
    ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean) of object;

  TRESTRequestEvent = procedure(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
    const Payload: String) of object;

  TSocketError = procedure(Sender: TObject; ErrorCode: Integer; const ErrorMessage: String) of object;

  TRESTErrorEvent = procedure(Sender: TObject; const ResourcePath: String; ErrorType: TRESTErrorType;
    ErrorCode: Integer; const ErrorMessage: String; var Handled: Boolean) of object;

  { TRESTClient }


  TRESTClient = class(TComponent)
  private
    FBaseURL: String;
    FHttpClient: THTTPSend;
    FOnRequest: TRESTRequestEvent;
    FOnResponseError: TRESTResponseEvent;
    FOnResponseSuccess: TRESTResponseEvent;
    FOnSocketError: TSocketError;
    function DoRequest(const ResourcePath: String; ResourceTag: PtrInt; const Payload: String;
      MethodType: THTTPMethodType): Boolean;
    function DoResponseCallback(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
      ResponseCode: Integer; ResponseStream: TStream): Boolean;
    function GetHttp: THTTPSend;
    procedure SetBaseURL(const AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
    function Get(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
    function Post(const ResourcePath: String; ResourceTag: PtrInt; const Data: String): Boolean;
    function Put(const ResourcePath: String; ResourceTag: PtrInt; const Data: String): Boolean;
    function Patch(const ResourcePath: String; ResourceTag: PtrInt; const Data: String): Boolean;
  published
    property BaseURL: String read FBaseURL write SetBaseURL;
    property Http: THTTPSend read GetHttp;
    property OnRequest: TRESTRequestEvent read FOnRequest write FOnRequest;
    property OnResponseSuccess: TRESTResponseEvent read FOnResponseSuccess write FOnResponseSuccess;
    property OnResponseError: TRESTResponseEvent read FOnResponseError write FOnResponseError;
    property OnSocketError: TSocketError read FOnSocketError write FOnSocketError;
  end;

  TParamLocation = (plPath, plQuery);

  { TModelDefParam }

  TModelDefParam = class(TParam)
  private
    FLocation: TParamLocation;
    FRequired: Boolean;
    function GetRequired: Boolean;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Location: TParamLocation read FLocation write FLocation default plPath;
    property Required: Boolean read GetRequired write FRequired;
  end;

  { TRESTResourceModelDef }

  TRESTResourceModelDef = class(TCollectionItem)
  private
    FCacheMode: TRESTCacheMode;
    FIdField: String;
    FName: String;
    FParams: TParams;
    FPath: String;
    procedure SetParams(AValue: TParams);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CacheMode: TRESTCacheMode read FCacheMode write FCacheMode default cmNone;
    property IdField: String read FIdField write FIdField;
    property Name: String read FName write FName;
    property Params: TParams read FParams write SetParams;
    property Path: String read FPath write FPath;
  end;

  { TRESTResourceModelDefs }

  TRESTResourceModelDefs = class(TCollection)
  private
    FOwner: TRESTResourceClient;
  protected
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TRESTResourceClient);
    procedure LoadFromFile(const FileName: String);
  end;

  { TRESTDataResource }

  TRESTDataResource = class(TInterfacedObject)
  private
    FModelDef: TRESTResourceModelDef;
    FResourceClient: TRESTResourceClient;
    FParams: TParams;
  protected
    function GetResourcePath(const ResourceId: String = ''): String;
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType; ResponseStream: TStream): Boolean; virtual; abstract;
    property ModelDef: TRESTResourceModelDef read FModelDef;
  public
    constructor Create(AModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient); virtual;
    destructor Destroy; override;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    property Params: TParams read GetParams;
  end;

  TRESTResourceRequestEvent = procedure(const ResourcePath: String; Resource: TRESTDataResource; Method: THTTPMethodType;
    const Payload: String) of object;

  //todo: abstract cache handler so it can be plugged another implementation

  { TRESTCacheHandler }

  TRESTCacheHandler = class
  private
    FModelCacheList: TFPHashObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCacheData(const ModelName, Path: String): TStream;
    procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
    procedure Invalidate(const ModelName: String);
  end;

  { TRESTResourceClient }

  TRESTResourceClient = class(TComponent, IResourceClient)
  private
    FCacheHandler: TRESTCacheHandler;
    FDefaultHeaders: TStrings;
    FModelDefs: TRESTResourceModelDefs;
    FModelDefLookup: TFPHashObjectList;
    FOnError: TRESTErrorEvent;
    FOnRequest: TRESTResourceRequestEvent;
    FRESTClient: TRESTClient;
    procedure BuildModelDefLookup;
    procedure CacheHandlerNeeded;
    function FindModelDef(const ModelName: String): TRESTResourceModelDef;
    function GetModelDef(const ModelName: String): TRESTResourceModelDef;
    function GetBaseURL: String;
    function GetCacheData(const ModelName, ResourcePath: String;
      DataResource: TRESTDataResource): Boolean;
    function GetDefaultHeaders: TStrings;
    function GetHttp: THTTPSend;
    procedure Request(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
      const Payload: String);
    procedure ResponseError(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
      ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean);
    procedure ResponseSuccess(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
      ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean);
    procedure SetBaseURL(const AValue: String);
    procedure SetModelDefs(AValue: TRESTResourceModelDefs);
    procedure SocketError(Sender: TObject; ErrorCode: Integer;
      const ErrorMessage: String);
    procedure UpdateCache(const ModelName, Path: String; Stream: TStream);
  protected
    procedure DoError(const ResourcePath: String; ErrorType: TRESTErrorType; ErrorCode: Integer; const ErrorMessage: String);
    function Delete(const ResourcePath: String; Resource: TRESTDataResource): Boolean;
    function Get(const ResourcePath: String; Resource: TRESTDataResource): Boolean;
    function Patch(const ResourcePath: String; Resource: TRESTDataResource; const Data: String): Boolean;
    function Post(const ResourcePath: String; Resource: TRESTDataResource; const Data: String): Boolean;
    function Put(const ResourcePath: String; Resource: TRESTDataResource; const Data: String): Boolean;
    procedure ModelDefsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connected: Boolean;
    function GetDataset(const ModelName: String): IDatasetResource;
    function GetJSONArray(const ModelName: String): IJSONArrayResource;
    function GetJSONObject(const ModelName: String): IJSONObjectResource;
    procedure InvalidateCache(const ModelName: String);
    function HasModel(const ModelName: String): Boolean;
  published
    property BaseURL: String read GetBaseURL write SetBaseURL;
    property DefaultHeaders: TStrings read GetDefaultHeaders;
    property Http: THTTPSend read GetHttp;
    property ModelDefs: TRESTResourceModelDefs read FModelDefs write SetModelDefs;
    property OnError: TRESTErrorEvent read FOnError write FOnError;
    property OnRequest: TRESTResourceRequestEvent read FOnRequest write FOnRequest;
  end;

const
  HTTPMethodNames: Array[THTTPMethodType] of String = (
    'GET',
    'POST',
    'PUT',
    'PATCH',
    'DELETE'
    );


implementation

uses
  LuiJSONUtils, variants, StrUtils, BufDataset, fpjsonrtti;

type
  { TRESTJSONArrayResource }

  TRESTJSONArrayResource = class(TRESTDataResource, IJSONArrayResource)
  private
    //todo: implement save through dirty checking
    //FSnapshot/FReference: TJSONArray;
    FData: TJSONArray;
  protected
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType; ResponseStream: TStream): Boolean; override;
  public
    destructor Destroy; override;
    function Fetch: Boolean;
    function GetData: TJSONArray;
    function Save(Options: TSaveOptions = []): Boolean;
    property Data: TJSONArray read GetData;
  end;

  { TRESTJSONObjectResource }

  TRESTJSONObjectResource = class(TRESTDataResource, IJSONObjectResource)
  private
    //todo: implement save through dirty checking
    //FSnapshot/FReference: TJSONObject;
    FData: TJSONObject;
    FIdValue: Variant;
    FOwnsData: Boolean;
    function DoFetch(const Id: String): Boolean;
    function DoSave(const Id: String; Options: TSaveOptions = []): Boolean;
  protected
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType; ResponseStream: TStream): Boolean; override;
  public
    constructor Create(AModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient); override;
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

  { TRESTDataset }

  TRESTDataset = class(TCustomBufDataset)
  private
    FResource: TRESTDataResource;
  protected
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
  public
    constructor Create(AOwner: TComponent; AResource: TRESTDataResource);
  end;

  { TRESTDatasetResource }

  TRESTDatasetResource = class(TRESTDataResource, IDatasetResource)
  private
    class var FFieldDefsDataCache: TFPHashObjectList;
    //the extra private keyword is necessary to avoid further fields being considered as class var
  private
    FIdValue: Variant;
    FDataset: TRESTDataset;
    function DoFetch(const IdValue: Variant): Boolean;
    procedure FieldDefsDataCacheNeeded;
    function GetFieldDefsData: TJSONArray;
    procedure LoadFieldDefs;
  protected
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType;
      ResponseStream: TStream): Boolean; override;
  public
    constructor Create(AModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient);
      override;
    destructor Destroy; override;
    function Fetch: Boolean;
    function Fetch(IdValue: Variant): Boolean;
    function GetDataset: TDataSet;
    function Save(Options: TSaveOptions = []): Boolean;
  end;

procedure TRESTDataset.ApplyRecUpdate(UpdateKind: TUpdateKind);
var
  IdField: TField;
  ResourcePath: String;
  RecordData: TJSONObject;
  HasId: Boolean;
begin
  IdField := FindField(FResource.ModelDef.IdField);
  HasId := (IdField <> nil) and not IdField.IsNull;
  if HasId then
    ResourcePath := FResource.GetResourcePath(IdField.AsString)
  else
    ResourcePath := FResource.GetResourcePath;

  case UpdateKind of
    ukInsert:
    begin
      //todo: handle setting record id after insert
      RecordData := DatasetToJSON(Self, [djoCurrentRecord], '') as TJSONObject;
      if HasId then
        FResource.FResourceClient.Put(ResourcePath, FResource, RecordData.AsJSON)
      else
        FResource.FResourceClient.Post(ResourcePath, FResource, RecordData.AsJSON);
      RecordData.Destroy;
    end;
    ukModify:
    begin
      if HasId then
      begin
        RecordData := DatasetToJSON(Self, [djoCurrentRecord], '') as TJSONObject;
        FResource.FResourceClient.Put(ResourcePath, FResource, RecordData.AsJSON);
        RecordData.Destroy;
      end;
    end;
    ukDelete:
    begin
      if HasId then
        FResource.FResourceClient.Delete(ResourcePath, FResource);
    end;
  end;
end;

constructor TRESTDataset.Create(AOwner: TComponent; AResource: TRESTDataResource);
begin
  inherited Create(AOwner);
  FResource := AResource;
end;

{ TRESTDatasetResource }

function TRESTDatasetResource.DoFetch(const IdValue: Variant): Boolean;
begin
  Result := FResourceClient.Get(GetResourcePath(VarToStr(IdValue)), Self);
end;

procedure TRESTDatasetResource.FieldDefsDataCacheNeeded;
begin
  if FFieldDefsDataCache = nil then
    FFieldDefsDataCache := TFPHashObjectList.Create(True);
end;

function TRESTDatasetResource.GetFieldDefsData: TJSONArray;
var
  ResourcePath: String;
  Http: THTTPSend;
begin
  Result := TJSONArray(FFieldDefsDataCache.Find(FModelDef.Name));
  if Result = nil then
  begin
    ResourcePath := GetResourcePath('fielddefs');
    Http := THTTPSend.Create;
    if Http.HTTPMethod('GET', FResourceClient.BaseURL + ResourcePath) then
    begin
      if TryStreamToJSON(Http.Document, Result) then
        FFieldDefsDataCache.Add(FModelDef.Name, Result);
    end;
    Http.Destroy;
  end;
  if Result = nil then
    raise Exception.CreateFmt('"%s": Unable to resolve fielddefs', [FModelDef.Name]);
end;

procedure TRESTDatasetResource.LoadFieldDefs;
var
  FieldDefsData: TJSONArray;
  DeStreamer: TJSONDeStreamer;
begin
  FieldDefsData := GetFieldDefsData;
  if FieldDefsData = nil then
    Exit;
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToCollection(FieldDefsData, FDataset.FieldDefs);
  finally
    DeStreamer.Destroy;
  end;
end;

procedure AddRecord(Dataset: TDataset; Data: TJSONObject);
var
  Field: TField;
  FieldName: String;
  i: Integer;
  PropData: TJSONData;
begin
  if Data.Count = 0 then
    Exit;
  Dataset.Append;
  try
    for i := 0 to Data.Count - 1 do
    begin
      FieldName := Data.Names[i];
      Field := Dataset.FindField(FieldName);
      if Field <> nil then
      begin
        PropData := Data.Items[i];
        case PropData.JSONType of
          jtString, jtBoolean, jtNumber:
            Field.Value := PropData.Value;
          jtArray, jtObject:
            Field.AsString := PropData.AsJSON;
        end;
      end;
    end;
    Dataset.Post;
  except
    Dataset.Cancel;
  end;
end;

function TRESTDatasetResource.ParseResponse(const ResourcePath: String; Method: THTTPMethodType;
  ResponseStream: TStream): Boolean;
var
  ResponseData, ItemData: TJSONData;
  i: Integer;
begin
  Result := (Method = hmtDelete) or TryStreamToJSON(ResponseStream, ResponseData);
  //todo: merge the common code from the three resource classes
  if not Result then
  begin
    FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: Invalid response format. Unable to parse as JSON', [FModelDef.Name]));
    Exit;
  end;
  case Method of
    hmtGet:
    begin
      FDataset.Close;
      if FDataset.FieldDefs.Count = 0 then
      begin
        LoadFieldDefs;
        FDataset.CreateDataset;
      end;
      FDataset.Open;
      //todo: DisableControls / BlockRead
      case ResponseData.JSONType of
        jtObject:
          AddRecord(FDataset, TJSONObject(ResponseData));
        jtArray:
          begin
            for i := 0 to ResponseData.Count -1 do
            begin
              ItemData := ResponseData.Items[i];
              if ItemData.JSONType = jtObject then
                AddRecord(FDataset, TJSONObject(ItemData));
            end;
            FDataset.First;
          end;
      end;
      FDataset.MergeChangeLog;
    end;
    hmtPost, hmtPut, hmtDelete, hmtPatch: ;
  end;
  if Method <> hmtDelete then
    ResponseData.Destroy;
end;

constructor TRESTDatasetResource.Create(AModelDef: TRESTResourceModelDef;
  ResourceClient: TRESTResourceClient);
begin
  inherited Create(AModelDef, ResourceClient);
  FDataset := TRESTDataset.Create(nil, Self);
  FieldDefsDataCacheNeeded;
end;

destructor TRESTDatasetResource.Destroy;
begin
  FDataset.Destroy;
  inherited Destroy;
end;

function TRESTDatasetResource.Fetch: Boolean;
begin
  FIdValue := Unassigned;
  Result := DoFetch(Null);
end;

function TRESTDatasetResource.Fetch(IdValue: Variant): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    FIdValue := IdValue;
    Result := DoFetch(IdValue);
  end
  else
    Result := Fetch();
end;

function TRESTDatasetResource.GetDataset: TDataSet;
begin
  Result := FDataset;
end;

function TRESTDatasetResource.Save(Options: TSaveOptions): Boolean;
begin
  Result := False;
  FDataset.ApplyUpdates;
  Result := True;
end;

{ TModelDefParam }

function TModelDefParam.GetRequired: Boolean;
begin
  if FLocation = plPath then
    Result := True
  else
    Result := FRequired;
end;

constructor TModelDefParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLocation := plPath;
  ParamType := ptInput;
  DataType := ftInteger;
end;

procedure TModelDefParam.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TModelDefParam then
  begin
    FRequired := TModelDefParam(Source).FRequired;
    FLocation := TModelDefParam(Source).Location;
  end;
end;

{ TRESTCacheHandler }

constructor TRESTCacheHandler.Create;
begin
  FModelCacheList := TFPHashObjectList.Create(True);
end;

destructor TRESTCacheHandler.Destroy;
begin
  FModelCacheList.Destroy;
  inherited Destroy;
end;

function TRESTCacheHandler.GetCacheData(const ModelName, Path: String): TStream;
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

procedure TRESTCacheHandler.UpdateCache(const ModelName, Path: String;
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

procedure TRESTCacheHandler.Invalidate(const ModelName: String);
var
  ModelCache: TObject;
begin
  ModelCache := FModelCacheList.Find(ModelName);
  if ModelCache <> nil then
    FModelCacheList.Remove(ModelCache);
end;

{ TRESTJSONObjectResource }

function TRESTJSONObjectResource.DoFetch(const Id: String): Boolean;
begin
  Result := FResourceClient.Get(GetResourcePath(Id), Self);
end;

function TRESTJSONObjectResource.DoSave(const Id: String; Options: TSaveOptions): Boolean;
var
  ResourcePath, IdField: String;
begin
  IdField := FModelDef.IdField;
  if (Id = '') and (IdField <> '') then
  begin
    ResourcePath := GetResourcePath;
    Result := FResourceClient.Post(ResourcePath, Self, FData.AsJSON);
  end
  else
  begin
    ResourcePath := GetResourcePath(Id);
    if soPatch in Options then
      Result := FResourceClient.Patch(ResourcePath, Self, FData.AsJSON)
    else
      Result := FResourceClient.Put(ResourcePath, Self, FData.AsJSON);
  end;
end;

function TRESTJSONObjectResource.ParseResponse(const ResourcePath: String; Method: THTTPMethodType;
  ResponseStream: TStream): Boolean;
var
  ResponseData: TJSONData;
begin
  Result := True;
  try
    ResponseData := StreamToJSON(ResponseStream);
  except
    FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: Invalid response format. Unable to parse as JSON', [FModelDef.Name]));
    Exit;
  end;
  case Method of
    hmtGet, hmtPost, hmtPut:
      begin
        if ResponseData = nil then
        begin
          Result := False;
          FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: No response data', [FModelDef.Name]));
          Exit;
        end;
        if ResponseData.JSONType <> jtObject then
        begin
          Result := False;
          FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: Invalid response format. Expected jtObject got %s',
            [FModelDef.Name, JSONTypeName(ResponseData.JSONType)]));
          ResponseData.Destroy;
          Exit;
        end;
        if FData = nil then
        begin
          FData := TJSONObject(ResponseData);
          FOwnsData := True;
        end
        else
        begin
          FData.Clear;
          CopyJSONObject(TJSONObject(ResponseData), FData);
          ResponseData.Destroy;
        end;
      end;
    hmtDelete:
      ;
  end;
end;

constructor TRESTJSONObjectResource.Create(AModelDef: TRESTResourceModelDef;
  ResourceClient: TRESTResourceClient);
begin
  inherited Create(AModelDef, ResourceClient);
  FData := TJSONObject.Create;
  FOwnsData := True;
end;

destructor TRESTJSONObjectResource.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

function TRESTJSONObjectResource.Delete: Boolean;
var
  ResourcePath: String;
  IdFieldData: TJSONData;
  Id: String;
begin
  Result := False;
  if VarIsEmpty(FIdValue) or VarIsNull(FIdValue) then
  begin
    if FData = nil then
    begin
      FResourceClient.DoError(GetResourcePath, reRequest, 0, 'Delete: Data not set');
      Exit;
    end;
    IdFieldData := FData.Find(FModelDef.IdField);
    if IdFieldData <> nil then
    begin
      if (IdFieldData.JSONType in [jtString, jtNumber]) then
        Id := IdFieldData.AsString
      else
      begin
        FResourceClient.DoError(GetResourcePath, reRequest, 0, 'Delete: Id field must be string or number');
        Exit;
      end
    end
    else
    begin
      FResourceClient.DoError(GetResourcePath, reRequest, 0, 'Delete: Id field not set');
      Exit;
    end;
  end
  else
    Id := VarToStr(FIdValue);
  ResourcePath := GetResourcePath(Id);
  Result := FResourceClient.Delete(ResourcePath, Self);
end;

function TRESTJSONObjectResource.Delete(IdValue: Variant): Boolean;
var
  ResourcePath: String;
begin
  if not VarIsEmpty(IdValue) then
  begin
    ResourcePath := GetResourcePath(VarToStr(IdValue));
    Result := FResourceClient.Delete(ResourcePath, Self);
  end
  else
  begin
    //calling Delete without parentesis does not work
    //the compiler thinks is refering to the function result
    Result := Delete();
  end;
end;

function TRESTJSONObjectResource.Fetch: Boolean;
var
  IdFieldData: TJSONData;
  IdField, Id: String;
begin
  FIdValue := Unassigned;
  IdField := FModelDef.IdField;
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
  Result := DoFetch(Id);
end;

function TRESTJSONObjectResource.Fetch(IdValue: Variant): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    FIdValue := IdValue;
    Result := DoFetch(VarToStr(IdValue));
  end
  else
    Result := Fetch();
end;

function TRESTJSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

function TRESTJSONObjectResource.Save(Options: TSaveOptions): Boolean;
var
  IdFieldData: TJSONData;
  Id: String;
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
      IdFieldData := FData.Find(FModelDef.IdField);
      if (IdFieldData <> nil) then
      begin
        if (IdFieldData.JSONType in [jtString, jtNumber]) then
          Id := IdFieldData.AsString
        else
        begin
          FResourceClient.DoError(GetResourcePath, reRequest, 0, 'Save: Id field must be string or number');
          Exit;
        end
      end
      else
        Id := '';
    end;
  end
  else
    Id := VarToStr(FIdValue);
  Result := DoSave(Id, Options);
end;

function TRESTJSONObjectResource.Save(IdValue: Variant; Options: TSaveOptions): Boolean;
begin
  if not VarIsEmpty(IdValue) then
  begin
    Result := DoSave(VarToStr(IdValue), Options);
    if Result then
      FIdValue := IdValue;
  end
  else
  begin
    //calling Save without parentesis does not work
    //the compiler thinks is refering to the function result
    Result := Save(Options);
  end;
end;

procedure TRESTJSONObjectResource.SetData(JSONObj: TJSONObject; OwnsData: Boolean);
begin
  if FOwnsData then
    FData.Free;
  FData := JSONObj;
  FOwnsData := OwnsData;
end;

{ TRESTResourceModelDefs }

function TRESTResourceModelDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TRESTResourceModelDefs.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action = cnAdded then
    TRESTResourceModelDef(Item).FIdField := 'id';
  if not (csDestroying in FOwner.ComponentState) then
    FOwner.ModelDefsChanged;
end;

constructor TRESTResourceModelDefs.Create(AOwner: TRESTResourceClient);
begin
  inherited Create(TRESTResourceModelDef);
  FOwner := AOwner;
end;

procedure TRESTResourceModelDefs.LoadFromFile(const FileName: String);
var
  JSONDestreamer: TJSONDeStreamer;
  ModelDefsData: TJSONArray;
begin
  ModelDefsData := nil;
  JSONDestreamer := TJSONDeStreamer.Create(nil);
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

{ TRESTDataResource }

constructor TRESTDataResource.Create(AModelDef: TRESTResourceModelDef;
  ResourceClient: TRESTResourceClient);
begin
  FParams := TParams.Create(TModelDefParam);
  FParams.Assign(AModelDef.Params);
  FModelDef := AModelDef;
  FResourceClient := ResourceClient;
end;

destructor TRESTDataResource.Destroy;
begin
  FParams.Destroy;
  inherited Destroy;
end;

function GetParamStr(Param: TParam): String;
begin
  case Param.DataType of
    ftDate: Result := IntToStr(Param.AsLargeInt);
    ftDateTime, ftTime: Result := FloatToStr(Param.AsFloat);
  else
    Result := Param.AsString;
  end;
end;

function TRESTDataResource.GetResourcePath(const ResourceId: String): String;
var
  Param: TModelDefParam;
  QueryStr: String;
  IsNull: Boolean;
  i: Integer;
begin
  Result := FModelDef.Path;
  QueryStr := '';
  for i := 0 to FParams.Count - 1 do
  begin
    Param := TModelDefParam(FParams.Items[i]);
    IsNull := Param.IsNull;
    if Param.Required and IsNull then
      raise Exception.CreateFmt('Param "%s" of "%s" model not assigned', [Param.Name, FModelDef.Name]);
    if Param.Location = plPath then
    begin
      Result := StringReplace(Result, '{' + Param.Name + '}', GetParamStr(Param),
        [rfReplaceAll, rfIgnoreCase]);
    end
    else
    begin
      if not IsNull then
      begin
        //todo: encode query fields
        if QueryStr = '' then
          QueryStr := Param.Name + '=' + GetParamStr(Param)
        else
          QueryStr := QueryStr + '&' + Param.Name + '=' + GetParamStr(Param);
      end;
    end;
  end;
  if ResourceId <> '' then
    Result := Result + '/' + ResourceId;
  if QueryStr <> '' then
    Result := Result + '?' + QueryStr;
end;

function TRESTDataResource.GetParams: TParams;
begin
  Result := FParams;
end;

function TRESTDataResource.ParamByName(const ParamName: String): TParam;
begin
  Result := FParams.ParamByName(ParamName);
end;

{ TRESTJSONArrayResource }

function TRESTJSONArrayResource.GetData: TJSONArray;
begin
  Result := FData;
end;

function TRESTJSONArrayResource.ParseResponse(const ResourcePath: String; Method: THTTPMethodType;
  ResponseStream: TStream): Boolean;
var
  ResponseData: TJSONData;
begin
  Result := True;
  try
    ResponseData := StreamToJSON(ResponseStream);
  except
    FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: Invalid response format. Unable to parse as JSON', [FModelDef.Name]));
    Exit;
  end;
  case Method of
    hmtGet:
      begin
        if ResponseData = nil then
        begin
          Result := False;
          FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: No response data', [FModelDef.Name]));
          Exit;
        end;
        if ResponseData.JSONType <> jtArray then
        begin
          Result := False;
          FResourceClient.DoError(ResourcePath, reResponse, 0, Format('%s: Invalid response format. Expected jtArray got %s',
            [FModelDef.Name, JSONTypeName(ResponseData.JSONType)]));
          Exit;
        end;
        //todo: keep the same data instance (extract items)
        FData.Free;
        FData := TJSONArray(ResponseData);
      end;
    hmtPost:
      ;
    hmtPut:
      ;
    hmtDelete:
      ;
  end;
end;

function TRESTJSONArrayResource.Save(Options: TSaveOptions): Boolean;
begin
  Result := True;
end;

destructor TRESTJSONArrayResource.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TRESTJSONArrayResource.Fetch: Boolean;
begin
  Result := FResourceClient.Get(GetResourcePath, Self);
end;

{ TRESTResourceModelDef }

procedure TRESTResourceModelDef.SetParams(AValue: TParams);
begin
  FParams.Assign(AValue);
end;

function TRESTResourceModelDef.GetDisplayName: string;
begin
  Result := FName;
end;

constructor TRESTResourceModelDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TParams.Create(TModelDefParam);
  FCacheMode := cmNone;
end;

destructor TRESTResourceModelDef.Destroy;
begin
  FParams.Destroy;
  inherited Destroy;
end;

procedure TRESTResourceModelDef.Assign(Source: TPersistent);
begin
  if Source is TRESTResourceModelDef then
  begin
     FIdField := TRESTResourceModelDef(Source).FIdField;
     FName := TRESTResourceModelDef(Source).FName;
     Params := TRESTResourceModelDef(Source).Params;
     FPath := TRESTResourceModelDef(Source).FPath;
  end
  else
    inherited Assign(Source);
end;

{ TRESTClient }

function TRESTClient.DoRequest(const ResourcePath: String; ResourceTag: PtrInt;
  const Payload: String; MethodType: THTTPMethodType): Boolean;
var
  PayloadLength: Integer;
begin
  FHttpClient.Clear;
  if Assigned(FOnRequest) then
    FOnRequest(ResourcePath, ResourceTag, MethodType, Payload);
  PayloadLength := Length(Payload);
  if PayloadLength > 0 then
    FHttpClient.Document.Write(Payload[1], PayloadLength);
  Result := FHttpClient.HTTPMethod(HTTPMethodNames[MethodType], BaseURL + ResourcePath);
  if Result then
  begin
    Result := DoResponseCallback(ResourcePath, ResourceTag, MethodType, FHttpClient.ResultCode, FHttpClient.Document) and
      (FHttpClient.ResultCode < 300);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TRESTClient.DoResponseCallback(const ResourcePath: String; ResourceTag: PtrInt;
  Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream): Boolean;
begin
  Result := True;
  if ResponseCode < 300 then
  begin
    if Assigned(FOnResponseSuccess) then
      FOnResponseSuccess(ResourcePath, ResourceTag, Method, ResponseCode, ResponseStream, Result);
  end
  else
  begin
    if Assigned(FOnResponseError) then
      FOnResponseError(ResourcePath, ResourceTag, Method, ResponseCode, ResponseStream, Result);
  end;
end;

function TRESTClient.GetHttp: THTTPSend;
begin
  Result := FHttpClient;
end;

procedure TRESTClient.SetBaseURL(const AValue: String);
begin
  if (AValue = '') or (AValue[Length(AValue)] <> '/') then
    FBaseURL := AValue + '/'
  else
    FBaseURL := AValue;
end;

constructor TRESTClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpClient := THTTPSend.Create;
end;

destructor TRESTClient.Destroy;
begin
  FHttpClient.Destroy;
  inherited Destroy;
end;

function TRESTClient.Delete(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
begin
  Result := DoRequest(ResourcePath, ResourceTag, '', hmtDelete);
end;

function TRESTClient.Get(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
begin
  Result := DoRequest(ResourcePath, ResourceTag, '', hmtGet);
end;

function TRESTClient.Post(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  Result := DoRequest(ResourcePath, ResourceTag, Data, hmtPost);
end;

function TRESTClient.Put(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  Result := DoRequest(ResourcePath, ResourceTag, Data, hmtPut);
end;

function TRESTClient.Patch(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  Result := DoRequest(ResourcePath, ResourceTag, Data, hmtPatch);
end;

{ TRESTResourceClient }

procedure TRESTResourceClient.Request(const ResourcePath: String;
  ResourceTag: PtrInt; Method: THTTPMethodType; const Payload: String);
begin
  if FDefaultHeaders <> nil then
    FRESTClient.Http.Headers.AddStrings(FDefaultHeaders);
  if Assigned(FOnRequest) then
    FOnRequest(ResourcePath, TRESTDataResource(ResourceTag), Method, Payload);
end;

procedure TRESTResourceClient.BuildModelDefLookup;
var
  i: Integer;
  ModelDef: TRESTResourceModelDef;
begin
  for i := 0 to FModelDefs.Count - 1 do
  begin
    ModelDef := TRESTResourceModelDef(FModelDefs.Items[i]);
    FModelDefLookup.Add(ModelDef.Name, ModelDef);
  end;
end;

procedure TRESTResourceClient.CacheHandlerNeeded;
begin
  if FCacheHandler = nil then
    FCacheHandler := TRESTCacheHandler.Create;
end;

function TRESTResourceClient.FindModelDef(const ModelName: String): TRESTResourceModelDef;
begin
  if FModelDefLookup = nil then
  begin
    FModelDefLookup := TFPHashObjectList.Create(False);
    BuildModelDefLookup;
  end;
  Result := TRESTResourceModelDef(FModelDefLookup.Find(ModelName));
end;

function TRESTResourceClient.GetModelDef(const ModelName: String): TRESTResourceModelDef;
begin
  Result := FindModelDef(ModelName);
  if Result = nil then
    raise Exception.CreateFmt('Unable to find resource model "%s"', [ModelName]);
end;

function TRESTResourceClient.GetBaseURL: String;
begin
  Result := FRESTClient.BaseURL;
end;

function TRESTResourceClient.GetCacheData(const ModelName, ResourcePath: String;
  DataResource: TRESTDataResource): Boolean;
var
  CacheData: TStream;
begin
  CacheHandlerNeeded;
  CacheData := FCacheHandler.GetCacheData(ModelName, ResourcePath);
  Result := CacheData <> nil;
  if Result then
  begin
    CacheData.Position := 0;
    Result := DataResource.ParseResponse(ResourcePath, hmtGet, CacheData);
  end;
end;

function TRESTResourceClient.GetDefaultHeaders: TStrings;
begin
  if FDefaultHeaders = nil then
    FDefaultHeaders := TStringList.Create;
  Result := FDefaultHeaders;
end;

function TRESTResourceClient.GetHttp: THTTPSend;
begin
  Result := FRESTClient.FHttpClient;
end;

procedure TRESTResourceClient.ResponseError(const ResourcePath: String; ResourceTag: PtrInt;
  Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean);
var
  ResponseData: TJSONObject;
  Message: String;
begin
  if TryStreamToJSON(ResponseStream, ResponseData) then
  begin
    Message := ResponseData.Get('message', '');
  end
  else
  begin
    //todo: check if message is a valid string
    SetLength(Message, ResponseStream.Size);
    ResponseStream.Position := 0;
    ResponseStream.Write(Message[1], ResponseStream.Size);
  end;
  DoError(ResourcePath, reService, ResponseCode, Message);
end;

procedure TRESTResourceClient.ResponseSuccess(const ResourcePath: String; ResourceTag: PtrInt;
  Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean);
var
  DataResource: TRESTDataResource absolute ResourceTag;
begin
  ValidData := DataResource.ParseResponse(ResourcePath, Method, ResponseStream);
end;

procedure TRESTResourceClient.SetBaseURL(const AValue: String);
begin
  FRESTClient.BaseURL := AValue;
end;

procedure TRESTResourceClient.SetModelDefs(AValue: TRESTResourceModelDefs);
begin
  FModelDefs.Assign(AValue);
end;

procedure TRESTResourceClient.SocketError(Sender: TObject; ErrorCode: Integer;
  const ErrorMessage: String);
begin
  DoError('', reSocket, ErrorCode, ErrorMessage);
end;

procedure TRESTResourceClient.UpdateCache(const ModelName, Path: String;
  Stream: TStream);
begin
  CacheHandlerNeeded;
  FCacheHandler.UpdateCache(ModelName, Path, Stream);
end;

procedure TRESTResourceClient.DoError(const ResourcePath: String; ErrorType: TRESTErrorType;
  ErrorCode: Integer; const ErrorMessage: String);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnError) then
    FOnError(Self, ResourcePath, ErrorType, ErrorCode, ErrorMessage, Handled);
  if not Handled then
    raise Exception.Create(ErrorMessage);
end;

function TRESTResourceClient.Delete(const ResourcePath: String; Resource: TRESTDataResource): Boolean;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := Resource.ModelDef;
  if ModelDef.CacheMode <> cmNone then
    InvalidateCache(ModelDef.Name);
  Result := FRESTClient.Delete(ResourcePath, PtrInt(Resource));
end;

function TRESTResourceClient.Get(const ResourcePath: String;
  Resource: TRESTDataResource): Boolean;
var
  ModelDef: TRESTResourceModelDef;
  CanCache: Boolean;
begin
  ModelDef := Resource.ModelDef;
  CanCache := ModelDef.CacheMode <> cmNone;
  if CanCache then
  begin
    Result := GetCacheData(ModelDef.Name, ResourcePath, Resource);
    if Result then
      Exit;
  end;
  Result := FRESTClient.Get(ResourcePath, PtrInt(Resource));
  if Result and CanCache then
  begin
    //todo: add local mode
    UpdateCache(ModelDef.Name, ResourcePath, FRESTClient.FHttpClient.Document);
  end;
end;

function TRESTResourceClient.Patch(const ResourcePath: String;
  Resource: TRESTDataResource; const Data: String): Boolean;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := Resource.ModelDef;
  if ModelDef.CacheMode <> cmNone then
    InvalidateCache(ModelDef.Name);
  Result := FRESTClient.Patch(ResourcePath, PtrInt(Resource), Data);
end;

function TRESTResourceClient.Post(const ResourcePath: String; Resource: TRESTDataResource;
  const Data: String): Boolean;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := Resource.ModelDef;
  if ModelDef.CacheMode <> cmNone then
    InvalidateCache(ModelDef.Name);
  Result := FRESTClient.Post(ResourcePath, PtrInt(Resource), Data);
end;

function TRESTResourceClient.Put(const ResourcePath: String; Resource: TRESTDataResource;
  const Data: String): Boolean;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := Resource.ModelDef;
  if ModelDef.CacheMode <> cmNone then
    InvalidateCache(ModelDef.Name);
  Result := FRESTClient.Put(ResourcePath, PtrInt(Resource), Data);
end;

procedure TRESTResourceClient.ModelDefsChanged;
begin
  FreeAndNil(FModelDefLookup);
end;

constructor TRESTResourceClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModelDefs := TRESTResourceModelDefs.Create(Self);
  FRESTClient := TRESTClient.Create(Self);
  FRESTClient.OnRequest := @Request;
  FRESTClient.OnResponseError := @ResponseError;
  FRESTClient.OnResponseSuccess := @ResponseSuccess;
  FRESTClient.OnSocketError := @SocketError;
end;

destructor TRESTResourceClient.Destroy;
begin
  FCacheHandler.Free;
  FDefaultHeaders.Free;
  FModelDefLookup.Free;
  FModelDefs.Destroy;
  inherited Destroy;
end;

function TRESTResourceClient.Connected: Boolean;
begin
  //todo
  Result := True;
end;

function TRESTResourceClient.GetDataset(const ModelName: String): IDatasetResource;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TRESTDatasetResource.Create(ModelDef, Self);
end;

function TRESTResourceClient.GetJSONArray(const ModelName: String): IJSONArrayResource;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TRESTJSONArrayResource.Create(ModelDef, Self);
end;

function TRESTResourceClient.GetJSONObject(const ModelName: String): IJSONObjectResource;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := GetModelDef(ModelName);
  Result := TRESTJSONObjectResource.Create(ModelDef, Self);
end;

procedure TRESTResourceClient.InvalidateCache(const ModelName: String);
begin
  CacheHandlerNeeded;
  FCacheHandler.Invalidate(ModelName);
end;

function TRESTResourceClient.HasModel(const ModelName: String): Boolean;
begin
  Result := FindModelDef(ModelName) <> nil;
end;

finalization
  TRESTDatasetResource.FFieldDefsDataCache.Free;

end.

