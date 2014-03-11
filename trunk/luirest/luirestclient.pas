unit LuiRESTClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, httpsend, contnrs, fpjson, db;

type

  TRESTResourceClient = class;

  THTTPMethodType = (hmtGet, hmtPost, hmtPut, hmtDelete);

  TRESTCacheMode = (cmNone, cmSession, cmLocal);

  TRESTErrorType = (reService, reResponse, reRequest, reSocket);

  TRESTResponseEvent = procedure(const ResourcePath: String; ResourceTag: PtrInt; Method: THTTPMethodType;
    ResponseCode: Integer; ResponseStream: TStream; var ValidData: Boolean) of object;

  TSocketError = procedure(Sender: TObject; ErrorCode: Integer; const ErrorMessage: String) of object;

  TRESTErrorEvent = procedure(Sender: TObject; const ResourcePath: String; ErrorType: TRESTErrorType;
    ErrorCode: Integer; const ErrorMessage: String; var Handled: Boolean) of object;

  { TRESTClient }


  TRESTClient = class(TComponent)
  private
    FBaseURL: String;
    FHttpClient: THTTPSend;
    FOnResponseError: TRESTResponseEvent;
    FOnResponseSuccess: TRESTResponseEvent;
    FOnSocketError: TSocketError;
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
  published
    property BaseURL: String read FBaseURL write SetBaseURL;
    property Http: THTTPSend read GetHttp;
    property OnResponseSuccess: TRESTResponseEvent read FOnResponseSuccess write FOnResponseSuccess;
    property OnResponseError: TRESTResponseEvent read FOnResponseError write FOnResponseError;
    property OnSocketError: TSocketError read FOnSocketError write FOnSocketError;
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
  end;

  { TRESTDataResource }

  TRESTDataResource = class(TInterfacedObject)
  private
    FModelDef: TRESTResourceModelDef;
    FResourceClient: TRESTResourceClient;
    FParams: TParams;
  protected
    function GetResourcePath: String;
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType; ResponseStream: TStream): Boolean; virtual; abstract;
    property ModelDef: TRESTResourceModelDef read FModelDef;
  public
    constructor Create(AModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient); virtual;
    destructor Destroy; override;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    property Params: TParams read GetParams;
  end;

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
    FModelDefs: TRESTResourceModelDefs;
    FModelDefLookup: TFPHashObjectList;
    FOnError: TRESTErrorEvent;
    FRESTClient: TRESTClient;
    procedure BuildModelDefLookup;
    procedure CacheHandlerNeeded;
    function FindModelDef(const ModelName: String): TRESTResourceModelDef;
    function GetBaseURL: String;
    function GetCacheData(const ModelName, ResourcePath: String;
      DataResource: TRESTDataResource): Boolean;
    function GetHttp: THTTPSend;
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
    function Post(const ResourcePath: String; Resource: TRESTDataResource; const Data: String): Boolean;
    function Put(const ResourcePath: String; Resource: TRESTDataResource; const Data: String): Boolean;
    procedure ModelDefsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetJSONArray(const ModelName: String): IJSONArrayResource;
    function GetJSONObject(const ModelName: String): IJSONObjectResource;
    procedure InvalidateCache(const ModelName: String);
  published
    property BaseURL: String read GetBaseURL write SetBaseURL;
    property Http: THTTPSend read GetHttp;
    property ModelDefs: TRESTResourceModelDefs read FModelDefs write SetModelDefs;
    property OnError: TRESTErrorEvent read FOnError write FOnError;
  end;


implementation

uses
  LuiJSONUtils, variants, StrUtils;

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
    function Save: Boolean;
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
    function DoSave(const Id: String): Boolean;
  protected
    function ParseResponse(const ResourcePath: String; Method: THTTPMethodType; ResponseStream: TStream): Boolean; override;
  public
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
  Result := FResourceClient.Get(GetResourcePath + IfThen(Id <> '', '/' + Id), Self);
end;

function TRESTJSONObjectResource.DoSave(const Id: String): Boolean;
var
  ResourcePath, IdField: String;
begin
  IdField := FModelDef.IdField;
  ResourcePath := GetResourcePath;
  if (Id = '') and (IdField <> '') then
  begin
    Result := FResourceClient.Post(ResourcePath, Self, FData.AsJSON);
  end
  else
  begin
    ResourcePath := ResourcePath + IfThen(Id <> '', '/' + Id);
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
  ResourcePath := GetResourcePath;
  if VarIsEmpty(FIdValue) or VarIsNull(FIdValue) then
  begin
    if FData = nil then
    begin
      FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Data not set');
      Exit;
    end;
    IdFieldData := FData.Find(FModelDef.IdField);
    if IdFieldData <> nil then
    begin
      if (IdFieldData.JSONType in [jtString, jtNumber]) then
        Id := IdFieldData.AsString
      else
      begin
        FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field must be string or number');
        Exit;
      end
    end
    else
    begin
      FResourceClient.DoError(ResourcePath, reRequest, 0, 'Delete: Id field not set');
      Exit;
    end;
  end
  else
    Id := VarToStr(FIdValue);
  ResourcePath := ResourcePath + '/' + Id;
  Result := FResourceClient.Delete(ResourcePath, Self);
end;

function TRESTJSONObjectResource.Fetch: Boolean;
var
  IdFieldData: TJSONData;
  IdField, Id: String;
begin
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
  FIdValue := Unassigned;
  Result := DoFetch(Id);
end;

function TRESTJSONObjectResource.Fetch(IdValue: Variant): Boolean;
begin
  FIdValue := IdValue;
  Result := DoFetch(VarToStr(IdValue));
end;

function TRESTJSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

function TRESTJSONObjectResource.Save: Boolean;
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
  Result := DoSave(Id);
end;

function TRESTJSONObjectResource.Save(IdValue: Variant): Boolean;
begin
  FIdValue := IdValue;
  Result := DoSave(VarToStr(IdValue));
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

{ TRESTDataResource }

constructor TRESTDataResource.Create(AModelDef: TRESTResourceModelDef;
  ResourceClient: TRESTResourceClient);
begin
  FParams := TParams.Create(TParam);
  FParams.Assign(AModelDef.Params);
  FModelDef := AModelDef;
  FResourceClient := ResourceClient;
end;

destructor TRESTDataResource.Destroy;
begin
  FParams.Destroy;
  inherited Destroy;
end;

function TRESTDataResource.GetResourcePath: String;
var
  Param: TParam;
  i: Integer;
begin
  Result := FModelDef.Path;
  for i := 0 to FParams.Count - 1 do
  begin
    Param := FParams.Items[i];
    Result := StringReplace(Result, '{' + Param.Name + '}', Param.AsString,
      [rfReplaceAll, rfIgnoreCase]);
  end;
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

function TRESTJSONArrayResource.Save: Boolean;
begin
  //
end;

destructor TRESTJSONArrayResource.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TRESTJSONArrayResource.Fetch: Boolean;
var
  ResourcePath: String;
begin
  ResourcePath := GetResourcePath;
  Result := FResourceClient.Get(ResourcePath, Self);
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
  FParams := TParams.Create(TParam);
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
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('DELETE', BaseURL + ResourcePath);
  if Result then
  begin
    Result := DoResponseCallback(ResourcePath, ResourceTag, hmtDelete, FHttpClient.ResultCode, FHttpClient.Document) and
      (FHttpClient.ResultCode < 300);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TRESTClient.Get(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
begin
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('GET', BaseURL + ResourcePath);
  if Result then
  begin
    Result := DoResponseCallback(ResourcePath, ResourceTag, hmtGet, FHttpClient.ResultCode, FHttpClient.Document) and
      (FHttpClient.ResultCode < 300);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TRESTClient.Post(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(Data[1], Length(Data));
  Result := FHttpClient.HTTPMethod('POST', BaseURL + ResourcePath);
  if Result then
  begin
    Result := DoResponseCallback(ResourcePath, ResourceTag, hmtPost, FHttpClient.ResultCode, FHttpClient.Document) and
      (FHttpClient.ResultCode < 300);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TRESTClient.Put(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(Data[1], Length(Data));
  Result := FHttpClient.HTTPMethod('PUT', BaseURL + ResourcePath);
  if Result then
  begin
    Result := DoResponseCallback(ResourcePath, ResourceTag, hmtPut, FHttpClient.ResultCode, FHttpClient.Document) and
      (FHttpClient.ResultCode < 300);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

{ TRESTResourceClient }

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
  FRESTClient.OnResponseError := @ResponseError;
  FRESTClient.OnResponseSuccess := @ResponseSuccess;
  FRESTClient.OnSocketError := @SocketError;
end;

destructor TRESTResourceClient.Destroy;
begin
  FCacheHandler.Free;
  FModelDefLookup.Free;
  FModelDefs.Destroy;
  inherited Destroy;
end;

function TRESTResourceClient.GetJSONArray(const ModelName: String): IJSONArrayResource;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := FindModelDef(ModelName);
  Result := TRESTJSONArrayResource.Create(ModelDef, Self);
end;

function TRESTResourceClient.GetJSONObject(const ModelName: String): IJSONObjectResource;
var
  ModelDef: TRESTResourceModelDef;
begin
  ModelDef := FindModelDef(ModelName);
  Result := TRESTJSONObjectResource.Create(ModelDef, Self);
end;

procedure TRESTResourceClient.InvalidateCache(const ModelName: String);
begin
  CacheHandlerNeeded;
  FCacheHandler.Invalidate(ModelName);
end;

end.

