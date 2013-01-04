unit LuiRESTClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, httpsend, contnrs, fpjson, db;

type

  TRESTResourceClient = class;

  THTTPMethodType = (hmtGet, hmtPost, hmtPut, hmtDelete);

  TRESTErrorType = (reService, reResponse, reRequest, reSocket);

  TRESTResponseEvent = procedure(ResourceTag: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream) of object;

  TSocketError = procedure(Sender: TObject; ErrorCode: Integer; const ErrorMessage: String) of object;

  TRESTErrorEvent = procedure(Sender: TObject; ErrorType: TRESTErrorType;
    ErrorCode: Integer; const ErrorMessage: String; var Handled: Boolean) of object;

  { TRESTClient }


  TRESTClient = class(TComponent)
  private
    FBaseURL: String;
    FHttpClient: THTTPSend;
    FOnResponseError: TRESTResponseEvent;
    FOnResponseSuccess: TRESTResponseEvent;
    FOnSocketError: TSocketError;
    procedure DoResponseCallback(ResourcePath: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream);
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
    property OnResponseSuccess: TRESTResponseEvent read FOnResponseSuccess write FOnResponseSuccess;
    property OnResponseError: TRESTResponseEvent read FOnResponseError write FOnResponseError;
    property OnSocketError: TSocketError read FOnSocketError write FOnSocketError;
  end;

  { TRESTResourceModelDef }

  TRESTResourceModelDef = class(TCollectionItem)
  private
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

  { TRESTResourceClient }

  TRESTResourceClient = class(TComponent)
  private
    FModelDefs: TRESTResourceModelDefs;
    FModelDefLookup: TFPHashObjectList;
    FOnError: TRESTErrorEvent;
    FRESTClient: TRESTClient;
    procedure BuildModelDefLookup;
    function FindModelDef(const ModelName: String): TRESTResourceModelDef;
    function GetBaseURL: String;
    procedure ResponseError(ResourceTag: PtrInt; Method: THTTPMethodType;
      ResponseCode: Integer; ResponseStream: TStream);
    procedure ResponseSuccess(ResourceTag: PtrInt; Method: THTTPMethodType;
      ResponseCode: Integer; ResponseStream: TStream);
    procedure SetBaseURL(const AValue: String);
    procedure SetModelDefs(AValue: TRESTResourceModelDefs);
    procedure SocketError(Sender: TObject; ErrorCode: Integer;
      const ErrorMessage: String);
  protected
    procedure DoError(ErrorType: TRESTErrorType; ErrorCode: Integer; const ErrorMessage: String);
    procedure ModelDefsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetJSONArray(const ModelName: String): IJSONArrayResource;
    function GetJSONObject(const ModelName: String): IJSONObjectResource;
  published
    property BaseURL: String read GetBaseURL write SetBaseURL;
    property ModelDefs: TRESTResourceModelDefs read FModelDefs write SetModelDefs;
    property OnError: TRESTErrorEvent read FOnError write FOnError;
  end;


implementation

uses
  LuiJSONUtils;

type

  { TRESTDataResource }

  TRESTDataResource = class(TInterfacedObject)
  private
    FModelDef: TRESTResourceModelDef;
    FResourceClient: TRESTResourceClient;
    FParams: TParams;
  protected
    function GetResourcePath: String;
    procedure ParseResponse(Method: THTTPMethodType; ResponseStream: TStream); virtual; abstract;
  public
    constructor Create(ModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient); virtual;
    destructor Destroy; override;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    property Params: TParams read GetParams;
  end;

  { TRESTJSONArrayResource }

  TRESTJSONArrayResource = class(TRESTDataResource, IJSONArrayResource)
  private
    //todo: implement save through dirty checking
    //FSnapshot/FReference: TJSONArray;
    FData: TJSONArray;
  protected
    procedure ParseResponse(Method: THTTPMethodType; ResponseStream: TStream); override;
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
    FOwnsData: Boolean;
  protected
    procedure ParseResponse(Method: THTTPMethodType; ResponseStream: TStream); override;
  public
    destructor Destroy; override;
    function Delete: Boolean;
    function Fetch: Boolean;
    function GetData: TJSONObject;
    function Save: Boolean;
    procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
    property Data: TJSONObject read GetData;
  end;

{ TRESTJSONObjectResource }

procedure TRESTJSONObjectResource.ParseResponse(Method: THTTPMethodType; ResponseStream: TStream);
var
  ResponseData: TJSONData;
begin
  ResponseData := StreamToJSONData(ResponseStream);
  case Method of
    hmtGet, hmtPost, hmtPut:
      begin
        if ResponseData = nil then
        begin
          FResourceClient.DoError(reResponse, 0, Format('%s: No response data', [FModelDef.Name]));
          Exit;
        end;
        if ResponseData.JSONType <> jtObject then
        begin
          FResourceClient.DoError(reResponse, 0, Format('%s: Invalid response format. Expected jtObject got %s',
            [FModelDef.Name, JSONTypeName(ResponseData.JSONType)]));
          Exit;
        end;
        if FData = nil then
          FData := TJSONObject(ResponseData)
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
  IdData: TJSONData;
begin
  if FData = nil then
  begin
    Result := False;
    Exit;
  end;
  IdData := FData.Find(FModelDef.IdField);
  if (IdData = nil) or not (IdData.JSONType in [jtString, jtNumber]) then
  begin
    //todo error handling
    Result := False;
    Exit;
  end;
  ResourcePath := GetResourcePath + '/' + IdData.AsString;
  Result := FResourceClient.FRESTClient.Delete(ResourcePath, PtrInt(Self));
end;

function TRESTJSONObjectResource.Fetch: Boolean;
var
  ResourcePath: String;
  IdData: TJSONData;
begin
  if (FData = nil) then
  begin
    Result := False;
    Exit;
  end;
  IdData := FData.Find(FModelDef.IdField);
  if (IdData = nil) or not (IdData.JSONType in [jtString, jtNumber]) then
  begin
    //todo error handling
    Result := False;
    Exit;
  end;
  ResourcePath := GetResourcePath + '/' + IdData.AsString;
  Result := FResourceClient.FRESTClient.Get(ResourcePath, PtrInt(Self));
end;

function TRESTJSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

function TRESTJSONObjectResource.Save: Boolean;
var
  ResourcePath: String;
  IdData: TJSONData;
begin
  if FData = nil then
  begin
    Result := True;
    Exit;
  end;
  ResourcePath := GetResourcePath;
  IdData := FData.Find(FModelDef.IdField);
  if IdData = nil then
  begin
    Result := FResourceClient.FRESTClient.Post(ResourcePath, PtrInt(Self), FData.AsJSON);
  end
  else
  begin
    if not(IdData.JSONType in [jtString, jtNumber]) then
    begin
      Result := False;
      Exit;
    end;
    ResourcePath := ResourcePath + '/' + IdData.AsString;
    Result := FResourceClient.FRESTClient.Put(ResourcePath, PtrInt(Self), FData.AsJSON);
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

{ TRESTDataResource }

constructor TRESTDataResource.Create(ModelDef: TRESTResourceModelDef; ResourceClient: TRESTResourceClient);
begin
  FParams := TParams.Create(TParam);
  FParams.Assign(ModelDef.Params);
  FModelDef := ModelDef;
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

procedure TRESTJSONArrayResource.ParseResponse(Method: THTTPMethodType; ResponseStream: TStream);
var
  ResponseData: TJSONData;
begin
  ResponseData := StreamToJSONData(ResponseStream);
  case Method of
    hmtGet:
      begin
        if ResponseData = nil then
        begin
          FResourceClient.DoError(reResponse, 0, Format('%s: No response data', [FModelDef.Name]));
          Exit;
        end;
        if ResponseData.JSONType <> jtArray then
        begin
          FResourceClient.DoError(reResponse, 0, Format('%s: Invalid response format. Expected jtArray got %s',
            [FModelDef.Name, JSONTypeName(ResponseData.JSONType)]));
          Exit;
        end;
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
  Result := FResourceClient.FRESTClient.Get(ResourcePath, PtrInt(Self));
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

procedure TRESTClient.DoResponseCallback(ResourcePath: PtrInt; Method: THTTPMethodType;
   ResponseCode: Integer; ResponseStream: TStream);
begin
  if ResponseCode < 300 then
  begin
    if Assigned(FOnResponseSuccess) then
      FOnResponseSuccess(ResourcePath, Method, ResponseCode, ResponseStream);
  end
  else
  begin
    if Assigned(FOnResponseError) then
      FOnResponseError(ResourcePath, Method, ResponseCode, ResponseStream);
  end;
end;

procedure TRESTClient.SetBaseURL(const AValue: String);
begin
  if AValue[Length(AValue)] <> '/' then
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
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtDelete, FHttpClient.ResultCode, FHttpClient.Document);
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
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtGet, FHttpClient.ResultCode, FHttpClient.Document);
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
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtPost, FHttpClient.ResultCode, FHttpClient.Document);
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
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtPut, FHttpClient.ResultCode, FHttpClient.Document);
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

function TRESTResourceClient.FindModelDef(const ModelName: String): TRESTResourceModelDef;
var
  i: Integer;
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

procedure TRESTResourceClient.ResponseError(ResourceTag: PtrInt; Method: THTTPMethodType;
  ResponseCode: Integer; ResponseStream: TStream);
var
  ResponseData: TJSONData;
  Message: String;
begin
  Message := '';
  ResponseData := nil;
  try
    ResponseData := StreamToJSONData(ResponseStream);
  except
    //
  end;
  if (ResponseData <> nil) and (ResponseData.JSONType = jtObject) then
    Message := TJSONObject(ResponseData).Get('message', '');
  if Message = '' then
    Message := 'Unknow error message';
  DoError(reService, ResponseCode, Message);
end;

procedure TRESTResourceClient.ResponseSuccess(ResourceTag: PtrInt; Method: THTTPMethodType;
  ResponseCode: Integer; ResponseStream: TStream);
var
  DataResource: TRESTDataResource absolute ResourceTag;
begin
  DataResource.ParseResponse(Method, ResponseStream);
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
  DoError(reSocket, ErrorCode, ErrorMessage);
end;

procedure TRESTResourceClient.DoError(ErrorType: TRESTErrorType; ErrorCode: Integer;
  const ErrorMessage: String);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnError) then
    FOnError(Self, ErrorType, ErrorCode, ErrorMessage, Handled);
  if not Handled then
    raise Exception.Create(ErrorMessage);
end;

procedure TRESTResourceClient.ModelDefsChanged;
begin
  FreeAndNil(FModelDefLookup);
end;

constructor TRESTResourceClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModelDefs := TRESTResourceModelDefs.Create(Self);
  FModelDefLookup := TFPHashObjectList.Create(True);
  FRESTClient := TRESTClient.Create(Self);
  FRESTClient.OnResponseError := @ResponseError;
  FRESTClient.OnResponseSuccess := @ResponseSuccess;
  FRESTClient.OnSocketError := @SocketError;
end;

destructor TRESTResourceClient.Destroy;
begin
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

end.

