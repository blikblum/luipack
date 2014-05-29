unit LuiRESTServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttp, contnrs, HTTPDefs, fpjson;

type
  TRESTResource = class;
  TRESTResourceDef = class;
  TRESTResourceStore = class;
  TRESTServiceModule = class;
  TRESTResourceClass = class of TRESTResource;


  TRESTResourceCreateEvent = procedure(out Resource: TRESTResource;
    ResourceTag: PtrInt) of object;
  TRESTResourceLoadEvent = procedure(Resource: TRESTResource;
    ResourceTag: PtrInt) of object;
  TRESTRequestEvent = procedure(Sender: TObject; ARequest: TRequest;
    AResponse: TResponse) of object;

  { TRESTResponseFormatter }

  TRESTResponseFormatter = class
  public
    class procedure SetStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; const Args: array of const); virtual;
  end;

  TRESTResponseFormatterClass = class of TRESTResponseFormatter;

  { TJSONRESTResponseFormatter }

  TJSONRESTResponseFormatter = class(TRESTResponseFormatter)
  public
    class procedure SetStatus(AResponse: TResponse; StatusCode: Integer; const Message: String;
      const Args: array of const); override;
  end;

  { TRESTResource }
  {$M+}
  TRESTResource = class
  private
    FSubPathResources: TRESTResourceStore;
    FSubPathParamName: String;
    FURIParams: TJSONObject;
    class var FServiceModule: TRESTServiceModule;
    procedure SubPathResourcesNeeded;
  protected
    procedure Loaded(Tag: PtrInt); virtual;
    procedure RedirectRequest(ARequest: TRequest; AResponse: TResponse;
      const Method, ResourcePath: String; Relative: Boolean = True);
    procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; const Args: array of const);
    procedure SetURIParam(const ParamName, ParamValue: String; IsNumeric: Boolean = False);
  public
    destructor Destroy; override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleSubPath(const SubPath: String; var SubPathResourceDef: TRESTResourceDef); virtual;
    procedure RegisterSubPath(const ResourceId: ShortString; ResourceClass: TRESTResourceClass; Tag: PtrInt);
    procedure RegisterSubPath(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
    procedure SetDefaultSubPath(const ParamName: String; ResourceClass: TRESTResourceClass; Tag: PtrInt);
    procedure SetDefaultSubPath(const ParamName: String; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
    property URIParams: TJSONObject read FURIParams;
  end;
  {$M-}

  { TRESTResourceDef }

  TRESTResourceDef = class
  private
    FOnResourceCreate: TRESTResourceCreateEvent;
    FResource: TRESTResource;
    FResourceClass: TRESTResourceClass;
    FTag: PtrInt;
  public
    constructor Create(AResourceClass: TRESTResourceClass; ATag: PtrInt);
    constructor Create(CreateCallback: TRESTResourceCreateEvent; ATag: PtrInt);
    destructor Destroy; override;
    function GetResource(URIParams: TJSONObject; OnResourceLoad: TRESTResourceLoadEvent): TRESTResource;
    property OnResourceCreate: TRESTResourceCreateEvent read FOnResourceCreate write FOnResourceCreate;
    property ResourceClass: TRESTResourceClass read FResourceClass write FResourceClass;
    property Tag: PtrInt read FTag write FTag;
  end;

  { TRESTResourceStore }

  TRESTResourceStore = class
  private
    FDefaultResourceDef: TRESTResourceDef;
    FList: TFPHashObjectList;
    procedure SetDefaultResourceDef(const Value: TRESTResourceDef);
    property DefaultResourceDef: TRESTResourceDef read FDefaultResourceDef write SetDefaultResourceDef;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(const ResourceId: ShortString): TRESTResourceDef;
    procedure Register(const ResourceId: ShortString; ResourceClass: TRESTResourceClass; Tag: PtrInt);
    procedure Register(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
  end;

  { TRESTServiceModule }

  TRESTServiceModule = class(TCustomHTTPModule)
  private
    FOnRequest: TRESTRequestEvent;
    FResources: TRESTResourceStore;
    FContentType: String;
    FOnResourceLoad: TRESTResourceLoadEvent;
    FResponseFormatter: TRESTResponseFormatterClass;
    FRootPath: String;
    procedure DoRequest(ARequest: TRequest; AResponse: TResponse);
    procedure SetRootPath(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ResolveRequest(ARequest: TRequest; AResponse: TResponse;
      const Method, Path: String; PathOffset: Integer);
    procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; const Args: array of const);
    property Resources: TRESTResourceStore read FResources;
    property ResponseFormatter: TRESTResponseFormatterClass read FResponseFormatter write FResponseFormatter;
  published
    property ContentType: String read FContentType write FContentType;
    property RootPath: String read FRootPath write SetRootPath;
    //events
    property OnRequest: TRESTRequestEvent read FOnRequest write FOnRequest;
    property OnResourceLoad: TRESTResourceLoadEvent read FOnResourceLoad write FOnResourceLoad;
  end;

implementation

const
  SRegisterError = 'RegisterResource (%s): %s must be <> nil';
  SDefaultSubPathError = 'SetDefaultSubPath (%s): %s must be <> nil';

{ TRESTResponseFormatter }

class procedure TRESTResponseFormatter.SetStatus(AResponse: TResponse; StatusCode: Integer;
  const Message: String; const Args: array of const);
begin
  AResponse.Code := StatusCode;
  AResponse.Contents.Add(Format(Message, Args));
end;

{ TJSONRESTResponseFormatter }

class procedure TJSONRESTResponseFormatter.SetStatus(AResponse: TResponse; StatusCode: Integer;
  const Message: String; const Args: array of const);
begin
  AResponse.Code := StatusCode;
  //todo: return the effective Path (can be different from PathInfo)
  AResponse.Contents.Add(Format('{"message":"%s"}', [StringToJSONString(Format(Message, Args))]));
end;

{ TRESTServiceModule }

procedure TRESTServiceModule.DoRequest(ARequest: TRequest; AResponse: TResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self, ARequest, AResponse);
end;

procedure TRESTServiceModule.SetRootPath(const Value: String);
begin
  if Value <> '' then
  begin
    if Value[1] <> '/' then
      FRootPath := '/' + Value
    else
      FRootPath := Value;
    if Value[Length(Value)] <> '/' then
      FRootPath := FRootPath + '/';
  end
  else
    FRootPath := '/';
end;

constructor TRESTServiceModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResources := TRESTResourceStore.Create;
  FContentType := 'application/json; charset=UTF-8';
  FResponseFormatter := TJSONRESTResponseFormatter;
  FRootPath := '/';
end;

destructor TRESTServiceModule.Destroy;
begin
  FResources.Free;
  inherited Destroy;
end;

function GetURIPart(const S: String; var StrOffset: Integer): String;
var
  P : integer;
begin
  if StrOffset > Length(S) then
    Exit('');
  P := IndexChar(S[StrOffset], Length(S) - StrOffset + 1, '/');
  if P = -1 then
    P := Length(S) + 1;
  Result := Copy(S, StrOffset, P);
  Inc(StrOffset, P + 1);
end;

function GetJSONExceptionBackTrace: String;
var
  i: Integer;
  Frames: PPointer;
begin
  Result := '["' + StringToJSONString(BackTraceStrFunc(ExceptAddr)) + '"';
  Frames := ExceptFrames;
  for i := 0 to ExceptFrameCount - 1 do
  begin
    Result := Result + ',"' + StringToJSONString(BackTraceStrFunc(Frames[i]))+'"';
  end;
  Result := Result + ']';
end;

procedure TRESTServiceModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  URIPath: String;
  RootPathPos: Integer;
begin
  try
    DoRequest(ARequest, AResponse);
    AResponse.ContentType := FContentType;
    TRESTResource.FServiceModule := Self;
    URIPath := ARequest.PathInfo;
    RootPathPos := Pos(FRootPath, URIPath);
    if RootPathPos <> 0 then
      ResolveRequest(ARequest, AResponse, UpperCase(ARequest.Method), URIPath, RootPathPos + Length(FRootPath))
    else
      SetResponseStatus(AResponse, 404, 'Root path not found. URIPath: "%s" RootPath: "%s"', [URIPath, FRootPath]);
  except
    on E:Exception do
    begin
      AResponse.Code := 500;
      AResponse.Contents.Add(Format('{"message":"%s", "backtrace":%s}', [StringToJSONString(Format('%s: %s', [E.ClassName, E.Message])),
        GetJSONExceptionBackTrace]));
    end;
  end;
end;

procedure TRESTServiceModule.ResolveRequest(ARequest: TRequest; AResponse: TResponse;
  const Method, Path: String; PathOffset: Integer);
var
  URIPart, NextURIPart: String;
  PartOffset: Integer;
  ResourceDef: TRESTResourceDef;
  Resource: TRESTResource;
  URIParams: TJSONObject;
begin
  URIParams := TJSONObject.Create;
  try
    PartOffset := PathOffset;
    URIPart := GetURIPart(Path, PartOffset);
    if URIPart = '' then
    begin
      SetResponseStatus(AResponse, 404, 'Resource path not found. PartOffset %d, URIPath: "%s"', [PartOffset, Path]);
      Exit;
    end;
    ResourceDef := FResources.Find(URIPart);
    if ResourceDef = nil then
    begin
      SetResponseStatus(AResponse, 404, 'Resource "%s" not registered', [URIPart]);
      Exit;
    end;

    Resource := ResourceDef.GetResource(URIParams, OnResourceLoad);
    if Resource = nil then
    begin
      SetResponseStatus(AResponse, 404, 'Unable to load resource "%s"', [URIPart]);
      Exit;
    end;

    NextURIPart := GetURIPart(Path, PartOffset);
    while NextURIPart <> '' do
    begin
      ResourceDef := nil;
      Resource.HandleSubPath(NextURIPart, ResourceDef);

      if ResourceDef = nil then
      begin
        SetResponseStatus(AResponse, 404, 'Resource "%s" not registered. Resource: %s, SubPathRes: %s',
          [NextURIPart, Resource.ClassName,
          BoolToStr(Boolean(Resource.FSubPathResources = nil), True)]);
        Exit;
      end;

      Resource := ResourceDef.GetResource(URIParams, OnResourceLoad);
      if Resource = nil then
      begin
        SetResponseStatus(AResponse, 404, 'Unable to load resource "%s"', [URIPart]);
        Exit;
      end;

      NextURIPart := GetURIPart(Path, PartOffset);
    end;

    if Method = 'GET' then
      Resource.HandleGet(ARequest, AResponse)
    else if Method = 'PUT' then
      Resource.HandlePut(ARequest, AResponse)
    else if Method = 'POST' then
    begin
      AResponse.Code := 201;
      Resource.HandlePost(ARequest, AResponse)
    end
    else if Method = 'DELETE' then
      Resource.HandleDelete(ARequest, AResponse)
    else
      SetResponseStatus(AResponse, 501, 'Method "%s" not implemented', [Method]);
  finally
    URIParams.Destroy;
  end;
end;

procedure TRESTServiceModule.SetResponseStatus(AResponse: TResponse; StatusCode: Integer;
  const Message: String; const Args: array of const);
begin
  FResponseFormatter.SetStatus(AResponse, StatusCode, Message, Args);
end;

{ TRESTResource }

procedure TRESTResource.SubPathResourcesNeeded;
begin
  if FSubPathResources = nil then
    FSubPathResources := TRESTResourceStore.Create;
end;

procedure TRESTResource.Loaded(Tag: PtrInt);
begin
  //
end;

procedure TRESTResource.RedirectRequest(ARequest: TRequest; AResponse: TResponse;
  const Method, ResourcePath: String; Relative: Boolean = True);
var
  Offset: Integer;
begin
  //todo: add ability to retieve the original request path and method
  if Relative then
    Offset := 1
  else
    Offset := Pos(FServiceModule.RootPath, ResourcePath) + Length(FServiceModule.RootPath);
  FServiceModule.ResolveRequest(ARequest, AResponse, Method, ResourcePath, Offset);
end;

procedure TRESTResource.SetResponseStatus(AResponse: TResponse; StatusCode: Integer;
  const Message: String; const Args: array of const);
begin
  FServiceModule.FResponseFormatter.SetStatus(AResponse, StatusCode, Message, Args);
end;

procedure TRESTResource.SetURIParam(const ParamName, ParamValue: String;
  IsNumeric: Boolean);
var
  DoubleValue: Double;
  Int64Value: Int64;
begin
  //todo: implement constraints
  //todo: add ERestException (define message + status code)
  if TryStrToInt64(ParamValue, Int64Value) then
    URIParams.Int64s[ParamName] := Int64Value
  else if TryStrToFloat(ParamValue, DoubleValue) then
    URIParams.Floats[ParamName] := DoubleValue
  else
  begin
    if not IsNumeric then
      URIParams.Strings[ParamName] := ParamValue
    else
      raise Exception.CreateFmt('Invalid param. "%s" expects a numeric value. Got "%s"', [ParamName, ParamValue]);
  end;
end;

procedure TRESTResource.RegisterSubPath(const ResourceId: ShortString;
  ResourceClass: TRESTResourceClass; Tag: PtrInt);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, ResourceClass, Tag);
end;

procedure TRESTResource.RegisterSubPath(const ResourceId: ShortString;
  CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, CreateCallback, Tag);
end;

procedure TRESTResource.SetDefaultSubPath(const ParamName: String;
  ResourceClass: TRESTResourceClass; Tag: PtrInt);
begin
  if ResourceClass = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Class']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(ResourceClass, Tag);
end;

procedure TRESTResource.SetDefaultSubPath(const ParamName: String;
  CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
begin
  if CreateCallback = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Event']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(CreateCallback, Tag);
end;

destructor TRESTResource.Destroy;
begin
  FSubPathResources.Free;
  inherited Destroy;
end;

procedure TRESTResource.HandleSubPath(const SubPath: String; var SubPathResourceDef: TRESTResourceDef);
begin
  if FSubPathResources <> nil then
  begin
    SubPathResourceDef := FSubPathResources.Find(SubPath);
    if SubPathResourceDef = nil then
    begin
      SubPathResourceDef := FSubPathResources.DefaultResourceDef;
      SetURIParam(FSubPathParamName, SubPath);
    end;
  end;
end;

procedure TRESTResource.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['DELETE']);
end;

procedure TRESTResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['GET']);
end;

procedure TRESTResource.HandlePost(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['POST']);
end;

procedure TRESTResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['PUT']);
end;

{ TRESTResourceStore }


procedure TRESTResourceStore.SetDefaultResourceDef(const Value: TRESTResourceDef);
begin
  if Value = FDefaultResourceDef then
    Exit;
  FreeAndNil(FDefaultResourceDef);
  FDefaultResourceDef := Value;
end;

constructor TRESTResourceStore.Create;
begin
  FList := TFPHashObjectList.Create(True);
end;

destructor TRESTResourceStore.Destroy;
begin
  FList.Destroy;
  FDefaultResourceDef.Free;
  inherited Destroy;
end;

function TRESTResourceStore.Find(const ResourceId: ShortString): TRESTResourceDef;
begin
  Result := TRESTResourceDef(FList.Find(ResourceId));
end;

procedure TRESTResourceStore.Register(const ResourceId: ShortString;
  ResourceClass: TRESTResourceClass; Tag: PtrInt);
var
  Def: TRESTResourceDef;
begin
  if ResourceClass = nil then
    raise Exception.CreateFmt(SRegisterError, [ResourceId, 'Class']);
  Def := TRESTResourceDef.Create(ResourceClass, Tag);
  FList.Add(ResourceId, Def);
end;

procedure TRESTResourceStore.Register(const ResourceId: ShortString;
  CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
var
  Def: TRESTResourceDef;
begin
  if CreateCallback = nil then
    raise Exception.CreateFmt(SRegisterError, [ResourceId, 'Event']);
  Def := TRESTResourceDef.Create(CreateCallback, Tag);
  FList.Add(ResourceId, Def);
end;

{ TRESTResourceDef }

constructor TRESTResourceDef.Create(AResourceClass: TRESTResourceClass;
  ATag: PtrInt);
begin
  inherited Create;
  FResourceClass := AResourceClass;
  FTag := ATag;
end;

constructor TRESTResourceDef.Create(CreateCallback: TRESTResourceCreateEvent;
  ATag: PtrInt);
begin
  inherited Create;
  FOnResourceCreate := CreateCallback;
  FTag := ATag;
end;

destructor TRESTResourceDef.Destroy;
begin
  FResource.Free;
  inherited Destroy;
end;

function TRESTResourceDef.GetResource(URIParams: TJSONObject; OnResourceLoad: TRESTResourceLoadEvent): TRESTResource;
var
  DoLoad: Boolean;
begin
  DoLoad := FResource = nil;
  if DoLoad then
  begin
    if Assigned(FOnResourceCreate) then
      FOnResourceCreate(FResource, FTag);
    if (FResource = nil) and Assigned(FResourceClass) then
      FResource := FResourceClass.Create;
  end;
  Result := FResource;
  if Result <> nil then
  begin
    Result.FURIParams := URIParams;
    if DoLoad then
    begin
      if Assigned(OnResourceLoad) then
        OnResourceLoad(Result, FTag);
      Result.Loaded(FTag);
    end;
  end;
end;

end.

