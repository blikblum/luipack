unit LuiRESTServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttp, contnrs, HTTPDefs, fpjson;

type
  TRESTResource = class;
  TRESTResourceDef = class;
  TRESTResourceStore = class;
  TRESTResourceClass = class of TRESTResource;

  TRESTResourceCreateEvent = procedure(out Resource: TRESTResource;
    ResourceTag: PtrInt) of object;
  TRESTResourceLoadEvent = procedure(Resource: TRESTResource;
    ResourceTag: PtrInt) of object;

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
    class var FResponseFormatter: TRESTResponseFormatterClass;
    procedure SubPathResourcesNeeded;
  protected
    procedure Loaded; virtual;
    procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; const Args: array of const);
  public
    destructor Destroy; override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleSubPath(const SubPath: String; var SubPathResourceDef: TRESTResourceDef); virtual;
    procedure RegisterSubPath(const ResourceId: ShortString; Resource: TRESTResource);
    procedure RegisterSubPath(const ResourceId: ShortString; ResourceClass: TRESTResourceClass; Tag: PtrInt);
    procedure RegisterSubPath(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
    procedure SetDefaultSubPath(const ParamName: String; Resource: TRESTResource);
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
    constructor Create(AResource: TRESTResource);
    constructor Create(AResourceClass: TRESTResourceClass; ATag: PtrInt);
    constructor Create(CreateCallback: TRESTResourceCreateEvent; ATag: PtrInt);
    destructor Destroy; override;
    function GetResource(URIParams: TJSONObject; OnResourceLoad: TRESTResourceLoadEvent): TRESTResource;
    property OnResourceCreate: TRESTResourceCreateEvent read FOnResourceCreate write FOnResourceCreate;
    property Resource: TRESTResource read FResource write FResource;
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
    procedure Register(const ResourceId: ShortString; Resource: TRESTResource);
    procedure Register(const ResourceId: ShortString; ResourceClass: TRESTResourceClass; Tag: PtrInt);
    procedure Register(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
  end;

  { TRESTServiceModule }

  TRESTServiceModule = class(TCustomHTTPModule)
  private
    FResources: TRESTResourceStore;
    FContentType: String;
    FOnResourceLoad: TRESTResourceLoadEvent;
    FResponseFormatter: TRESTResponseFormatterClass;
    FRootPath: String;
    procedure SetRootPath(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; const Args: array of const);
    property Resources: TRESTResourceStore read FResources;
    property ResponseFormatter: TRESTResponseFormatterClass read FResponseFormatter write FResponseFormatter;
  published
    property ContentType: String read FContentType write FContentType;
    property RootPath: String read FRootPath write SetRootPath;
    //events
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
  AResponse.Contents.Add(Format('{"message":"%s"}', [StringToJSONString(Format(Message, Args))]));
end;

{ TRESTServiceModule }

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
  FResources.Destroy;
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

procedure TRESTServiceModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  URIPath, URIPart, NextURIPart, MethodStr: String;
  i, PartOffset: Integer;
  ResourceDef: TRESTResourceDef;
  Resource: TRESTResource;
  URIParams: TJSONObject;
begin
  AResponse.ContentType := FContentType;
  TRESTResource.FResponseFormatter := FResponseFormatter;
  MethodStr := UpperCase(ARequest.Method);
  URIPath := ARequest.PathInfo;
  i := Pos(FRootPath, URIPath);
  if i <> 0 then
  begin
    URIParams := TJSONObject.Create;
    try
      PartOffset := i + Length(FRootPath);
      URIPart := GetURIPart(URIPath, PartOffset);
      //the first part is by convention a collection
      if URIPart = '' then
      begin
        SetResponseStatus(AResponse, 404, 'Resource path not found. PartOffset %d, URIPath: "%s"', [PartOffset, URIPath]);
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

      NextURIPart := GetURIPart(URIPath, PartOffset);
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

        NextURIPart := GetURIPart(URIPath, PartOffset);
      end;

      if MethodStr = 'GET' then
        Resource.HandleGet(ARequest, AResponse)
      else if MethodStr = 'PUT' then
        Resource.HandlePut(ARequest, AResponse)
      else if MethodStr = 'POST' then
      begin
        AResponse.Code := 201;
        Resource.HandlePost(ARequest, AResponse)
      end
      else if MethodStr = 'DELETE' then
        Resource.HandleDelete(ARequest, AResponse)
      else
        SetResponseStatus(AResponse, 501, 'Method "%s" not implemented', [MethodStr]);
    finally
      URIParams.Destroy;
    end;
  end
  else
    SetResponseStatus(AResponse, 404, 'Root path not found. URIPath: "%s" RootPath: "%s"', [URIPath, FRootPath]);
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

procedure TRESTResource.Loaded;
begin
  //
end;

procedure TRESTResource.SetResponseStatus(AResponse: TResponse; StatusCode: Integer;
  const Message: String; const Args: array of const);
begin
  FResponseFormatter.SetStatus(AResponse, StatusCode, Message, Args);
  //AResponse.Contents.Add(FResponseFormatter.ClassName);
end;

procedure TRESTResource.RegisterSubPath(const ResourceId: ShortString;
  Resource: TRESTResource);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, Resource);
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
  Resource: TRESTResource);
begin
  if Resource = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Instance']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(Resource);
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
      URIParams.Strings[FSubPathParamName] := SubPath;
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
  Resource: TRESTResource);
var
  Def: TRESTResourceDef;
begin
  if Resource = nil then
    raise Exception.CreateFmt(SRegisterError, [ResourceId, 'Instance']);
  Def := TRESTResourceDef.Create(Resource);
  FList.Add(ResourceId, Def);
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

constructor TRESTResourceDef.Create(AResource: TRESTResource);
begin
  inherited Create;
  FResource := AResource;
end;

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
      Result.Loaded;
    end;
  end;
end;

end.

