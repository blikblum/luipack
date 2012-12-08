unit LuiREST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttp, contnrs, HTTPDefs, fpjson;

type
  TCustomRESTResource = class;
  TRESTResourceDef = class;
  TRESTResourceStore = class;
  TCustomRESTResourceClass = class of TCustomRESTResource;

  TRESTResourceCreateEvent = procedure(out Resource: TCustomRESTResource;
    ResourceTag: PtrInt) of object;
  TRESTResourceLoadEvent = procedure(Resource: TCustomRESTResource;
    ResourceTag: PtrInt) of object;

  { TCustomRESTResource }

  TCustomRESTResource = class(TPersistent)
  private
    FSubPathResources: TRESTResourceStore;
    FSubPathParamName: String;
    FURIParams: TJSONObject;
    procedure SubPathResourcesNeeded;
  protected
    procedure Loaded; virtual;
    procedure RegisterSubPath(const ResourceId: ShortString; Resource: TCustomRESTResource);
    procedure RegisterSubPath(const ResourceId: ShortString; ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
    procedure RegisterSubPath(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
    procedure SetDefaultSubPath(const ParamName: String; Resource: TCustomRESTResource);
    procedure SetDefaultSubPath(const ParamName: String; ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
    procedure SetDefaultSubPath(const ParamName: String; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
  public
    destructor Destroy; override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure HandleSubPath(const SubPath: String; var SubPathResourceDef: TRESTResourceDef); virtual;
    property URIParams: TJSONObject read FURIParams;
  end;

  { TRESTResourceDef }

  TRESTResourceDef = class
  private
    FOnResourceCreate: TRESTResourceCreateEvent;
    FResource: TCustomRESTResource;
    FResourceClass: TCustomRESTResourceClass;
    FTag: PtrInt;
  public
    constructor Create(AResource: TCustomRESTResource);
    constructor Create(AResourceClass: TCustomRESTResourceClass; ATag: PtrInt);
    constructor Create(CreateCallback: TRESTResourceCreateEvent; ATag: PtrInt);
    destructor Destroy; override;
    function GetResource(URIParams: TJSONObject; OnResourceLoad: TRESTResourceLoadEvent): TCustomRESTResource;
    property OnResourceCreate: TRESTResourceCreateEvent read FOnResourceCreate write FOnResourceCreate;
    property Resource: TCustomRESTResource read FResource write FResource;
    property ResourceClass: TCustomRESTResourceClass read FResourceClass write FResourceClass;
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
    function Get(const ResourceId: ShortString): TCustomRESTResource;
    procedure Register(const ResourceId: ShortString; Resource: TCustomRESTResource);
    procedure Register(const ResourceId: ShortString; ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
    procedure Register(const ResourceId: ShortString; CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
  end;

  { TRESTServiceModule }

  TRESTServiceModule = class(TCustomHTTPModule)
  private
    FResources: TRESTResourceStore;
    FContentType: String;
    FOnCreateResource: TRESTResourceLoadEvent;
    FRootPath: String;
    procedure SetRootPath(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    property Resources: TRESTResourceStore read FResources;
  published
    property ContentType: String read FContentType write FContentType;
    property RootPath: String read FRootPath write SetRootPath;
    //events
    property OnCreateResource: TRESTResourceLoadEvent read FOnCreateResource write FOnCreateResource;
  end;

  procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; Args: array of const);

implementation

const
  SRegisterError = 'RegisterResource (%s): %s must be <> nil';
  SDefaultSubPathError = 'SetDefaultSubPath (%s): %s must be <> nil';

{ TRESTServiceModule }

procedure SetResponseStatus(AResponse: TResponse;
  StatusCode: Integer; const Message: String; Args: array of const);
begin
  AResponse.Code := StatusCode;
  AResponse.Contents.Text := Format(Message, Args);
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
  FRootPath := '/';
end;

destructor TRESTServiceModule.Destroy;
begin
  FResources.Destroy;
  inherited Destroy;
end;

procedure TRESTServiceModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);


  function GetURIPart(const S : String; var StrOffset: Integer) : String;
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

var
  URIPath, URIPart, NextURIPart, MethodStr: String;
  i, PartOffset: Integer;
  ResourceDef, NextResourceDef: TRESTResourceDef;
  Resource: TCustomRESTResource;
  URIParams: TJSONObject;
begin
  AResponse.ContentType := FContentType;
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

      Resource := ResourceDef.GetResource(URIParams, OnCreateResource);
      if Resource = nil then
      begin
        SetResponseStatus(AResponse, 404, 'Unable to load resource "%s"', [URIPart]);
        Exit;
      end;

      NextURIPart := GetURIPart(URIPath, PartOffset);
      while NextURIPart <> '' do
      begin
        NextResourceDef := nil;
        Resource.HandleSubPath(NextURIPart, NextResourceDef);

        if NextResourceDef = nil then
        begin
          SetResponseStatus(AResponse, 404, 'Resource "%s" not registered. Resource: %s, SubPathRes: %s',
            [NextURIPart, Resource.ClassName,
            BoolToStr(Boolean(Resource.FSubPathResources = nil), True)]);
          Exit;
        end;

        ResourceDef := NextResourceDef;
        Resource := ResourceDef.GetResource(URIParams, OnCreateResource);
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

{ TCustomRESTResource }

procedure TCustomRESTResource.SubPathResourcesNeeded;
begin
  if FSubPathResources = nil then
    FSubPathResources := TRESTResourceStore.Create;
end;

procedure TCustomRESTResource.Loaded;
begin
  //
end;

procedure TCustomRESTResource.RegisterSubPath(const ResourceId: ShortString;
  Resource: TCustomRESTResource);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, Resource);
end;

procedure TCustomRESTResource.RegisterSubPath(const ResourceId: ShortString;
  ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, ResourceClass, Tag);
end;

procedure TCustomRESTResource.RegisterSubPath(const ResourceId: ShortString;
  CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
begin
  SubPathResourcesNeeded;
  FSubPathResources.Register(ResourceId, CreateCallback, Tag);
end;

procedure TCustomRESTResource.SetDefaultSubPath(const ParamName: String;
  Resource: TCustomRESTResource);
begin
  if Resource = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Instance']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(Resource);
end;

procedure TCustomRESTResource.SetDefaultSubPath(const ParamName: String;
  ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
begin
  if ResourceClass = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Class']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(ResourceClass, Tag);
end;

procedure TCustomRESTResource.SetDefaultSubPath(const ParamName: String;
  CreateCallback: TRESTResourceCreateEvent; Tag: PtrInt);
begin
  if CreateCallback = nil then
    raise Exception.CreateFmt(SDefaultSubPathError, [ParamName, 'Event']);
  SubPathResourcesNeeded;
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(CreateCallback, Tag);
end;

destructor TCustomRESTResource.Destroy;
begin
  FSubPathResources.Free;
  inherited Destroy;
end;

procedure TCustomRESTResource.HandleSubPath(const SubPath: String; var SubPathResourceDef: TRESTResourceDef);
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

procedure TCustomRESTResource.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['DELETE']);
end;

procedure TCustomRESTResource.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['GET']);
end;

procedure TCustomRESTResource.HandlePost(ARequest: TRequest; AResponse: TResponse);
begin
  SetResponseStatus(AResponse, 405, 'Method "%s" not allowed for this resource', ['POST']);
end;

procedure TCustomRESTResource.HandlePut(ARequest: TRequest; AResponse: TResponse);
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

function TRESTResourceStore.Get(const ResourceId: ShortString): TCustomRESTResource;
var
  Def: TRESTResourceDef;
begin
  Def := TRESTResourceDef(FList.Find(ResourceId));
  if Def = nil then
    Def := DefaultResourceDef;
  Result := Def.Resource;
  //todo: notify about creation
  if Result = nil then
  begin
    if Def.OnResourceCreate <> nil then
       Def.OnResourceCreate(Result, Def.Tag)
    else if (Def.ResourceClass <> nil) then
      Result := Def.ResourceClass.Create;
  end;
end;

procedure TRESTResourceStore.Register(const ResourceId: ShortString;
  Resource: TCustomRESTResource);
var
  Def: TRESTResourceDef;
begin
  if Resource = nil then
    raise Exception.CreateFmt(SRegisterError, [ResourceId, 'Instance']);
  Def := TRESTResourceDef.Create(Resource);
  FList.Add(ResourceId, Def);
end;

procedure TRESTResourceStore.Register(const ResourceId: ShortString;
  ResourceClass: TCustomRESTResourceClass; Tag: PtrInt);
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

constructor TRESTResourceDef.Create(AResource: TCustomRESTResource);
begin
  inherited Create;
  FResource := AResource;
end;

constructor TRESTResourceDef.Create(AResourceClass: TCustomRESTResourceClass;
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

function TRESTResourceDef.GetResource(URIParams: TJSONObject; OnResourceLoad: TRESTResourceLoadEvent): TCustomRESTResource;
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

