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

  { TCustomRESTResource }

  TCustomRESTResource = class(TPersistent)
  private
    FSubPathResources: TRESTResourceStore;
    FSubPathParamName: String;
    FURIParams: TJSONObject;
  protected
    procedure RegisterSubPathResource(const ResourceId: ShortString; Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
    procedure SetDefaultSubResource(const ParamName: String; Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
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
    FResource: TCustomRESTResource;
    FResourceClass: TCustomRESTResourceClass;
  public
    constructor Create(AResource: TCustomRESTResource; AResourceClass: TCustomRESTResourceClass);
    destructor Destroy; override;
    property ResourceClass: TCustomRESTResourceClass read FResourceClass write FResourceClass;
    property Resource: TCustomRESTResource read FResource write FResource;
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
    procedure RegisterResource(const ResourceId: ShortString; Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
  end;

  TCreateRESTResourceEvent = procedure(Resource: TCustomRESTResource) of object;

  { TRESTServiceModule }

  TRESTServiceModule = class(TCustomHTTPModule)
  private
    FBaseResources: TRESTResourceStore;
    FContentType: String;
    FOnCreateResource: TCreateRESTResourceEvent;
    FRootPath: String;
    procedure SetRootPath(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure RegisterResource(const ResourceName: ShortString; ResourceClass: TCustomRESTResourceClass);
  published
    property ContentType: String read FContentType write FContentType;
    property RootPath: String read FRootPath write SetRootPath;
    //events
    property OnCreateResource: TCreateRESTResourceEvent read FOnCreateResource write FOnCreateResource;
  end;

  procedure SetResponseStatus(AResponse: TResponse; StatusCode: Integer; const Message: String; Args: array of const);

implementation

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
  FBaseResources := TRESTResourceStore.Create;
  FContentType := 'application/json; charset=UTF-8';
  FRootPath := '/';
end;

destructor TRESTServiceModule.Destroy;
begin
  FBaseResources.Destroy;
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
      ResourceDef := FBaseResources.Find(URIPart);
      if ResourceDef = nil then
      begin
        SetResponseStatus(AResponse, 404, 'Resource "%s" not registered', [URIPart]);
        Exit;
      end;

      //todo: handle OnCreate
      if ResourceDef.Resource = nil then
      begin
        ResourceDef.Resource := ResourceDef.ResourceClass.Create;
        if Assigned(FOnCreateResource) then
          FOnCreateResource(ResourceDef.Resource);
      end;
      ResourceDef.Resource.FURIParams := URIParams;

      NextURIPart := GetURIPart(URIPath, PartOffset);
      while NextURIPart <> '' do
      begin
        NextResourceDef := nil;
        ResourceDef.Resource.HandleSubPath(NextURIPart, NextResourceDef);

        if NextResourceDef = nil then
        begin
          SetResponseStatus(AResponse, 404, 'Resource "%s" not registered. Resource: %s, SubPathRes: %s',
            [NextURIPart, ResourceDef.Resource.ClassName,
            BoolToStr(Boolean(ResourceDef.Resource.FSubPathResources = nil), True)]);
          Exit;
        end;

        ResourceDef := NextResourceDef;

        if ResourceDef.Resource = nil then
        begin
          ResourceDef.Resource := ResourceDef.ResourceClass.Create;
          if Assigned(FOnCreateResource) then
            FOnCreateResource(ResourceDef.Resource);
        end;
        ResourceDef.Resource.FURIParams := URIParams;

        NextURIPart := GetURIPart(URIPath, PartOffset);
      end;

      if MethodStr = 'GET' then
        ResourceDef.Resource.HandleGet(ARequest, AResponse)
      else if MethodStr = 'PUT' then
        ResourceDef.Resource.HandlePut(ARequest, AResponse)
      else if MethodStr = 'POST' then
      begin
        AResponse.Code := 201;
        ResourceDef.Resource.HandlePost(ARequest, AResponse)
      end
      else if MethodStr = 'DELETE' then
        ResourceDef.Resource.HandleDelete(ARequest, AResponse)
      else
        SetResponseStatus(AResponse, 501, 'Method "%s" not implemented', [MethodStr]);
    finally
      URIParams.Destroy;
    end;
  end
  else
    SetResponseStatus(AResponse, 404, 'Root path not found. URIPath: "%s" RootPath: "%s"', [URIPath, FRootPath]);
end;

procedure TRESTServiceModule.RegisterResource(const ResourceName: ShortString; ResourceClass: TCustomRESTResourceClass);
begin
  FBaseResources.RegisterResource(ResourceName, nil, ResourceClass);
end;

{ TCustomRESTResource }

procedure TCustomRESTResource.RegisterSubPathResource(const ResourceId: ShortString;
  Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
begin
  if FSubPathResources = nil then
    FSubPathResources := TRESTResourceStore.Create;
  FSubPathResources.RegisterResource(ResourceId, Resource, ResourceClass);
end;

procedure TCustomRESTResource.SetDefaultSubResource(const ParamName: String;
  Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
begin
  if FSubPathResources = nil then
    FSubPathResources := TRESTResourceStore.Create;
  //the class or the instance must be <> nil
  if (PtrUInt(Resource) + PtrUInt(ResourceClass)) = 0 then
    raise Exception.CreateFmt('SetDefaultResource (%s): Class or Instance must be <> nil', [ParamName]);
  FSubPathParamName := ParamName;
  FSubPathResources.DefaultResourceDef := TRESTResourceDef.Create(Resource, ResourceClass);
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
  if (Result = nil) and (Def.ResourceClass <> nil) then
    Result := Def.ResourceClass.Create;
end;

procedure TRESTResourceStore.RegisterResource(const ResourceId: ShortString;
  Resource: TCustomRESTResource; ResourceClass: TCustomRESTResourceClass);
var
  Def: TRESTResourceDef;
begin
  //the class or the instance must be <> nil
  if (PtrUInt(Resource) + PtrUInt(ResourceClass)) = 0 then
    raise Exception.CreateFmt('RegisterResource (%s): Class or Instance must be <> nil', [ResourceId]);
  Def := TRESTResourceDef.Create(Resource, ResourceClass);
  FList.Add(ResourceId, Def);
end;

{ TRESTResourceDef }

constructor TRESTResourceDef.Create(AResource: TCustomRESTResource;
  AResourceClass: TCustomRESTResourceClass);
begin
  inherited Create;
  FResource := AResource;
  FResourceClass := AResourceClass;
end;

destructor TRESTResourceDef.Destroy;
begin
  FResource.Free;
  inherited Destroy;
end;

end.

