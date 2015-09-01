unit LuiRESTSwagger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, HTTPDefs, LuiRESTServer, LuiRESTSqldb;

type

  { TSwaggerDefinitionResource }

  TSwaggerDefinitionResource = class(TRESTResource)
  private
    FRootResources: TRESTResourceStore;
    FPathList: TStringList;
    procedure AddPath(Resource: TRESTResource; const Path: String);
    procedure ParseResources(ResourceDefs: TRESTResourceStore;
      const BasePath: String);
  protected
    procedure Loaded(Tag: PtrInt); override;
  public
    constructor Create; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

{ TSwaggerDefinitionResource }

procedure TSwaggerDefinitionResource.Loaded(Tag: PtrInt);
var
  Store: TRESTResourceStore absolute Tag;
begin
  inherited Loaded(Tag);
  if not (Store is TRESTResourceStore) then
    raise Exception.Create('TSwaggerDefinitionResource expect a TRESTResourceStore in Tag');
  FRootResources := Store;
end;

constructor TSwaggerDefinitionResource.Create;
begin
  inherited Create;
  FPathList := TStringList.Create;
  FPathList.OwnsObjects := True;
end;

function CreatePathInfoData(Resource: TRESTResource): TJSONObject;
begin
  //todo: DRY + check response codes
  //todo: find a way to check if method is overriden
  Result := TJSONObject.Create;
  {
  if TMethod(Resource.HandleGet).Code <> TMethod(TRESTResource(@TRESTResource).HandleGet).Code then
  begin
    Result.Add('get', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '200', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end;

  if @Resource.HandlePut <> TRESTResource.HandlePut then
  begin
    Result.Add('put', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '200', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end;
  if @Resource.HandlePost <> @TRESTResource.HandlePost then
  begin
    Result.Add('post', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '201', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end;
  if @Resource.HandleDelete <> @TRESTResource.HandleDelete then
  begin
    Result.Add('delete', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '200', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end;
  }
end;

function CreateSqldbPathInfoData(Resource: TSqldbResource): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('get', TJSONObject.Create([
    'responses', TJSONObject.Create([
        '200', TJSONObject.Create([
           'description', 'OK'
        ])
      ])
  ]));
  if Resource.ReadOnly then
    Exit;
  if Resource.IsCollection then
  begin
    Result.Add('post', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '201', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end
  else
  begin
    Result.Add('put', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '200', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
    Result.Add('delete', TJSONObject.Create([
      'responses', TJSONObject.Create([
          '200', TJSONObject.Create([
             'description', 'OK'
          ])
        ])
    ]));
  end;
end;

type
  TRESTResourceAccess = class(TRESTResource)

  end;

procedure TSwaggerDefinitionResource.AddPath(Resource: TRESTResource;
  const Path: String);
var
  PathInfoData: TJSONObject;
begin
  if Resource is TSqldbResource then
    PathInfoData := CreateSqldbPathInfoData(TSqldbResource(Resource))
  else
    PathInfoData := CreatePathInfoData(Resource);
  FPathList.AddObject(Path, PathInfoData);
  if TRESTResourceAccess(Resource).SubPathResources <> nil then
    ParseResources(TRESTResourceAccess(Resource).SubPathResources, Path);
end;

procedure TSwaggerDefinitionResource.ParseResources(ResourceDefs: TRESTResourceStore; const BasePath: String);
var
  i: Integer;
  Resource: TRESTResource;
  SubPath, ParamName: String;
begin
  for i := 0 to ResourceDefs.Count - 1 do
  begin
    //todo: proper order. Use Index to insert or use a custom sort
    Resource := ResourceDefs[i].GetResource(URIParams, nil);
    if Resource = Self then
      continue;
    SubPath := ResourceDefs.Names[i];
    AddPath(Resource, BasePath + '/' + SubPath);
    ParamName := TRESTResourceAccess(Resource).SubPathParamName;
    if ParamName <> '' then
    begin
      Resource := TRESTResourceAccess(Resource).SubPathResources.DefaultResourceDef.GetResource(URIParams, nil);
      AddPath(Resource, BasePath + '/' + SubPath + '/{' + ParamName + '}');
    end;
  end;
end;

procedure TSwaggerDefinitionResource.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
var
  RootData, PathsData: TJSONObject;
  i: Integer;
begin

  RootData := TJSONObject.Create;
  PathsData := TJSONObject.Create;
  RootData.Add('paths', PathsData);
  try
    ParseResources(FRootResources, '');
    for i := 0 to FPathList.Count - 1 do
    begin
      PathsData.Add(FPathList[i], FPathList.Objects[i] as TJSONObject);
      FPathList.Objects[i] := nil;
    end;
    AResponse.Contents.Add(RootData.AsJSON);
    AResponse.Code := 200;
  finally
    RootData.Destroy;
  end;
end;

end.

