unit CAMICUApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresentationManager, LuiRESTClient, fpjson;

type

  { TCAMICUAppConfig }

  TCAMICUAppConfig = class(TPersistent)
  private
    FBaseURL: String;
    FFileName: String;
    function GetBaseURL: String;
    procedure SetBaseURL(const Value: String);
  public
    procedure Load;
    procedure Save;
    property FileName: String read FFileName write FFileName;
  published
    property BaseURL: String read GetBaseURL write SetBaseURL;
  end;

  TCAMICUApp = class(TComponent, IFPObserver)
  private
    FConfig: TCAMICUAppConfig;
    FResources: TRESTResourceClient;
    FPresentations: TPresentationManager;
    procedure InitializeResources;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
      Data: Pointer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    function ConnectToService: Boolean;
    property Config: TCAMICUAppConfig read FConfig;
    property Presentations: TPresentationManager read FPresentations;
  end;

implementation

uses
  fpjsonrtti, LuiJSONUtils, LuiServices, LuiJSONModel, LuiDataClasses,
  CAMICUConsts, FileUtil;

{ TCAMICUApp }

procedure TCAMICUApp.InitializeResources;
var
  JSONDestreamer: TJSONDeStreamer;
  ServiceDef: TJSONData;
begin
  JSONDestreamer := TJSONDeStreamer.Create(nil);
  try
    ServiceDef := TJSONFile.Load('webservice.json');
    try
      JSONDestreamer.JSONToCollection(ServiceDef, FResources.ModelDefs);
    finally
      ServiceDef.Free;
    end;
  finally
    JSONDestreamer.Destroy;
  end;
end;

procedure TCAMICUApp.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (ASender = FConfig) and (Operation = ooChange) then
    FResources.BaseURL := FConfig.BaseURL;
end;

constructor TCAMICUApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := TCAMICUAppConfig.Create;
  FConfig.FileName := ExpandFileNameUTF8('config.json');
  FConfig.FPOAttachObserver(Self);
  FResources := TRESTResourceClient.Create(Self);
  FPresentations := TPresentationManager.Create(Self);
  Services.Register(IResourceClient, FResources);
  Services.Register(IPresentationManager, FPresentations);
  TJSONModel.DefaultResourceClient := FResources as IResourceClient;
end;

destructor TCAMICUApp.Destroy;
begin
  FConfig.Destroy;
  inherited Destroy;
end;

procedure TCAMICUApp.Initialize;
begin
  FConfig.Load;
  FResources.BaseURL := FConfig.BaseURL;
  InitializeResources;
end;

function TCAMICUApp.ConnectToService: Boolean;
var
  AppInfoResource: IJSONObjectResource;
  AppInfoData: TJSONObject;
begin
  Result := Length(FResources.BaseURL) > 7;
  if Result then
  begin
    //lower the timeout to test if app server exists
    FResources.Http.Timeout := 1500;
    AppInfoResource := FResources.GetJSONObject('root');
    try
      Result := AppInfoResource.Fetch('serviceinfo');
      AppInfoData := AppInfoResource.Data;
    except
      Result := False;
    end;
  end;
  if Result then
  begin
    //wait a little more for regular requests
    FResources.Http.Timeout := 3500;
  end;
end;


{ TCAMICUAppConfig }

function TCAMICUAppConfig.GetBaseURL: String;
begin
  if FBaseURL = '' then
    Result := DefaultWSAddress
  else
    Result := FBaseURL;
end;

procedure TCAMICUAppConfig.SetBaseURL(const Value: String);
begin
  if Value <> DefaultWSAddress then
    FBaseURL := Value
  else
    FBaseURL := '';
end;

procedure TCAMICUAppConfig.Load;
var
  Data: TJSONObject;
  JSONDestreamer: TJSONDeStreamer;
begin
  if TryReadJSONFile(FileName, Data) then
  begin
    JSONDestreamer := TJSONDeStreamer.Create(nil);
    try
      JSONDestreamer.JSONToObject(Data, Self);
    finally
      JSONDestreamer.Destroy;
    end;
  end;
end;

procedure TCAMICUAppConfig.Save;
var
  Data: TJSONObject;
  JSONStreamer: TJSONStreamer;
begin
  JSONStreamer := TJSONStreamer.Create(nil);
  try
    Data := JSONStreamer.ObjectToJSON(Self);
  finally
    JSONStreamer.Destroy;
  end;
  TJSONFile.Save(Data, FileName, []);
  FPONotifyObservers(Self, ooChange, nil);
end;


end.

