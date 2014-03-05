unit DataHubProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataModel, DataView, typinfo, fpjson;

type
  { TDataHubProject }

  TDataHubProject = class(TComponent, IFieldResolver)
  private
    FFileName: String;
    FModels: TDataModels;
    FOnLoad: TNotifyEvent;
    FViews: TDataViews;
    procedure SetModels(Value: TDataModels);
    procedure SetViews(AValue: TDataViews);
    procedure HandleStreamProperty(Sender : TObject; AObject : TObject; Info : PPropInfo; var Res : TJSONData);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Find(const ID: String): TDataModelField;
    procedure Load;
    procedure LoadFromFile(const AFileName: String);
    procedure Save;
    procedure SaveToFile(const AFileName: String);
    property FileName: String read FFileName write FFileName;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
  published
    property Models: TDataModels read FModels write SetModels;
    property Views: TDataViews read FViews write SetViews;
  end;

implementation

uses
  LuiJSONUtils, fpjsonrtti;

{ TDataHubProject }

procedure TDataHubProject.SetModels(Value: TDataModels);
begin
  FModels.Assign(Value);
end;

procedure TDataHubProject.SetViews(AValue: TDataViews);
begin
  FViews.Assign(AValue);
end;

procedure TDataHubProject.HandleStreamProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; var Res: TJSONData);
begin
  if ((AObject is TDataModelField) and SameText(Info^.Name, 'FieldTypeName')) or
    ((AObject = Self) and SameText(Info^.Name, 'Tag')) then
  begin
    FreeAndNil(Res);
  end;
end;

constructor TDataHubProject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModels := TDataModels.Create;
  FViews := TDataViews.Create(Self, TDataView);
end;

destructor TDataHubProject.Destroy;
begin
  FViews.Destroy;
  FModels.Destroy;
  inherited Destroy;
end;

procedure TDataHubProject.Assign(Source: TPersistent);
begin
  if Source is TDataHubProject then
  begin
    FFileName := TDataHubProject(Source).FileName;
    Models := TDataHubProject(Source).Models;
  end
  else
    inherited Assign(Source);
end;

function TDataHubProject.Find(const ID: String): TDataModelField;
var
  i, j: Integer;
  Model: TDataModel;
begin
  for i := 0 to FModels.Count - 1 do
  begin
    Model := FModels[i];
    for j := 0 to Model.Fields.Count - 1 do
    begin
      Result := Model.Fields[j];
      if Result.FieldID = ID then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TDataHubProject.Load;
begin
  LoadFromFile(FFileName);
end;

procedure TDataHubProject.LoadFromFile(const AFileName: String);
var
  DeStreamer: TJSONDeStreamer;
  FileData: TJSONObject;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    FileData := TJSONFile.Load(AFileName) as TJSONObject;
    try
      DeStreamer.JSONToObject(FileData, Self);
    finally
      FileData.Free;
    end;
  finally
    DeStreamer.Destroy;
  end;
  if FOnLoad <> nil then
    FOnLoad(Self);
end;

procedure TDataHubProject.Save;
begin
  SaveToFile(FFileName);
end;

procedure TDataHubProject.SaveToFile(const AFileName: String);
var
  Streamer: TJSONStreamer;
  FileData: TJSONObject;
begin
  //todo: decouple save - add TProjectJSONStorage / TProjectSqlite3Storage etc
  //TprojectContainer /Holder
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := @HandleStreamProperty;
    FileData := Streamer.ObjectToJSON(Self);
    try
      TJSONFile.Save(FileData, AFileName, []);
    finally
      FileData.Free;
    end;
  finally
    Streamer.Destroy;
  end;
end;

end.

