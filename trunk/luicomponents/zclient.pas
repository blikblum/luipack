unit ZClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZDataset, ZConnection, fpjson, fpjsonrtti;

type

  { TZCustomClient }

  TZCustomClient = class(TComponent)
  private
    FConnection: TZConnection;
    FDeStreamer: TJSONDeStreamer;
    FQueryDefs: TJSONObject;
    FLastActiveCheck: TDateTime;
    procedure CheckQueryDefs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
    function CreateQuery(AOwner: TComponent; const QueryAlias: String): TZReadOnlyQuery;
    procedure LoadDefs(const FileName: String);
    property Connection: TZConnection read FConnection;
  end;

implementation

uses
  dateutils, LuiJSONUtils;

constructor TZCustomClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := TZConnection.Create(Self);
  FDeStreamer := TJSONDeStreamer.Create(Self);
end;

destructor TZCustomClient.Destroy;
begin
  FQueryDefs.Free;
  inherited Destroy;
end;

function TZCustomClient.Active: Boolean;
begin
  Result := Connection.Connected;
  if Result then
  begin
    if MinutesBetween(Now, FLastActiveCheck) > 5 then
    begin
      try
        Connection.Reconnect;
      except
        Result := False;
      end;
      Result := Connection.Connected;
    end;
  end
  else
  begin
    Result := True;
    try
      Connection.Connect;
    except
      Result := False;
    end;
  end;
  FLastActiveCheck := Now;
end;


procedure TZCustomClient.LoadDefs(const FileName: String);
var
  DefFile: TFileStream;
begin
  FreeAndNil(FQueryDefs);
  DefFile := TFileStream.Create(FileName, fmOpenRead);
  try
    FQueryDefs := StreamToJSON(DefFile) as TJSONObject;
  finally
    DefFile.Destroy;
  end;
end;


function TZCustomClient.CreateQuery(AOwner: TComponent; const QueryAlias: String): TZReadOnlyQuery;
var
  QueryDef: TJSONObject;
  i: Integer;
begin
  CheckQueryDefs;
  i := FQueryDefs.IndexOfName(QueryAlias, True);
  if i = -1 then
    raise Exception.CreateFmt('Dataset alias "%s" not found', [QueryAlias]);
  QueryDef := FQueryDefs.Items[i] as TJSONObject;

  Result := TZReadOnlyQuery.Create(AOwner);
  Result.Connection := Connection;

  FDeStreamer.JSONToObject(QueryDef, Result);
end;

procedure TZCustomClient.CheckQueryDefs;
begin
  if FQueryDefs = nil then
    raise Exception.Create('ESUSClient query definitions not initialized');
end;

end.

