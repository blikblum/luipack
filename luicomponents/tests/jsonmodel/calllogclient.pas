unit CallLogClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, db, fpjson, LuiFPTestHelpers;

type

  { TCallLogClient }

  TCallLogClient = class(TComponent, IResourceClient)
  public
    function Connected: Boolean;
    function GetDataset(const ModelName: String): IDatasetResource;
    function GetJSONArray(const ModelName: String): IJSONArrayResource;
    function GetJSONObject(const ModelName: String): IJSONObjectResource;
    function HasModel(const ModelName: String): Boolean;
    procedure InvalidateCache(const ModelName: String);
  end;

var
  Calls: TProcedureCalls;

implementation

uses
  LuiJSONHelpers, LuiJSONUtils, variants;

type

  { TCallLogDataResource }

  TCallLogDataResource = class(TInterfacedObject, IDataResource)
  private
    FParams: TParams;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Fetch: Boolean; overload;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    function Save(Options: TSaveOptions = []): Boolean; virtual;
    property Params: TParams read GetParams;
  end;

  { TCallLogDatasetResource }

  TCallLogDatasetResource = class(TCallLogDataResource, IDatasetResource)
  private
    FDataset: TDataSet;
  public
    function GetDataset: TDataSet;
    function Fetch(IdValue: Variant): Boolean; overload;
    property Dataset: TDataSet read GetDataset;
  end;

  { TCallLogJSONObjectResource }

  TCallLogJSONObjectResource = class(TCallLogDataResource, IJSONObjectResource)
  private
    FData: TJSONObject;
    FOwnsData: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Delete: Boolean;
    function Delete(IdValue: Variant): Boolean;
    function Fetch(IdValue: Variant): Boolean; overload;
    function GetData: TJSONObject;
    function Save(Options: TSaveOptions = []): Boolean; override;
    function Save(IdValue: Variant; Options: TSaveOptions = []): Boolean; overload;
    procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
    property Data: TJSONObject read GetData;
  end;

  { TCallLogJSONArrayResource }

  TCallLogJSONArrayResource = class(TCallLogDataResource, IJSONArrayResource)
  private
    FData: TJSONArray;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetData: TJSONArray;
    property Data: TJSONArray read GetData;
  end;

function VarToJSON(V: Variant): TJSONData;
begin
  case VarType(V) of
    varnull, varempty:  Result := TJSONNull.Create;
    varstring: Result := TJSONString.Create(V);
    vardouble, vardate: Result := TJSONFloatNumber.Create(V);
    varinteger, varlongword, varshortint: Result := TJSONIntegerNumber.Create(V);
    varint64, varqword: Result := TJSONInt64Number.Create(V);
    varboolean: Result := TJSONBoolean.Create(V);
    //nil is interpreted as olestream
    varolestr: Result := TJSONNull.Create;
  end;
end;

{ TCallLogDataResource }

constructor TCallLogDataResource.Create;
begin
  FParams := TParams.Create;
end;

destructor TCallLogDataResource.Destroy;
begin
  FParams.Destroy;
  inherited Destroy;
end;

function TCallLogDataResource.Fetch: Boolean;
begin
  Calls.Add('fetch', []);
end;

function TCallLogDataResource.GetParams: TParams;
begin
  Result := FParams;
end;

function TCallLogDataResource.ParamByName(const ParamName: String): TParam;
begin
  Result  := FParams.ParamByName(ParamName);
end;

function TCallLogDataResource.Save(Options: TSaveOptions): Boolean;
begin
  Calls.Add('save', []);
end;

{ TCallLogJSONArrayResource }

constructor TCallLogJSONArrayResource.Create;
begin
  inherited Create;
  FData := TJSONArray.Create;
end;

destructor TCallLogJSONArrayResource.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

function TCallLogJSONArrayResource.GetData: TJSONArray;
begin
  Result := FData;
end;



function TCallLogDatasetResource.GetDataset: TDataSet;
begin
  Result := FDataset;
end;

function TCallLogDatasetResource.Fetch(IdValue: Variant): Boolean;
begin
  Calls.Add('fetch', [VarToJSON(IdValue)]);
  Result := True;
end;

constructor TCallLogJSONObjectResource.Create;
begin
  inherited Create;
  FData := TJSONObject.Create;
  FOwnsData := True;
end;

destructor TCallLogJSONObjectResource.Destroy;
begin
  if FOwnsData then
    FData.Destroy;
  inherited Destroy;
end;

function TCallLogJSONObjectResource.Delete: Boolean;
begin
  Calls.Add('delete', []);
  Result := True;
end;

function TCallLogJSONObjectResource.Delete(IdValue: Variant): Boolean;
begin
  Calls.Add('delete', [VarToJSON(IdValue)]);
  Result := True;
end;

function TCallLogJSONObjectResource.Fetch(IdValue: Variant): Boolean;
begin
  Calls.Add('fetch', [VarToJSON(IdValue)]);
  Result := True;
end;

function TCallLogJSONObjectResource.GetData: TJSONObject;
begin
  Result := FData;
end;

const
  IdCount: Integer = 0;

function TCallLogJSONObjectResource.Save(Options: TSaveOptions): Boolean;
begin
  Result := inherited Save(Options);
  Inc(IdCount);
  FData.Integers['id'] := IdCount;
end;

function TCallLogJSONObjectResource.Save(IdValue: Variant; Options: TSaveOptions): Boolean;
begin
  if VarIsEmpty(IdValue) or VarIsNull(IdValue) then
  begin
    Inc(IdCount);
    FData.Integers['id'] := IdCount;
  end;
  Calls.Add('save', [VarToJSON(IdValue)]);
  Result := True;
end;

procedure TCallLogJSONObjectResource.SetData(JSONObj: TJSONObject; OwnsData: Boolean);
begin
  if FOwnsData then
    FData.Free;
  FData := JSONObj;
  FOwnsData := OwnsData;
  Calls.Add('setdata', []);
end;


function TCallLogClient.Connected: Boolean;
begin
  Result := True;
end;

function TCallLogClient.GetDataset(const ModelName: String): IDatasetResource;
begin
  Result := TCallLogDatasetResource.Create;
end;

function TCallLogClient.GetJSONArray(const ModelName: String): IJSONArrayResource;
begin
  Result := TCallLogJSONArrayResource.Create;
end;

function TCallLogClient.GetJSONObject(const ModelName: String): IJSONObjectResource;
begin
  Result := TCallLogJSONObjectResource.Create;
end;

function TCallLogClient.HasModel(const ModelName: String): Boolean;
begin
  Result := True;
end;

procedure TCallLogClient.InvalidateCache(const ModelName: String);
begin
  //
end;

initialization
  Calls := TProcedureCalls.Create;

finalization
  Calls.Destroy;

end.

