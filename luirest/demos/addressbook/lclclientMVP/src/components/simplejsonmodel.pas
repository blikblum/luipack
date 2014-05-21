unit SimpleJSONModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, fpjson, db;

type

  TSimpleJSONCollection = class;

  { TSimpleJSONModel }

  TSimpleJSONModel = class(TPersistent)
  private
    FCollection: TSimpleJSONCollection;
    FData: TJSONObject;
    FResource: IJSONObjectResource;
    FOwnsData: Boolean;
    class var FDefaultResourceClient: IResourceClient;
    function GetIndex: Integer;
    function GetIsNew: Boolean;
    class procedure SetDefaultResourceClient(Value: IResourceClient); static;
    procedure ResourceNeeded;
  protected
    function CreateData: TJSONObject; virtual;
    class function GetIdField: String; virtual;
    class function GetResourceClient: IResourceClient; virtual;
    class function GetResourceName: String; virtual;
    property Collection: TSimpleJSONCollection read FCollection;
    property ResourceClient: IResourceClient read GetResourceClient;
    property ResourceName: String read GetResourceName;
  public
    constructor Create(Data: TJSONObject = nil); virtual;
    destructor Destroy; override;
    class procedure ClearCache;
    procedure Delete;
    procedure Fetch;
    procedure Fetch(const IdValue: Variant);
    function ParamByName(const ParamName: String): TParam;
    procedure Save;
    property Data: TJSONObject read FData;
    class property DefaultResourceClient: IResourceClient write SetDefaultResourceClient;
    property Index: Integer read GetIndex;
    property IsNew: Boolean read GetIsNew;
  end;

  TSimpleJSONModelClass = class of TSimpleJSONModel;

  { TSimpleJSONCollection }

  TSimpleJSONCollection = class(TPersistent)
  private
    FData: TJSONArray;
    FModelClass: TSimpleJSONModelClass;
    FResource: IJSONArrayResource;
    FItemResource: IJSONObjectResource;
    FOwnsData: Boolean;
    function GetParams: TParams;
    procedure ItemResourceNeeded(Item: TSimpleJSONModel);
  protected
    procedure Changed;
    procedure DeleteItem(Item: TSimpleJSONModel);
    procedure FetchItem(Item: TSimpleJSONModel);
    procedure FetchItem(Item: TSimpleJSONModel; const IdValue: Variant);
    procedure ParseData(ResourceData: TJSONData; out CollectionData: TJSONArray;
      out FreeCollectionData: Boolean); virtual;
    procedure ResetData;
    procedure SaveItem(Item: TSimpleJSONModel);
    property Resource: IJSONArrayResource read FResource;
  public
    constructor Create(ModelClass: TSimpleJSONModelClass);
    destructor Destroy; override;
    function Add: TSimpleJSONModel;
    function GetItem(ItemData: TJSONObject): TSimpleJSONModel;
    function GetItem(ItemIndex: Integer): TSimpleJSONModel;
    procedure Fetch;
    function ParamByName(const ParamName: String): TParam;
    property Data: TJSONArray read FData;
    property Params: TParams read GetParams;
  end;

implementation

uses
  LCLProc;

{ TSimpleJSONCollection }

function TSimpleJSONCollection.GetParams: TParams;
begin
  Result := FResource.Params;
end;

procedure TSimpleJSONCollection.ItemResourceNeeded(Item: TSimpleJSONModel);
begin
  if FItemResource = nil then
    FItemResource := FModelClass.GetResourceClient.GetJSONObject(FModelClass.GetResourceName);
  FItemResource.Params.Assign(FResource.Params);
  FItemResource.SetData(Item.Data, False);
  //todo: merge item params
end;

procedure TSimpleJSONCollection.ResetData;
begin
  if FOwnsData then
    FreeAndNil(FData);
  ParseData(FResource.Data, FData, FOwnsData);
  Changed;
end;

procedure TSimpleJSONCollection.Changed;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

procedure TSimpleJSONCollection.DeleteItem(Item: TSimpleJSONModel);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Delete then
  begin
    //todo: add check to see if really removed
    Data.Extract(Item.Data);
    Item.FOwnsData := True;
    FPONotifyObservers(Self, ooDeleteItem, Item);
  end;
end;

procedure TSimpleJSONCollection.FetchItem(Item: TSimpleJSONModel);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Fetch then
  begin
    if Data.IndexOf(Item.Data) <> -1 then
      Changed;
  end;
end;

procedure TSimpleJSONCollection.FetchItem(Item: TSimpleJSONModel;
  const IdValue: Variant);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Fetch(IdValue) then
  begin
    if Data.IndexOf(Item.Data) <> -1 then
      Changed;
  end;
end;

procedure TSimpleJSONCollection.ParseData(ResourceData: TJSONData; out
  CollectionData: TJSONArray; out FreeCollectionData: Boolean);
begin
  if ResourceData.JSONType = jtArray then
    CollectionData := TJSONArray(ResourceData);
  FreeCollectionData := False;
end;

procedure TSimpleJSONCollection.SaveItem(Item: TSimpleJSONModel);
var
  ItemIsNew: Boolean;
begin
  ItemResourceNeeded(Item);
  ItemIsNew := Item.IsNew;
  if FItemResource.Save then
  begin
    if Data.IndexOf(Item.Data) = -1 then
    begin
      if Item.FOwnsData then
      begin
        Item.FOwnsData := False;
        Data.Add(Item.Data);
      end
      else
        Data.Add(TJSONObject(Item.Data.Clone));
    end;
    if ItemIsNew then
      FPONotifyObservers(Self, ooAddItem, Item)
    else
      FPONotifyObservers(Self, ooChange, Item);
  end;
end;

constructor TSimpleJSONCollection.Create(ModelClass: TSimpleJSONModelClass);
begin
  FModelClass := ModelClass;
  FResource := ModelClass.GetResourceClient.GetJSONArray(ModelClass.GetResourceName);
end;

destructor TSimpleJSONCollection.Destroy;
begin
  if FOwnsData then
    FreeAndNil(FData);
  inherited Destroy;
end;

function TSimpleJSONCollection.Add: TSimpleJSONModel;
begin
  Result := FModelClass.Create;
  Result.FCollection := Self;
end;

function TSimpleJSONCollection.GetItem(ItemData: TJSONObject): TSimpleJSONModel;
begin
  if Data.IndexOf(ItemData) = -1 then
    raise Exception.Create('ItemData must be an item of the collection');
  Result := FModelClass.Create(ItemData);
  Result.FCollection := Self;
end;

function TSimpleJSONCollection.GetItem(ItemIndex: Integer): TSimpleJSONModel;
var
  ItemData: TJSONObject;
begin
  ItemData := Data.Objects[ItemIndex];
  Result := FModelClass.Create(ItemData);
  Result.FCollection := Self;
end;

procedure TSimpleJSONCollection.Fetch;
begin
  if FResource.Fetch then
  begin
    ResetData;
  end;
end;

function TSimpleJSONCollection.ParamByName(const ParamName: String): TParam;
begin
  Result := FResource.ParamByName(ParamName);
end;

{ TSimpleJSONModel }

class procedure TSimpleJSONModel.SetDefaultResourceClient(Value: IResourceClient);
begin
  //todo: some check
  FDefaultResourceClient := Value;
end;

function TSimpleJSONModel.GetIndex: Integer;
begin
  if FCollection <> nil then
    Result := FCollection.Data.IndexOf(FData)
  else
    Result := -1;
end;

function TSimpleJSONModel.GetIsNew: Boolean;
begin
  //todo: the current approach does not works when the resource id is know before add
  //i.e. when PUT method is added to create
  //add a state property and check in Create and when Save? This will allow a deleted state also
  Result := FData.Find(GetIdField) = nil;
end;

procedure TSimpleJSONModel.ResourceNeeded;
begin
  if FResource = nil then
  begin
    FResource := ResourceClient.GetJSONObject(GetResourceName);
    FResource.SetData(FData, False);
  end;
end;

function TSimpleJSONModel.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

class function TSimpleJSONModel.GetIdField: String;
begin
  //todo: get through Resource info?
  Result := 'id';
end;

class function TSimpleJSONModel.GetResourceClient: IResourceClient;
begin
  Result := FDefaultResourceClient;
end;

class function TSimpleJSONModel.GetResourceName: String;
var
  i: Integer;
begin
  Result := LowerCase(ClassName);
  if Result[1] = 't' then
    System.Delete(Result, 1, 1);
  i := Pos('model', Result);
  if i > 0 then
    System.Delete(Result, i, Length(Result));
end;

constructor TSimpleJSONModel.Create(Data: TJSONObject);
begin
  if Data = nil then
  begin
    FData := CreateData;
    FOwnsData := True;
  end
  else
    FData := Data;
end;

destructor TSimpleJSONModel.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

class procedure TSimpleJSONModel.ClearCache;
begin
  GetResourceClient.InvalidateCache(GetResourceName);
end;

procedure TSimpleJSONModel.Delete;
begin
  if FCollection <> nil then
    FCollection.DeleteItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Delete;
  end;
end;

procedure TSimpleJSONModel.Fetch;
begin
  if FCollection <> nil then
    FCollection.FetchItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Fetch;
  end;
end;

procedure TSimpleJSONModel.Fetch(const IdValue: Variant);
begin
  if FCollection <> nil then
    FCollection.FetchItem(Self, IdValue)
  else
  begin
    ResourceNeeded;
    FResource.Fetch(IdValue);
  end;
end;

function TSimpleJSONModel.ParamByName(const ParamName: String): TParam;
begin
  ResourceNeeded;
  Result := FResource.ParamByName(ParamName);
end;

procedure TSimpleJSONModel.Save;
begin
  if FCollection <> nil then
    FCollection.SaveItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Save;
  end;
end;

end.

