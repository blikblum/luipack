unit LuiJSONModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiDataClasses, fpjson, db, contnrs;

type

  //todo: define json data ownership -> currently is a mess

  TJSONCollection = class;

  { TJSONModel }

  TJSONModel = class(TPersistent)
  private
    FCollection: TJSONCollection;
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
    procedure Save(const IdValue: Variant);
    property Collection: TJSONCollection read FCollection;
    property Data: TJSONObject read FData;
    class property DefaultResourceClient: IResourceClient write SetDefaultResourceClient;
    property Index: Integer read GetIndex;
    property IsNew: Boolean read GetIsNew;
  end;

  TJSONModelClass = class of TJSONModel;

  { TJSONCollection }

  TJSONCollection = class(TPersistent)
  private
    FData: TJSONArray;
    FItemClass: TJSONModelClass;
    FResource: IJSONArrayResource;
    FItemResource: IJSONObjectResource;
    FItems: TFPObjectList;
    FOwnsData: Boolean;
    function GetCount: Integer;
    function GetParams: TParams;
    function GetResource: IJSONArrayResource;
    procedure ItemResourceNeeded(Item: TJSONModel);
    procedure ItemsNeeded;
  protected
    procedure Changed;
    procedure DeleteItem(Item: TJSONModel);
    procedure FetchItem(Item: TJSONModel);
    procedure FetchItem(Item: TJSONModel; const IdValue: Variant);
    function GetItem(ItemIndex: Integer): TJSONModel;
    class function GetItemClass: TJSONModelClass; virtual;
    procedure ParseData(ResourceData: TJSONData; out CollectionData: TJSONArray;
      out FreeCollectionData: Boolean); virtual;
    procedure ResetData;
    procedure ResourceNeeded;
    property Resource: IJSONArrayResource read GetResource;
  public
    constructor Create; virtual;
    constructor Create(AItemClass: TJSONModelClass); virtual;
    destructor Destroy; override;
    function Add(Item: TJSONModel): TJSONModel;
    function CreateItem: TJSONModel;
    function Get(ItemData: TJSONObject): TJSONModel;
    function Fetch: Boolean;
    function Find(const Id: Variant): TJSONModel;
    function IndexOf(Item: TJSONModel): Integer;
    function ParamByName(const ParamName: String): TParam;
    function SaveItem(Item: TJSONModel; AddItem: Boolean = True): Boolean;
    function SaveItem(Item: TJSONModel; const IdValue: Variant; AddItem: Boolean = True): Boolean;
    property Count: Integer read GetCount;
    property Data: TJSONArray read FData;
    property ItemClass: TJSONModelClass read FItemClass;
    property Items[Index: Integer]: TJSONModel read GetItem; default;
    property Params: TParams read GetParams;
  end;

  TJSONCollectionClass = class of TJSONCollection;

  { TJSONCollectionSelection }

  TJSONCollectionSelection = class(TPersistent, IFPObserver)
  private
    FCollection: TJSONCollection;
    FData: TJSONObject;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure SetData(Value: TJSONObject);
  public
    constructor Create(ACollection: TJSONCollection);
    destructor Destroy; override;
    property Data: TJSONObject read FData write SetData;
  end;

implementation

uses
  LuiJSONUtils;

{ TJSONCollectionSelection }

procedure TJSONCollectionSelection.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (ASender = FCollection) then
  begin
    case Operation of
      //free itself
      ooFree: Free;
    end;
  end;
end;

procedure TJSONCollectionSelection.SetData(Value: TJSONObject);
begin
  if FData = Value then Exit;
  FData := Value;
  FPONotifyObservers(Self, ooChange, nil);
end;

constructor TJSONCollectionSelection.Create(ACollection: TJSONCollection);
begin
  ACollection.FPOAttachObserver(Self);
  FCollection := ACollection;
end;

destructor TJSONCollectionSelection.Destroy;
begin
  if FCollection <> nil then
  begin
    FCollection.FPODetachObserver(Self);
    FCollection := nil;
  end;
  inherited Destroy;
end;

{ TJSONCollection }

function TJSONCollection.GetParams: TParams;
begin
  ResourceNeeded;
  Result := FResource.Params;
end;

function TJSONCollection.GetResource: IJSONArrayResource;
begin
  ResourceNeeded;
  Result := FResource;
end;

function TJSONCollection.GetCount: Integer;
begin
  if FItems <> nil then
    Result := FItems.Count
  else if FData <> nil then
    Result := FData.Count
  else
    Result := 0;
end;

procedure TJSONCollection.ItemResourceNeeded(Item: TJSONModel);
begin
  ResourceNeeded;
  if FItemResource = nil then
    FItemResource := FItemClass.GetResourceClient.GetJSONObject(FItemClass.GetResourceName);
  FItemResource.Params.Assign(FResource.Params);
  FItemResource.SetData(Item.Data, False);
  //todo: merge item params
end;

procedure TJSONCollection.ItemsNeeded;
var
  i: Integer;
  Model: TJSONModel;
  ItemData: TJSONData;
begin
  if FItems = nil then
  begin
    FItems := TFPObjectList.Create(True);
    if FData <> nil then
    begin
      for i := 0 to FData.Count - 1 do
      begin
        ItemData := FData.Items[i];
        //todo: raise an error if not a json object?
        if ItemData.JSONType = jtObject then
        begin
          Model := FItemClass.Create(TJSONObject(ItemData));
          Model.FCollection := Self;
          FItems.Add(Model);
        end;
      end;
    end;
  end;
end;

procedure TJSONCollection.ResetData;
begin
  ResourceNeeded;
  if FOwnsData then
    FreeAndNil(FData);
  ParseData(FResource.Data, FData, FOwnsData);
  FreeAndNil(FItems);
  Changed;
end;

procedure TJSONCollection.ResourceNeeded;
begin
  if FResource = nil then
    FResource := FItemClass.GetResourceClient.GetJSONArray(FItemClass.GetResourceName);
end;

procedure TJSONCollection.Changed;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

procedure TJSONCollection.DeleteItem(Item: TJSONModel);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Delete then
  begin
    //todo: CreateItem check to see if really removed
    Data.Extract(Item.Data);
    Item.FOwnsData := True;
    FPONotifyObservers(Self, ooDeleteItem, Item);
    if FItems <> nil then
      FItems.Remove(Item);
  end;
end;

procedure TJSONCollection.FetchItem(Item: TJSONModel);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Fetch then
  begin
    if Data.IndexOf(Item.Data) <> -1 then
      FPONotifyObservers(Self, ooChange, Item);
  end;
end;

procedure TJSONCollection.FetchItem(Item: TJSONModel;
  const IdValue: Variant);
begin
  ItemResourceNeeded(Item);
  if FItemResource.Fetch(IdValue) then
  begin
    if Data.IndexOf(Item.Data) <> -1 then
      FPONotifyObservers(Self, ooChange, Item);
  end;
end;

procedure TJSONCollection.ParseData(ResourceData: TJSONData; out
  CollectionData: TJSONArray; out FreeCollectionData: Boolean);
begin
  if ResourceData.JSONType = jtArray then
    CollectionData := TJSONArray(ResourceData);
  FreeCollectionData := False;
end;

function TJSONCollection.SaveItem(Item: TJSONModel; AddItem: Boolean): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Save;
  if Result and AddItem then
  begin
    if IndexOf(Item) = -1 then
      Add(Item);
  end;
end;

function TJSONCollection.SaveItem(Item: TJSONModel; const IdValue: Variant;
  AddItem: Boolean): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Save(IdValue);
  if Result and AddItem then
  begin
    if IndexOf(Item) = -1 then
      Add(Item);
  end;
end;

constructor TJSONCollection.Create;
begin
  FItemClass := GetItemClass;
end;

constructor TJSONCollection.Create(AItemClass: TJSONModelClass);
begin
  if AItemClass = nil then
    raise Exception.Create('TJSONCollection - ItemClass is not set');
  FItemClass := AItemClass;
end;

destructor TJSONCollection.Destroy;
begin
  FItems.Free;
  if FOwnsData then
    FreeAndNil(FData);
  inherited Destroy;
end;

function TJSONCollection.CreateItem: TJSONModel;
begin
  Result := Add(FItemClass.Create);
end;

function TJSONCollection.Add(Item: TJSONModel): TJSONModel;
begin
  ItemsNeeded;
  FItems.Add(Item);
  if FData = nil then
  begin
    FData := TJSONArray.Create;
    FOwnsData := True;
  end;
  Item.FOwnsData := False;
  FData.Add(Item.Data);
  Item.FCollection := Self;
  FPONotifyObservers(Self, ooAddItem, Item);
end;

function TJSONCollection.Get(ItemData: TJSONObject): TJSONModel;
var
  ItemIndex: Integer;
begin
  //todo: search for ItemData in the FItems directly??
  ItemIndex := Data.IndexOf(ItemData);
  if ItemIndex = -1 then
    raise Exception.Create('ItemData must be an item of the collection');
  Result := GetItem(ItemIndex);
end;

function TJSONCollection.GetItem(ItemIndex: Integer): TJSONModel;
begin
  ItemsNeeded;
  if (ItemIndex < 0) or (ItemIndex >= FItems.Count) then
    raise Exception.Create('Invalid Index');
  Result := TJSONModel(FItems[ItemIndex]);
end;

class function TJSONCollection.GetItemClass: TJSONModelClass;
begin
  Result := TJSONModel;
end;

function TJSONCollection.Fetch: Boolean;
begin
  ResourceNeeded;
  Result := FResource.Fetch;
  if Result then
  begin
    ResetData;
  end;
end;

function TJSONCollection.Find(const Id: Variant): TJSONModel;
var
  ItemIndex: Integer;
begin
  //todo: get directly from FItems or CreateItem mechanism to ensure consistency between FItems and FData
  ItemIndex := GetJSONIndexOf(FData, [FItemClass.GetIdField, Id]);
  if ItemIndex > -1 then
    Result := GetItem(ItemIndex)
  else
    Result := nil;
end;

function TJSONCollection.IndexOf(Item: TJSONModel): Integer;
begin
  if FItems <> nil then
    Result := FItems.IndexOf(Item)
  else if FData <> nil then
    Result := FData.IndexOf(Item.Data)
  else
    Result := -1;
end;

function TJSONCollection.ParamByName(const ParamName: String): TParam;
begin
  ResourceNeeded;
  Result := FResource.ParamByName(ParamName);
end;

{ TJSONModel }

class procedure TJSONModel.SetDefaultResourceClient(Value: IResourceClient);
begin
  //todo: some check
  FDefaultResourceClient := Value;
end;

function TJSONModel.GetIndex: Integer;
begin
  if FCollection <> nil then
    Result := FCollection.Data.IndexOf(FData)
  else
    Result := -1;
end;

function TJSONModel.GetIsNew: Boolean;
begin
  //todo: the current approach does not works when the resource id is know before add
  //i.e. when PUT method is added to create
  //add a state property and check in Create and when Save? This will allow a deleted state also
  Result := FData.Find(GetIdField) = nil;
end;

procedure TJSONModel.ResourceNeeded;
begin
  if FResource = nil then
  begin
    FResource := ResourceClient.GetJSONObject(GetResourceName);
    FResource.SetData(FData, False);
  end;
end;

function TJSONModel.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

class function TJSONModel.GetIdField: String;
begin
  //todo: get through Resource info?
  Result := 'id';
end;

class function TJSONModel.GetResourceClient: IResourceClient;
begin
  Result := FDefaultResourceClient;
end;

class function TJSONModel.GetResourceName: String;
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

constructor TJSONModel.Create(Data: TJSONObject);
begin
  if Data = nil then
  begin
    FData := CreateData;
    FOwnsData := True;
  end
  else
    FData := Data;
end;

destructor TJSONModel.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

class procedure TJSONModel.ClearCache;
begin
  GetResourceClient.InvalidateCache(GetResourceName);
end;

procedure TJSONModel.Delete;
begin
  if FCollection <> nil then
    FCollection.DeleteItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Delete;
  end;
end;

procedure TJSONModel.Fetch;
begin
  if FCollection <> nil then
    FCollection.FetchItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Fetch;
  end;
end;

procedure TJSONModel.Fetch(const IdValue: Variant);
begin
  if FCollection <> nil then
    FCollection.FetchItem(Self, IdValue)
  else
  begin
    ResourceNeeded;
    FResource.Fetch(IdValue);
  end;
end;

function TJSONModel.ParamByName(const ParamName: String): TParam;
begin
  ResourceNeeded;
  Result := FResource.ParamByName(ParamName);
end;

procedure TJSONModel.Save;
begin
  if FCollection <> nil then
    FCollection.SaveItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Save;
  end;
end;

procedure TJSONModel.Save(const IdValue: Variant);
begin
  if FCollection <> nil then
    FCollection.SaveItem(Self)
  else
  begin
    ResourceNeeded;
    FResource.Save(IdValue);
  end;
end;

end.

