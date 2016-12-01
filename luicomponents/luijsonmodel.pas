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
    procedure Changed;
    function CreateData: TJSONObject; virtual;
    function DoFetch(const IdValue: Variant): Boolean; virtual;
    function DoSave(const IdValue: Variant; Options: TSaveOptions): Boolean; virtual;
    procedure ParseData({%H-}AData: TJSONObject); virtual;
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
    function Fetch: Boolean;
    function Fetch(const IdValue: Variant): Boolean;
    function ParamByName(const ParamName: String): TParam;
    function Save(Options: TSaveOptions = []): Boolean;
    function Save(const IdValue: Variant; Options: TSaveOptions = []): Boolean;
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
    function FetchItem(Item: TJSONModel): Boolean;
    function FetchItem(Item: TJSONModel; const IdValue: Variant): Boolean;
    function GetItem(ItemIndex: Integer): TJSONModel;
    class function GetItemClass: TJSONModelClass; virtual;
    function GetResourceName: String; virtual;
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
    procedure Clear;
    function CreateItem(AddItem: Boolean = True): TJSONModel;
    procedure Exchange(Index1, Index2: Integer);
    function Fetch: Boolean;
    function Find(const Id: Variant): TJSONModel;
    function Get(ItemData: TJSONObject): TJSONModel;
    function IndexOf(Item: TJSONModel): Integer;
    function ParamByName(const ParamName: String): TParam;
    procedure Remove(Item: TJSONModel);
    function SaveItem(Item: TJSONModel; Options: TSaveOptions = []; AddItem: Boolean = True): Boolean;
    function SaveItem(Item: TJSONModel; const IdValue: Variant; Options: TSaveOptions = []; AddItem: Boolean = True): Boolean;
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
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; {%H-}Data: Pointer);
    procedure SetData(Value: TJSONObject);
  public
    constructor Create(ACollection: TJSONCollection);
    destructor Destroy; override;
    property Data: TJSONObject read FData write SetData;
  end;

implementation

uses
  LuiJSONUtils, LuiJSONHelpers;

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
    FResource := FItemClass.GetResourceClient.GetJSONArray(GetResourceName);
end;

procedure TJSONCollection.Changed;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

procedure TJSONCollection.DeleteItem(Item: TJSONModel);
var
  ShouldRemove: Boolean;
begin
  if Item.IsNew then
    ShouldRemove := True
  else
  begin
    ItemResourceNeeded(Item);
    ShouldRemove := FItemResource.Delete;
  end;
  if ShouldRemove then
  begin
    //todo: CreateItem check to see if really removed
    Data.Extract(Item.Data);
    Item.FOwnsData := True;
    FPONotifyObservers(Self, ooDeleteItem, Item);
    Remove(Item);
  end;
end;

function TJSONCollection.FetchItem(Item: TJSONModel): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Fetch;
  if Result then
  begin
    if Data.IndexOf(Item.Data) <> -1 then
      FPONotifyObservers(Self, ooChange, Item);
  end;
end;

function TJSONCollection.FetchItem(Item: TJSONModel; const IdValue: Variant): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Fetch(IdValue);
  if Result then
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

function TJSONCollection.SaveItem(Item: TJSONModel; Options: TSaveOptions; AddItem: Boolean): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Save(Options);
  if Result then
  begin
    if AddItem and (IndexOf(Item) = -1) then
      Add(Item)
    else
      FPONotifyObservers(Self, ooChange, Item);
  end;
end;

function TJSONCollection.SaveItem(Item: TJSONModel; const IdValue: Variant;
  Options: TSaveOptions; AddItem: Boolean): Boolean;
begin
  ItemResourceNeeded(Item);
  Result := FItemResource.Save(IdValue, Options);
  if Result then
  begin
    if AddItem and (IndexOf(Item) = -1) then
      Add(Item)
    else
      FPONotifyObservers(Self, ooChange, Item);
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
  if not AItemClass.InheritsFrom(GetItemClass) then
    raise Exception.CreateFmt('TJSONCollection - "%s" does not inherit from "%s"',
      [AItemClass.ClassName, GetItemClass.ClassName]);
  FItemClass := AItemClass;
end;

destructor TJSONCollection.Destroy;
begin
  FItems.Free;
  if FOwnsData then
    FreeAndNil(FData);
  inherited Destroy;
end;

function TJSONCollection.CreateItem(AddItem: Boolean): TJSONModel;
begin
  Result := FItemClass.Create;
  Result.FCollection := Self;
  if AddItem then
    Add(Result);
end;

procedure TJSONCollection.Exchange(Index1, Index2: Integer);
begin
  if FData <> nil then
    FData.Exchange(Index1, Index2);
  if FItems <> nil then
    FItems.Exchange(Index1, Index2);
  Changed;
end;

function TJSONCollection.Add(Item: TJSONModel): TJSONModel;
begin
  Result := Item;
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

procedure TJSONCollection.Clear;
var
  PreviousCount: Integer;
begin
  PreviousCount := 0;
  if FData <> nil then
  begin
    PreviousCount := FData.Count;
    FData.Clear;
  end;
  if FItems <> nil then
    FItems.Clear;
  if PreviousCount > 0 then
    Changed;
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

function TJSONCollection.GetResourceName: String;
begin
  Result := FItemClass.GetResourceName;
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
  ItemIndex := FData.IndexOf([FItemClass.GetIdField, Id]);
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

procedure TJSONCollection.Remove(Item: TJSONModel);
begin
  if FItems <> nil then
    FItems.Remove(Item);
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

procedure TJSONModel.Changed;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

function TJSONModel.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function TJSONModel.DoFetch(const IdValue: Variant): Boolean;
begin
  if FCollection <> nil then
    Result := FCollection.FetchItem(Self, IdValue)
  else
  begin
    ResourceNeeded;
    Result := FResource.Fetch(IdValue);
  end;
  if Result then
    ParseData(FData);
  Changed;
end;

function TJSONModel.DoSave(const IdValue: Variant; Options: TSaveOptions): Boolean;
begin
  if FCollection <> nil then
    Result := FCollection.SaveItem(Self, IdValue, Options)
  else
  begin
    ResourceNeeded;
    Result := FResource.Save(IdValue, Options);
  end;
end;

procedure TJSONModel.ParseData(AData: TJSONObject);
begin
  //
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

function TJSONModel.Fetch: Boolean;
begin
  Result := DoFetch(Unassigned);
end;

function TJSONModel.Fetch(const IdValue: Variant): Boolean;
begin
  Result := DoFetch(IdValue);
end;

function TJSONModel.ParamByName(const ParamName: String): TParam;
begin
  ResourceNeeded;
  Result := FResource.ParamByName(ParamName);
end;

function TJSONModel.Save(Options: TSaveOptions): Boolean;
begin
  Result := DoSave(Unassigned, Options);
end;

function TJSONModel.Save(const IdValue: Variant; Options: TSaveOptions): Boolean;
begin
  Result := DoSave(IdValue, Options);
end;

end.

