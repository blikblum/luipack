unit LuiJSONClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, contnrs, LuiJSONUtils;

type

  TJSONChangeType = (jcCreated, jcDeleted, jcUpdated);
  TJSONChangeTypes = set of TJSONChangeType;
  TJSONChangeSetCallback = procedure(JSONObj: TJSONObject; Data: PtrInt; ChangeType: TJSONChangeType; var Continue: Boolean) of object;
  TJSONChangeSetStaticCallback = procedure(JSONObj: TJSONObject; Data: PtrInt; ChangeType: TJSONChangeType; var Continue: Boolean);

  { TJSONChangeSet }

  TJSONChangeSet = class(TComponent)
  private
    FCreatedList: TFPObjectList;
    FDeletedList: TFPObjectList;
    FUpdatedList: TFPObjectList;
    function GetCount: Cardinal;
    procedure SetOwnsCreated(AValue: Boolean);
    procedure SetOwnsDeleted(AValue: Boolean);
    procedure SetOwnsUpdated(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear(ChangeTypes: TJSONChangeTypes = [jcDeleted, jcCreated, jcUpdated]);
    function ForEachCall(Callback: TJSONChangeSetCallback; Data: PtrInt; ChangeTypes: TJSONChangeTypes = [jcDeleted, jcCreated, jcUpdated]): Boolean;
    function ForEachCall(Callback: TJSONChangeSetStaticCallback; Data: PtrInt; ChangeTypes: TJSONChangeTypes = [jcDeleted, jcCreated, jcUpdated]): Boolean;
    procedure MarkCreated(Obj: TJSONObject);
    procedure MarkDeleted(Obj: TJSONObject);
    procedure MarkUpdated(Obj: TJSONObject);
    property Count: Cardinal read GetCount;
    property OwnsCreated: Boolean write SetOwnsCreated;
    property OwnsDeleted: Boolean write SetOwnsDeleted;
    property OwnsUpdated: Boolean write SetOwnsUpdated;
  end;

  { TJSONGroupTree }

  TJSONGroupTree = class(TComponent)
  private
    FChildrenProperty: String;
    FItemKey: String;
    FData: TJSONArray;
    FGroupData: TJSONArray;
    FItemData: TJSONArray;
    FGroupKey: String;
    FSortFunction: TJSONArraySortCompare;
    FInvalid: Boolean;
    function GetData: TJSONArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData;
    function GetFirst: TJSONObject;
    procedure Invalidate;
    property Data: TJSONArray read GetData;
    property ChildrenProperty: String read FChildrenProperty write FChildrenProperty;
    property GroupData: TJSONArray read FGroupData write FGroupData;
    property GroupKey: String read FGroupKey write FGroupKey;
    property ItemData: TJSONArray read FItemData write FItemData;
    property ItemKey: String read FItemKey write FItemKey;
    property SortFunction: TJSONArraySortCompare read FSortFunction write FSortFunction;
  end;

  { TJSONLookup }

  TJSONLookup = class(TComponent)
  private
    FData: TJSONArray;
    FKeyProperty: String;
    FValueProperty: String;
    FOwnsData: Boolean;
    function FindValueData(const PropertyName: String; const KeyValue: Variant): TJSONData;
    function GetItems(KeyValue: Variant): TJSONObject;
    function GetKeys(Index: Integer): Variant;
    function GetStrings(const KeyValue: Variant): String;
    function GetValues(const KeyValue: Variant): Variant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const PropertyName: String; const KeyValue, Default: Variant): Variant;
    function IndexOf(const KeyValue: Variant): Integer;
    procedure LoadData(AData: TJSONArray; OwnsData: Boolean = False);
    procedure Map(Data: TJSONArray; const KeyProp, ValueProp: String);
    procedure Map(Data: TJSONObject; const KeyProp, ValueProp: String);
    property Data: TJSONArray read FData;
    property Items[KeyValue: Variant]: TJSONObject read GetItems;
    property KeyProperty: String read FKeyProperty write FKeyProperty;
    property Keys[Index: Integer]: Variant read GetKeys;
    property Strings[KeyValue: Variant]: String read GetStrings;
    property ValueProperty: String read FValueProperty write FValueProperty;
    property Values[KeyValue: Variant]: Variant read GetValues;
  end;

  { TWeakJSONArray }

  TWeakJSONArray = class(TJSONArray)
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  fgl, LuiJSONHelpers;

type
  TJSONObjectMap = specialize TFPGMap<Integer, TJSONObject>;

  TJSONArrayAccess = class(TJSONData)
  private
    FList: TFPObjectList;
  end;

{ TWeakJSONArray }

procedure TWeakJSONArray.AfterConstruction;
begin
  inherited AfterConstruction;
  TJSONArrayAccess(Self).FList.OwnsObjects := False;
end;

{ TJSONChangeSet }

constructor TJSONChangeSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreatedList := TFPObjectList.Create(False);
  FDeletedList := TFPObjectList.Create(True);
  FUpdatedList := TFPObjectList.Create(False);
end;

destructor TJSONChangeSet.Destroy;
begin
  FCreatedList.Destroy;
  FDeletedList.Destroy;
  FUpdatedList.Destroy;
  inherited Destroy;
end;

procedure TJSONChangeSet.Clear(ChangeTypes: TJSONChangeTypes);
begin
  if jcCreated in ChangeTypes then
    FCreatedList.Clear;
  if jcUpdated in ChangeTypes then
    FUpdatedList.Clear;
  if jcDeleted in ChangeTypes then
    FDeletedList.Clear;
end;

function TJSONChangeSet.ForEachCall(Callback: TJSONChangeSetCallback; Data: PtrInt;
  ChangeTypes: TJSONChangeTypes): Boolean;
var
  i: Integer;
begin
  Result := True;
  if jcDeleted in ChangeTypes then
  begin
    for i := 0 to FDeletedList.Count - 1 do
    begin
      Callback(TJSONObject(FDeletedList[i]), Data, jcDeleted, Result);
      if not Result then
        Exit;
    end;
  end;
  if jcUpdated in ChangeTypes then
  begin
    for i := 0 to FUpdatedList.Count - 1 do
    begin
      Callback(TJSONObject(FUpdatedList[i]), Data, jcUpdated, Result);
      if not Result then
        Exit;
    end;
  end;
  if jcCreated in ChangeTypes then
  begin
    for i := 0 to FCreatedList.Count - 1 do
    begin
      Callback(TJSONObject(FCreatedList[i]), Data, jcCreated, Result);
      if not Result then
        Exit;
    end;
  end;
end;

function TJSONChangeSet.ForEachCall(Callback: TJSONChangeSetStaticCallback; Data: PtrInt;
  ChangeTypes: TJSONChangeTypes): Boolean;
var
  i: Integer;
begin
  Result := True;
  if jcDeleted in ChangeTypes then
  begin
    for i := 0 to FDeletedList.Count - 1 do
    begin
      Callback(TJSONObject(FDeletedList[i]), Data, jcDeleted, Result);
      if not Result then
        Exit;
    end;
  end;
  if jcUpdated in ChangeTypes then
  begin
    for i := 0 to FUpdatedList.Count - 1 do
    begin
      Callback(TJSONObject(FUpdatedList[i]), Data, jcUpdated, Result);
      if not Result then
        Exit;
    end;
  end;
  if jcCreated in ChangeTypes then
  begin
    for i := 0 to FCreatedList.Count - 1 do
    begin
      Callback(TJSONObject(FCreatedList[i]), Data, jcCreated, Result);
      if not Result then
        Exit;
    end;
  end;
end;

procedure TJSONChangeSet.MarkCreated(Obj: TJSONObject);
begin
  if (Obj = nil) or (FCreatedList.IndexOf(Obj) > -1) then
    Exit;
  FCreatedList.Add(Obj);
end;

procedure TJSONChangeSet.MarkDeleted(Obj: TJSONObject);
begin
  if (Obj = nil) or (FDeletedList.IndexOf(Obj) > -1) then
    Exit;
  FUpdatedList.Remove(Obj);
  if FCreatedList.Remove(Obj) = -1 then
    FDeletedList.Add(Obj)
  else
  begin
    if FDeletedList.OwnsObjects then
      Obj.Destroy;
  end;
end;

procedure TJSONChangeSet.MarkUpdated(Obj: TJSONObject);
begin
  if (Obj = nil) or (FCreatedList.IndexOf(Obj) > -1) or
    (FUpdatedList.IndexOf(Obj) > -1) then
    Exit;
  FUpdatedList.Add(Obj);
end;

procedure TJSONChangeSet.SetOwnsDeleted(AValue: Boolean);
begin
  FDeletedList.OwnsObjects := AValue;
end;

procedure TJSONChangeSet.SetOwnsCreated(AValue: Boolean);
begin
  FCreatedList.OwnsObjects := AValue;
end;

function TJSONChangeSet.GetCount: Cardinal;
begin
  Result := FCreatedList.Count + FUpdatedList.Count + FDeletedList.Count;
end;

procedure TJSONChangeSet.SetOwnsUpdated(AValue: Boolean);
begin
  FUpdatedList.OwnsObjects := AValue;
end;

{ TJSONGroupTree }

function TJSONGroupTree.GetData: TJSONArray;
begin
  if FInvalid then
    LoadData;
  Result := FData;
end;

constructor TJSONGroupTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TJSONArray.Create;
  FGroupKey := 'id';
  FChildrenProperty := 'children';
end;

destructor TJSONGroupTree.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure TJSONGroupTree.LoadData;
var
  GroupMap: TJSONObjectMap;
  ChildrenData: TJSONArray;
  GroupObjData, ItemObjData: TJSONObject;
  i, KeyIndex: Integer;
begin
  FData.Clear;
  if (FGroupData = nil) or (FItemData = nil) then
    raise Exception.Create('TJSONGroupTree - ItemData and GroupData must be set');
  GroupMap := TJSONObjectMap.Create;
  //todo: add option to sort the resulting array after LoadData.
  //In this case sort the groupmap can be sorted
  try
    //LoadData map
    for i := 0 to FGroupData.Count -1 do
    begin
      GroupObjData := FGroupData.Objects[i];
      //todo: map JSONData to allow key in any type, not only integer
      GroupMap.Add(GroupObjData.Get(FGroupKey, 0), TJSONObject(GroupObjData.Clone));
    end;
    //add items to each group
    for i := 0 to FItemData.Count - 1 do
    begin
      ItemObjData := FItemData.Objects[i];
      //todo: see what todo with orphan items
      KeyIndex := GroupMap.IndexOf(ItemObjData.Get(FItemKey, -1));
      if KeyIndex > -1 then
      begin
        GroupObjData := GroupMap.Data[KeyIndex];
        ChildrenData := TJSONArray(GroupObjData.Find(FChildrenProperty));
        if ChildrenData = nil then
        begin
          ChildrenData := TWeakJSONArray.Create;
          GroupObjData.Add(FChildrenProperty, ChildrenData);
        end;
        ChildrenData.Add(ItemObjData);
      end;
    end;
    //LoadData the tree with only those with children
    for i := 0 to GroupMap.Count -1 do
    begin
      GroupObjData := GroupMap.Data[i];
      ChildrenData := TJSONArray(GroupObjData.Find(FChildrenProperty));
      if ChildrenData <> nil then
      begin
        if FSortFunction <> nil then
          SortJSONArray(ChildrenData, FSortFunction);
        FData.Add(GroupObjData);
      end
      else
        GroupObjData.Destroy;
    end;
  finally
    GroupMap.Destroy;
  end;
  FInvalid := False;
  FPONotifyObservers(Self, ooChange, nil);
end;

function TJSONGroupTree.GetFirst: TJSONObject;
var
  GroupObjData: TJSONObject;
  TheData, ChildrenData: TJSONArray;
  i: Integer;
begin
  Result := nil;
  TheData := GetData;
  for i := 0 to TheData.Count - 1 do
  begin
    GroupObjData := TheData.Objects[i];
    if FindJSONProp(GroupObjData, FChildrenProperty, ChildrenData) then
    begin
      if ChildrenData.Count > 0 then
      begin
        Result := ChildrenData.Objects[0];
        Exit;
      end;
    end;
  end;
end;

procedure TJSONGroupTree.Invalidate;
begin
  FInvalid := True;
end;

{ TJSONLookup }

function TJSONLookup.GetValues(const KeyValue: Variant): Variant;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(FValueProperty, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.Value
  else
    Result := Null;
end;

constructor TJSONLookup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyProperty := 'id';
  FValueProperty := 'name';
  FData := TJSONArray.Create;
  FOwnsData := True;
end;

destructor TJSONLookup.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

procedure TJSONLookup.AssignTo(Dest: TPersistent);
var
  i: Integer;
  ItemData, ValueData: TJSONData;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).Clear;
    for i := 0 to FData.Count - 1 do
    begin
      ItemData := FData.Items[i];
      if ItemData.JSONType = jtObject then
      begin
        ValueData := TJSONObject(ItemData).Find(FValueProperty);
        if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
          TStrings(Dest).Add(ValueData.AsString);
      end;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

function TJSONLookup.GetValue(const PropertyName: String; const KeyValue, Default: Variant): Variant;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(PropertyName, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.Value
  else
    Result := Default;
end;

function TJSONLookup.IndexOf(const KeyValue: Variant): Integer;
begin
  Result := FData.IndexOf([FKeyProperty, KeyValue]);
end;

procedure TJSONLookup.LoadData(AData: TJSONArray; OwnsData: Boolean);
begin
  if FOwnsData then
    FData.Free;
  if AData = nil then
    raise Exception.Create('TJONLookup - Data must be assigned');
  FData := AData;
  FOwnsData := OwnsData;
end;

procedure TJSONLookup.Map(Data: TJSONArray; const KeyProp, ValueProp: String);
var
  i: Integer;
  ItemData: TJSONData;
begin
  for i := 0 to Data.Count - 1 do
  begin
    ItemData := Data.Items[i];
    if ItemData.JSONType = jtObject then
      Map(TJSONObject(ItemData), KeyProp, ValueProp);
  end;
end;

procedure TJSONLookup.Map(Data: TJSONObject; const KeyProp, ValueProp: String);
begin
  SetJSONPropValue(Data, ValueProp, Values[Data.Get(KeyProp)], True);
end;

function TJSONLookup.FindValueData(const PropertyName: String; const KeyValue: Variant): TJSONData;
var
  ItemData: TJSONObject;
begin
  ItemData := FindJSONObject(FData, [FKeyProperty, KeyValue]);
  if ItemData <> nil then
    Result := ItemData.Find(PropertyName)
  else
    Result := nil;
end;

function TJSONLookup.GetItems(KeyValue: Variant): TJSONObject;
begin
  Result := FindJSONObject(FData, [FKeyProperty, KeyValue]);
end;

function TJSONLookup.GetKeys(Index: Integer): Variant;
var
  ItemData: TJSONData;
begin
  Result := Null;
  if (Index >= 0) and (Index < FData.Count) then
  begin
    ItemData := FData.Items[Index];
    if ItemData.JSONType = jtObject then
      Result := TJSONObject(ItemData).Get(FKeyProperty);
  end;
end;

function TJSONLookup.GetStrings(const KeyValue: Variant): String;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(FValueProperty, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.AsString
  else
    Result := '';
end;

end.

