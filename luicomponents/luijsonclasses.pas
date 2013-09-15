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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build;
    property Data: TJSONArray read FData;
    property ChildrenProperty: String read FChildrenProperty write FChildrenProperty;
    property GroupData: TJSONArray read FGroupData write FGroupData;
    property GroupKey: String read FGroupKey write FGroupKey;
    property ItemData: TJSONArray read FItemData write FItemData;
    property ItemKey: String read FItemKey write FItemKey;
    property SortFunction: TJSONArraySortCompare read FSortFunction write FSortFunction;
  end;

implementation

uses
  fgl;

type
  TJSONObjectMap = specialize TFPGMap<Integer, TJSONObject>;

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

procedure TJSONGroupTree.Build;
var
  GroupMap: TJSONObjectMap;
  ChildrenData: TJSONArray;
  GroupObjData, ItemObjData: TJSONObject;
  i, KeyIndex: Integer;
begin
  FData.Clear;
  GroupMap := TJSONObjectMap.Create;
  //todo: add option to sort the resulting array after build.
  //In this case sort the groupmap can be sorted
  try
    //build map
    for i := 0 to FGroupData.Count -1 do
    begin
      GroupObjData := FGroupData.Objects[i];
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
          ChildrenData := TJSONArray.Create;
          GroupObjData.Add(FChildrenProperty, ChildrenData);
        end;
        ChildrenData.Add(ItemObjData.Clone);
      end;
    end;
    //build the tree with only those with children
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
end;

end.

