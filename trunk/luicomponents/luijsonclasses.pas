unit LuiJSONClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONUtils;

type

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
  GroupMap.Sorted := True;
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
      if GroupMap.Find(ItemObjData.Get(FItemKey, -1), KeyIndex) then
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

