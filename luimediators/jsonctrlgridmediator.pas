unit JSONCtrlGridMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fpjson, contnrs;

type

  TJSONCtrlGridMediator = class;

  { TJSONGridItem }

  TJSONGridItem = class(TPersistent)
  private
    FOwner: TJSONCtrlGridMediator;
    FControl: TControl;
    FData: TJSONObject;
  public
    procedure Notify(Operation: TFPObservedOperation; Data: Pointer);
    property Control: TControl read FControl;
    //todo: add metadata?
    property Data: TJSONObject read FData;
  end;

  TJSONGridItemCreate = procedure(Item: TJSONGridItem) of object;

  TJSONGridItemNotification = procedure(Item: TJSONGridItem; Operation: TFPObservedOperation; Data: Pointer) of object;

  { TJSONCtrlGridMediator }

  TJSONCtrlGridMediator = class(TComponent)
  private
    FControl: TWinControl;
    FItemControlClass: TControlClass;
    FData: TJSONArray;
    FItemList: TFPObjectList;
    FOnItemCreate: TJSONGridItemCreate;
    FOnItemNotification: TJSONGridItemNotification;
    procedure CheckProperties;
    procedure ClearGrid;
    procedure DoDeleteItem(Item: TJSONGridItem);
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TJSONGridItem;
    procedure InternalAddItem(ItemData: TJSONObject);
    procedure ItemNotification(Item: TJSONGridItem;
      Operation: TFPObservedOperation; Data: Pointer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(ItemData: TJSONObject);
    procedure DeleteItem(ItemData: TJSONObject);
    procedure LoadData;
    property Data: TJSONArray read FData write FData;
    property ItemControlClass: TControlClass read FItemControlClass write FItemControlClass;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TJSONGridItem read GetItems;
    function FindItem(ItemData: TJSONObject): TJSONGridItem;
  published
    property Control: TWinControl read FControl write FControl;
    property OnItemCreate: TJSONGridItemCreate read FOnItemCreate write FOnItemCreate;
    property OnItemNotification: TJSONGridItemNotification read FOnItemNotification write FOnItemNotification;
  end;

implementation

uses
  LuiRTTIUtils, LuiMiscUtils, Forms;

{ TJSONGridItem }

procedure TJSONGridItem.Notify(Operation: TFPObservedOperation; Data: Pointer);
begin
  FOwner.ItemNotification(Self, Operation, Data);
end;

{ TJSONCtrlGridMediator }

procedure TJSONCtrlGridMediator.ItemNotification(Item: TJSONGridItem;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Assigned(FOnItemNotification) then
    FOnItemNotification(Item, Operation, Data);
  //todo: make delete optional
  if Operation = ooDeleteItem then
    Application.QueueAsyncCall(TDataEvent(@DoDeleteItem), PtrInt(Item));
end;

constructor TJSONCtrlGridMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TFPObjectList.Create(True);
end;

destructor TJSONCtrlGridMediator.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

procedure TJSONCtrlGridMediator.AddItem(ItemData: TJSONObject);
begin
  CheckProperties;
  if FData.IndexOf(ItemData) = -1 then
    FData.Add(ItemData);
  InternalAddItem(ItemData);
end;

procedure TJSONCtrlGridMediator.DeleteItem(ItemData: TJSONObject);
var
  Item: TJSONGridItem;
begin
  Item := FindItem(ItemData);
  if Item <> nil then
    DoDeleteItem(Item);
end;

procedure TJSONCtrlGridMediator.LoadData;
var
  i: Integer;
begin
  CheckProperties;
  ClearGrid;
  if FData <> nil then
  begin
    for i := 0 to FData.Count - 1 do
    begin
      InternalAddItem(FData.Objects[i]);
    end;
  end;
end;

function TJSONCtrlGridMediator.FindItem(ItemData: TJSONObject): TJSONGridItem;
var
  i: Integer;
begin
  for i := 0 to FItemList.Count - 1 do
  begin
    Result := TJSONGridItem(FItemList.Items[i]);
    if Result.Data = ItemData then
      Exit;
  end;
  Result := nil;
end;

procedure TJSONCtrlGridMediator.ClearGrid;
var
  Item: TJSONGridItem;
  i: Integer;
begin
  for i := 0 to FItemList.Count - 1 do
  begin
    Item := TJSONGridItem(FItemList[i]);
    //todo: use releasecomponent?
    Item.Control.Destroy;
  end;
end;

procedure TJSONCtrlGridMediator.DoDeleteItem(Item: TJSONGridItem);
begin
  FreeAndNil(Item.FControl);
  FItemList.Remove(Item);
end;

function TJSONCtrlGridMediator.GetItemCount: Integer;
begin
  Result := FItemList.Count;
end;

function TJSONCtrlGridMediator.GetItems(Index: Integer): TJSONGridItem;
begin
  Result := TJSONGridItem(FItemList[Index]);
end;

procedure TJSONCtrlGridMediator.CheckProperties;
begin
  if FControl = nil then
    raise Exception.Create('Control not set');
  if FItemControlClass = nil then
    raise Exception.Create('ItemControlClass not set');
end;

procedure TJSONCtrlGridMediator.InternalAddItem(ItemData: TJSONObject);
var
  Item: TJSONGridItem;
  ItemControl: TControl;
begin
  Item := TJSONGridItem.Create;
  ItemControl := FItemControlClass.Create(Self);
  Item.FOwner := Self;
  Item.FData := ItemData;
  Item.FControl := ItemControl;
  FItemList.Add(Item);
  ItemControl.Name := Name + 'CtrlGridItem' + IntToStr(PtrInt(ItemControl));
  SetObjectProperties(ItemControl, ['GridItem', Item]);
  if Assigned(FOnItemCreate) then
    FOnItemCreate(Item);
  CallMethod(ItemControl, 'Initialize');
  ItemControl.Parent := FControl;
end;

end.

