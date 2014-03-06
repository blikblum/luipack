unit CollectionVirtualTreeMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VTController, VirtualTrees;

type

  TStringArray = array of String;

  { TCollectionVirtualTreeMediator }

  TCollectionVirtualTreeMediator = class(TCustomVirtualTreeController, IFPObserver)
  private
    FCollection: TCollection;
    FDefaultProperty: String;
    FPropertyNames: TStringArray;
    procedure SetCollection(AValue: TCollection);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); override;
    procedure TreeChanged; override;
    procedure LoadCollection;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
      Data: Pointer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCollectionItem(Node: PVirtualNode): TCollectionItem;
    procedure LoadCheckedItems(CheckedCollection: TCollection);
    property DefaultProperty: String read FDefaultProperty write FDefaultProperty;
    //todo: change to a properties
    //property PropertyNames: TStringArray read FPropertyNames write FPropertyNames;
    property Collection: TCollection read FCollection write SetCollection;
  end;

implementation

uses
  typinfo;

type

  TVTAccess = class(TCustomVirtualStringTree)
  end;

  TCollectionAccess = class(TCollection)
  end;

  TCollectionNodeData = record
    Item: TCollectionItem;
  end;
  PCollectionNodeData = ^TCollectionNodeData;

{ TCollectionVirtualTreeMediator }

procedure TCollectionVirtualTreeMediator.LoadCheckedItems(
  CheckedCollection: TCollection);
var
  Node: PVirtualNode;
  CheckedItem, Item: TCollectionItem;
begin
  CheckedCollection.BeginUpdate;
  try
    Node := Tree.GetFirstChecked;
    while Node <> nil do
    begin
      Item := GetCollectionItem(Node);
      CheckedItem := CheckedCollection.Add;
      CheckedItem.Assign(Item);
      Node := Tree.GetNextChecked(Node);
    end;
  finally
    CheckedCollection.EndUpdate;
  end;
end;

procedure TCollectionVirtualTreeMediator.SetCollection(AValue: TCollection);
begin
  if FCollection = AValue then Exit;
  if FCollection <> nil then
    FCollection.FPODetachObserver(Self);
  FCollection := AValue;
  if AValue <> nil then
    AValue.FPOAttachObserver(Self);
  LoadCollection;
end;

procedure TCollectionVirtualTreeMediator.DoGetText(Node: PVirtualNode;
Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Item: TCollectionItem;
  PropInfo: PPropInfo;
begin
  Item := GetCollectionItem(Node);
  if Item = nil then
    Exit;
  if (Column > -1) and (Column < Length(FPropertyNames)) then
    PropInfo := GetPropInfo(Item, FPropertyNames[Column])
  else
    PropInfo := GetPropInfo(Item, DefaultProperty);
  if PropInfo <> nil then
    CellText := GetStrProp(Item, PropInfo)
  else
    CellText := Item.DisplayName;
end;

procedure TCollectionVirtualTreeMediator.DoInitNode(ParentNode,
Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PCollectionNodeData;
begin
  Data := Tree.GetNodeData(Node);
  Data^.Item := FCollection.Items[Node^.Index];
  Node^.CheckType := ctCheckBox;
end;

procedure TCollectionVirtualTreeMediator.TreeChanged;
begin
  inherited TreeChanged;
  if Tree <> nil then
    TVTAccess(Tree).NodeDataSize := SizeOf(TCollectionNodeData);
  LoadCollection;
end;

constructor TCollectionVirtualTreeMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Events := [evGetText, evInitNode];
end;

destructor TCollectionVirtualTreeMediator.Destroy;
begin
  if FCollection <> nil then
    FCollection.FPODetachObserver(Self);
  inherited Destroy;
end;

function TCollectionVirtualTreeMediator.GetCollectionItem(Node: PVirtualNode): TCollectionItem;
var
  Data: PCollectionNodeData;
begin
  Data := PCollectionNodeData(Tree.GetNodeData(Node));
  if Data <> nil then
    Result := Data^.Item
  else
    Result := nil;
end;

procedure TCollectionVirtualTreeMediator.LoadCollection;
begin
  if Tree = nil then
    Exit;
  Tree.BeginUpdate;
  Tree.Clear;
  if FCollection <> nil then
    TVTAccess(Tree).RootNodeCount := FCollection.Count;
  Tree.EndUpdate;
end;

procedure TCollectionVirtualTreeMediator.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  //todo: report TCollection events being sent between an update
  if TCollectionAccess(FCollection).UpdateCount > 0 then
    Exit;
  case Operation of
    ooChange, ooAddItem, ooDeleteItem:
      LoadCollection;
    ooFree:
      Collection := nil;
  end;
end;

end.

