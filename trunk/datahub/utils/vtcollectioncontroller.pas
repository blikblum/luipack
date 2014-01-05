unit VTCollectionController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VTController, VirtualTrees;

type
  TStringArray = array of String;

  TCreateCheckedCollection = procedure(var NewCollection: TCollection) of object;

  { TCollectionVirtualTreeController }

  TCollectionVirtualTreeController = class(TCustomVirtualTreeController)
  private
    FCollection: TCollection;
    FDefaultProperty: String;
    FOnCreateCheckedCollection: TCreateCheckedCollection;
    FPropertyNames: TStringArray;
    FCheckedCollection: TCollection;
    procedure LoadCheckedCollection;
    function GetCheckedCollection: TCollection;
    procedure SetOnCreateCheckedCollection(Value: TCreateCheckedCollection);
  protected
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); override;
    procedure TreeChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetCollectionItem(Node: PVirtualNode): TCollectionItem;
    procedure Load;
    property CheckedCollection: TCollection read GetCheckedCollection;
    property DefaultProperty: String read FDefaultProperty write FDefaultProperty;
    property PropertyNames: TStringArray read FPropertyNames write FPropertyNames;
    property Collection: TCollection read FCollection write FCollection;
    property OnCreateCheckedCollection: TCreateCheckedCollection read FOnCreateCheckedCollection write SetOnCreateCheckedCollection;
  end;

implementation

uses
  typinfo;

type
  TVTAccess = class(TCustomVirtualStringTree)
  end;

  TCollectionNodeData = record
    Item: TCollectionItem;
  end;
  PCollectionNodeData = ^TCollectionNodeData;

{ TCollectionVirtualTreeController }

procedure TCollectionVirtualTreeController.LoadCheckedCollection;
var
  Node: PVirtualNode;
  CheckItem, Item: TCollectionItem;
begin
  if Assigned(FOnCreateCheckedCollection) then
    FOnCreateCheckedCollection(FCheckedCollection);
  if FCheckedCollection = nil then
    FCheckedCollection := TCollection.Create(FCollection.ItemClass);
  Node := Tree.GetFirstChecked;
  while Node <> nil do
  begin
    Item := GetCollectionItem(Node);
    CheckItem := FCheckedCollection.Add;
    CheckItem.Assign(Item);
    Node := Tree.GetNextChecked(Node);
  end;
end;

function TCollectionVirtualTreeController.GetCheckedCollection: TCollection;
begin
  if FCheckedCollection = nil then
    LoadCheckedCollection;
  Result := FCheckedCollection;
end;

procedure TCollectionVirtualTreeController.SetOnCreateCheckedCollection(
  Value: TCreateCheckedCollection);
begin
  if FOnCreateCheckedCollection = Value then Exit;
  FOnCreateCheckedCollection := Value;
end;

procedure TCollectionVirtualTreeController.DoChecked(Node: PVirtualNode);
begin
  FreeAndNil(FCheckedCollection);
end;

procedure TCollectionVirtualTreeController.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Item: TCollectionItem;
  PropInfo: PPropInfo;
begin
  Item := GetCollectionItem(Node);
  if (Column > -1) and (Column < Length(FPropertyNames)) then
    PropInfo := FindPropInfo(Item, FPropertyNames[Column])
  else
    PropInfo := FindPropInfo(Item, DefaultProperty);
  if PropInfo <> nil then
    CellText := GetStrProp(Item, PropInfo);
end;

procedure TCollectionVirtualTreeController.DoInitNode(ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PCollectionNodeData;
begin
  Data := Tree.GetNodeData(Node);
  Data^.Item := FCollection.Items[Node^.Index];
  Node^.CheckType := ctCheckBox;
end;

procedure TCollectionVirtualTreeController.TreeChanged;
begin
  inherited TreeChanged;
  if Tree <> nil then
    TVTAccess(Tree).NodeDataSize := SizeOf(TCollectionNodeData);
end;

constructor TCollectionVirtualTreeController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Events := [evGetText, evInitNode, evChecked];
end;

function TCollectionVirtualTreeController.GetCollectionItem(Node: PVirtualNode): TCollectionItem;
var
  Data: PCollectionNodeData;
begin
  Data := PCollectionNodeData(Tree.GetNodeData(Node));
  if Data <> nil then
    Result := Data^.Item
  else
    Result := nil;
end;

procedure TCollectionVirtualTreeController.Load;
begin
  if (Tree = nil) or (Collection = nil) then
    Exit;
  Tree.BeginUpdate;
  Tree.Clear;
  TVTAccess(Tree).RootNodeCount := FCollection.Count;
  Tree.EndUpdate;
end;

end.

