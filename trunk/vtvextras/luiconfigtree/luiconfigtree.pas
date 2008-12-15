unit LuiConfigTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiConfig, VirtualTrees;

type

  { TLuiConfigTree }

  TLuiConfigTree = class(TCustomVirtualStringTree, IConfigObserver)
  private
    FConfig: TLuiConfig;
    FItems: TStrings;
    FSections: TStrings;
    FVisibleSections: TStrings;
    procedure ConfigNotification(NotificationType: TLuiConfigNotificationType;
      Data: PtrInt);
    procedure InitColumns;
    procedure LoadTree;
    procedure SetConfig(const AValue: TLuiConfig);
  protected
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString); override;
    procedure DoInitChildren(Node: PVirtualNode;
      var NodeChildCount: Cardinal); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Config: TLuiConfig read FConfig write SetConfig;
    property Sections: TStrings read FSections;
  end;

implementation

type
  TConfigData = record
    Key: ShortString;
  end;
  PConfigData = ^TConfigData;

{ TLuiConfigTree }

procedure TLuiConfigTree.ConfigNotification(
  NotificationType: TLuiConfigNotificationType; Data: PtrInt);
begin
  case NotificationType of
    lcnOpen:
      LoadTree;
    lcnClose:
      Clear;
  end;
end;

procedure TLuiConfigTree.InitColumns;
var
  Column: TVirtualTreeColumn;
begin
  //todo: is necessary to set the Column text?
  Column := Header.Columns.Add;
  Column.Text := 'Key';
  Column := Header.Columns.Add;
  Column.Text := 'Data';
end;

procedure TLuiConfigTree.LoadTree;
begin
  BeginUpdate;
  Clear;
  if FSections.Count > 0 then
    FVisibleSections.Assign(FSections)
  else
    FConfig.ReadSections(FVisibleSections);
  RootNodeCount := FVisibleSections.Count;
  EndUpdate;
end;

procedure TLuiConfigTree.SetConfig(const AValue: TLuiConfig);
begin
  if FConfig = AValue then
    Exit;
  if FConfig <> nil then
    FConfig.RemoveObserver(Self);
  FConfig := AValue;
  if FConfig <> nil then
    FConfig.AddObserver(Self);
end;

function TLuiConfigTree.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex
  ): Boolean;
begin
  Result := (Node^.Parent = RootNode) and (Column = 1);
end;

procedure TLuiConfigTree.DoExpanded(Node: PVirtualNode);
begin
  ValidateChildren(Node, True);
  inherited DoExpanded(Node);
end;

procedure TLuiConfigTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data, ParentData: PConfigData;
begin
  inherited DoGetText(Node, Column, TextType, CellText);
  //the user changed the text through OnGetText
  if CellText <> '' then
    Exit;
  Data := GetNodeData(Node);
  if Node^.Parent = RootNode then
  begin
    //Section
    CellText := FConfig.GetSectionText(Data^.Key);
  end
  else
  begin
    //Item
    if Column = 0 then
      CellText := FConfig.GetItemText(Data^.Key)
    else
    begin
      ParentData := GetNodeData(Node^.Parent);
      //handle item data differently according to data type
      CellText := FConfig.ReadString(ParentData^.Key, Data^.Key);
    end;
  end;
end;

procedure TLuiConfigTree.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal);
var
  Data: PConfigData;
begin
  Data := GetNodeData(Node);
  FConfig.ReadSection(Data^.Key, FItems);
  NodeChildCount := FItems.Count;
end;

procedure TLuiConfigTree.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  Data: PConfigData;
begin
  inherited DoInitNode(ParentNode, Node, InitStates);
  Data := GetNodeData(Node);
  if ParentNode = nil then
  begin
    //Section
    InitStates := InitStates + [ivsHasChildren];
    Data^.Key := FVisibleSections[Node^.Index];
  end
  else
  begin
    //Item
    Data^.Key := FItems[Node^.Index];
  end;
end;

constructor TLuiConfigTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultText := '';
  NodeDataSize := SizeOf(TConfigData);
  FItems := TStringList.Create;
  FSections := TStringList.Create;
  FVisibleSections := TStringList.Create;
  InitColumns;
end;

destructor TLuiConfigTree.Destroy;
begin
  Config := nil; //necessary to remove from the config observer list
  FItems.Destroy;
  FSections.Destroy;
  FVisibleSections.Destroy;
  inherited Destroy;
end;

end.

