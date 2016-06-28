unit VTObjectTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, VirtualNodeInterfaces;

type
  TObjectData = record
    Obj: IVirtualNode;
  end;

  PObjectData = ^TObjectData;

  { TVirtualObjectTree }

  TVirtualObjectTree = class (TCustomVirtualStringTree)
  private
    FNodeList: IVirtualNodeList;
    procedure SetNodeList(const AValue: IVirtualNodeList);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var AText: String); override;
    {$if VTMajorVersion > 4}
    function DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal): Boolean; override;
    {$else}
    procedure DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal); override;
    {$endif}
    procedure DoInitNode(AParent, Node: PVirtualNode;
                     var InitStates: TVirtualNodeInitStates); override;
  public
    constructor Create(AOwner: TComponent); override;
    property NodeList: IVirtualNodeList read FNodeList write SetNodeList;
  end;

implementation

{ TVirtualObjectTree }

procedure TVirtualObjectTree.SetNodeList(const AValue: IVirtualNodeList);
begin
  if FNodeList = AValue then
    Exit;
  FNodeList := AValue;
  if FNodeList = nil then
    Clear
  else
    RootNodeCount := FNodeList.GetNodeCount;
end;

procedure TVirtualObjectTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var AText: String);
var
  Data: PObjectData;
begin
  Data := GetNodeData(Node);
  AText := Data^.Obj.NodeTitle;
  inherited DoGetText(Node, Column, TextType, AText);
end;

{$if VTMajorVersion > 4}
function TVirtualObjectTree.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal): Boolean;
{$else}
procedure TVirtualObjectTree.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal);
{$endif}
var
  Data: PObjectData;
begin
  Data := GetNodeData(Node);
  NodeChildCount := Data^.Obj.ChildNodeCount;
  {$if  VTMajorVersion > 4}
  Result := True;
  {$endif}
end;

procedure TVirtualObjectTree.DoInitNode(AParent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  Data, ParentData: PObjectData;
begin
  Data := GetNodeData(Node);
  if AParent = nil then
  begin
    Data^.Obj := NodeList.GetNode(Node^.Index);
  end
  else
  begin
    ParentData := GetNodeData(AParent);
    Data^.Obj := ParentData^.Obj.GetChildNode(Node^.Index);
  end;
  if Data^.Obj.ChildNodeCount > 0 then
    Include(InitStates, ivsHasChildren);
end;

constructor TVirtualObjectTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NodeDataSize := SizeOf(TObjectData);
end;

end.

