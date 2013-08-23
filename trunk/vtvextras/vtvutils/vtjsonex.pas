unit VTJSONEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VTJSON, VirtualTrees, fpjson, Graphics;

type

  { TVirtualJSONOptionTree }

  TVirtualJSONOptionTree = class(TVirtualJSONTreeView)
  private
    FOptionData: TJSONObject;
    FOwnsOptionData: Boolean;
    procedure DoLoadOptionData;
  protected
    procedure DoBeforeItemErase(ACanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var AColor: TColor; var EraseAction: TItemEraseAction); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String); override;
    procedure DoPaintText(Node: PVirtualNode; const ACanvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); override;
  public
    destructor Destroy; override;
    procedure LoadOptionData(OptionData: TJSONObject; OwnsOptionData: Boolean = False);
  published

  end;


implementation

uses
  LuiJSONUtils;

{ TVirtualJSONOptionTree }

procedure TVirtualJSONOptionTree.DoLoadOptionData;
var
  ItemNode, ItemOptionNode: PVirtualNode;
  ItemData, ItemOptionData: TJSONObject;
  PropName: String;
  i, CheckedIndex: Integer;
  PropData, ItemOptionValueData: TJSONData;
  ChildrenData: TJSONArray;
begin
  if FOptionData = nil then
  begin
    //clear
  end
  else
  begin
    ItemNode := GetFirst;
    while ItemNode <> nil do
    begin
      ItemData := GetData(ItemNode) as TJSONObject;
      PropName := ItemData.Get('prop', '');
      PropData := FOptionData.Find(PropName);
      if PropData <> nil then
      begin
        if FindJSONProp(ItemData, 'children', ChildrenData) then
        begin
          if PropData.JSONType = jtString then
          begin
            CheckedIndex := GetJSONIndexOf(ChildrenData, ['custom', True]);
          end
          else
          begin
            CheckedIndex := -1;
            for i := 0 to ChildrenData.Count - 1 do
            begin
              ItemOptionData := ChildrenData.Objects[i];
              ItemOptionValueData := ItemOptionData.Find('value');
              if ItemOptionValueData <> nil then
              begin
                if CompareJSONData(ItemOptionValueData, PropData) = 0 then
                begin
                  CheckedIndex := i;
                  Break;
                end;
              end
              else
              begin
                if (PropData.JSONType = jtNumber) and (PropData.AsInteger = i) then
                begin
                  CheckedIndex := i;
                  Break;
                end;
              end;
            end;
          end;
          ItemOptionNode := GetFirstChild(ItemNode);
          while ItemOptionNode <> nil do
          begin
            if ItemOptionNode^.Index = CheckedIndex then
            begin
              ItemOptionNode^.Dummy := 1;
              CheckState[ItemOptionNode] := csCheckedNormal;
              ItemOptionNode^.Dummy := 0;
              Break;
            end;
            ItemOptionNode := GetNextSibling(ItemOptionNode);
          end;
        end;
      end;
      ItemNode := GetNextSibling(ItemNode);
    end;
  end;
end;

procedure TVirtualJSONOptionTree.DoBeforeItemErase(ACanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var AColor: TColor; var EraseAction: TItemEraseAction);
begin
  if GetNodeLevel(Node) = 0 then
   begin
     AColor := clLtGray;
     EraseAction := eaColor;
   end;
  inherited DoBeforeItemErase(ACanvas, Node, ItemRect, AColor, EraseAction);
end;

procedure TVirtualJSONOptionTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: TJSONObject;
begin
  NodeData := GetData(Node) as TJSONObject;
  Allowed := NodeData.Get('custom', False);
  inherited DoCanEdit(Node, Column, Allowed);
end;

procedure TVirtualJSONOptionTree.DoChecked(Node: PVirtualNode);
var
  ParentData, NodeData: TJSONObject;
  PropName: String;
begin
  //hack to skip when check state is being programatically set
  if Node^.Dummy = 1 then
    Exit;
  if GetNodeLevel(Node) = 1 then
  begin
    NodeData := GetData(Node) as TJSONObject;
    ParentData := GetData(NodeParent[Node]) as TJSONObject;
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    if PropName <> '' then
    begin
      if Node^.CheckType = ctRadioButton then
      begin
        if not NodeData.Get('custom', False) then
          FOptionData.Integers[PropName] := Node^.Index
        else
        begin
          if Node^.CheckState = csCheckedNormal then
            EditNode(Node, 0);
       end;
      end
      else
      begin
        //todo set value instead of index
        if Node^.CheckState = csCheckedNormal then
          FOptionData.Integers[PropName] := Node^.Index
        else
          FOptionData.Delete(PropName);
      end;
    end;
  end;
  inherited DoChecked(Node);
end;

procedure TVirtualJSONOptionTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  NodeData: TJSONObject;
  ParentData: TJSONObject;
  PropName: String;
begin
  inherited DoGetText(Node, Column, TextType, CellText);
  NodeData := GetData(Node) as TJSONObject;
  if (GetNodeLevel(Node) > 0) and NodeData.Get('custom', False) then
  begin
    ParentData := GetData(Node^.Parent) as TJSONObject;
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    CellText := FOptionData.Get(PropName, CellText);
  end;
end;

procedure TVirtualJSONOptionTree.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  NodeData: TJSONObject;
begin
  inherited DoInitNode(ParentNode, Node, InitStates);
  if ParentNode = nil then
  begin
    Node^.CheckType := ctNone;
    NodeHeight[Node] := 22;
  end
  else
  begin
    NodeData := GetData(Node) as TJSONObject;
    if NodeData.Get('check', False) then
      Node^.CheckType := ctCheckBox
    else
      Node^.CheckType := ctRadioButton;
    Include(InitStates, ivsMultiline);
  end;
end;

procedure TVirtualJSONOptionTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const AText: String);
var
  NodeData: TJSONObject;
  PropName: String;
  ParentData: TJSONObject;
begin
  if GetNodeLevel(Node) > 0 then
  begin
    ParentData := GetData(Node^.Parent) as TJSONObject;
    NodeData := GetData(Node) as TJSONObject;
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    FOptionData.Strings[PropName] := AText;
  end;
  inherited DoNewText(Node, Column, AText);
end;

procedure TVirtualJSONOptionTree.DoPaintText(Node: PVirtualNode; const ACanvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if GetNodeLevel(Node) = 0 then
    ACanvas.Font.Style := [fsBold];
  inherited DoPaintText(Node, ACanvas, Column, TextType);
end;

destructor TVirtualJSONOptionTree.Destroy;
begin
  if FOwnsOptionData then
    FOptionData.Free;
  inherited Destroy;
end;

procedure TVirtualJSONOptionTree.LoadOptionData(OptionData: TJSONObject; OwnsOptionData: Boolean);
begin
  if FOwnsOptionData then
    FOptionData.Free;
  FOptionData := OptionData;
  FOwnsOptionData := OwnsOptionData;
end;

end.

