unit VTJSONEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VTJSON, VirtualTrees, fpjson, Graphics;

type

  { TJSONQuestionTreeView }

  TJSONQuestionTreeView = class(TVirtualJSONTreeView)
  private
    FAnswerData: TJSONObject;
    FOwnsAnswerData: Boolean;
    procedure SetCheckedOption(QuestionNode: PVirtualNode; Index: Integer);
    procedure DoLoadAnswerData;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadAnswerData(AnswerData: TJSONObject; OwnsAnswerData: Boolean = False);
  published

  end;


implementation

uses
  LuiJSONUtils;

function GetOptionIndex(OptionsData: TJSONArray; AnswerData: TJSONObject; const PropName: String): Integer;
var
  PropData: TJSONData;
  FallbackIndex: Integer;
  OptionData: TJSONObject;
  OptionValueData: TJSONData;
  i: Integer;
begin
  Result := -1;
  PropData := AnswerData.Find(PropName);
  if PropData <> nil then
  begin
    FallbackIndex := -1;
    for i := 0 to OptionsData.Count - 1 do
    begin
      OptionData := OptionsData.Objects[i];
      OptionValueData := OptionData.Find('value');
      if OptionValueData <> nil then
      begin
        if CompareJSONData(OptionValueData, PropData) = 0 then
        begin
          Result := i;
          Exit;
        end;
      end
      else
      begin
        // store the matched index to be used in last case
        if (PropData.JSONType = jtNumber) and (PropData.AsInteger = i) then
          FallbackIndex := i;
      end;
    end;
    if (Result = -1) and (PropData.JSONType = jtString) then
      Result := GetJSONIndexOf(OptionsData, ['custom', True]);
    if Result = -1 then
      Result := FallbackIndex;
  end;
end;

{ TJSONQuestionTreeView }

procedure TJSONQuestionTreeView.SetCheckedOption(QuestionNode: PVirtualNode; Index: Integer);
var
  OptionNode: PVirtualNode;
begin
  OptionNode := GetFirstChild(QuestionNode);
  while OptionNode <> nil do
  begin
    if OptionNode^.Index = Index then
    begin
      OptionNode^.Dummy := 1;
      CheckState[OptionNode] := csCheckedNormal;
      OptionNode^.Dummy := 0;
      Break;
    end;
    OptionNode := GetNextSibling(OptionNode);
  end;
end;

procedure TJSONQuestionTreeView.DoLoadAnswerData;
var
  QuestionNode: PVirtualNode;
  QuestionData, OptionData, CheckData: TJSONObject;
  OptionsData: TJSONArray;
  PropData, OptionValueData: TJSONData;
  PropName: String;
  i, CheckedIndex: Integer;
begin
  if FAnswerData = nil then
  begin
    //clear
  end
  else
  begin
    QuestionNode := GetFirst;
    while QuestionNode <> nil do
    begin
      if GetData(QuestionNode, QuestionData) and FindJSONProp(QuestionData, 'children', OptionsData) then
      begin
        PropName := QuestionData.Get('prop', '');
        if QuestionData.Get('type', '') <> 'checkgroup' then
        begin
          CheckedIndex := GetOptionIndex(OptionsData, FAnswerData, PropName);
          if CheckedIndex >= 0 then
            SetCheckedOption(QuestionNode, CheckedIndex);
        end
        else
        begin
          PropData := FAnswerData.Find(PropName);
          if (PropData <> nil) and (PropData.JSONType = jtObject) then
            CheckData := TJSONObject(PropData)
          else
            CheckData := FAnswerData;
          // handle check group
          for i := 0 to OptionsData.Count - 1 do
          begin
            CheckedIndex := -1;
            OptionData := OptionsData.Objects[i];
            PropName := OptionData.Get('prop', '');
            if PropName <> '' then
            begin
              OptionValueData := OptionData.Find('value');
              if OptionValueData <> nil then
              begin
                if CompareJSONData(OptionValueData, CheckData.Find(PropName)) = 0 then
                  CheckedIndex := i;
              end
              else
              begin
                if CheckData.Get(PropName, False) then
                  CheckedIndex := i;
              end;
              if CheckedIndex >= 0 then
                SetCheckedOption(QuestionNode, CheckedIndex);
            end;
          end;
        end;
      end;
      QuestionNode := GetNextSibling(QuestionNode);
    end;
  end;
end;

procedure TJSONQuestionTreeView.DoBeforeItemErase(ACanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var AColor: TColor; var EraseAction: TItemEraseAction);
begin
  if GetNodeLevel(Node) = 0 then
   begin
     AColor := clLtGray;
     EraseAction := eaColor;
   end;
  inherited DoBeforeItemErase(ACanvas, Node, ItemRect, AColor, EraseAction);
end;

procedure TJSONQuestionTreeView.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: TJSONObject;
begin
  if GetData(Node, NodeData) then
    Allowed := NodeData.Get('custom', False);
  inherited DoCanEdit(Node, Column, Allowed);
end;

procedure TJSONQuestionTreeView.DoChecked(Node: PVirtualNode);
var
  QuestionData, OptionData, CheckData: TJSONObject;
  ValueData: TJSONData;
  OptionPropName, QuestionPropName, ValuePropName: String;
begin
  //hack to skip when check state is being programatically set
  if Node^.Dummy = 1 then
    Exit;
  if (GetNodeLevel(Node) = 1) and GetData(Node, OptionData)
    and GetData(Node^.Parent, QuestionData) then
  begin
    ValuePropName := QuestionData.Get('valueprop', 'value');
    QuestionPropName := QuestionData.Get('prop', '');
    OptionPropName := OptionData.Get('prop', QuestionPropName);
    if OptionPropName <> '' then
    begin
      ValueData := OptionData.Find(ValuePropName);
      if Node^.CheckType = ctRadioButton then
      begin
        if not OptionData.Get('custom', False) then
        begin
          if ValueData <> nil then
            FAnswerData.Elements[OptionPropName] := ValueData.Clone
          else
            FAnswerData.Integers[OptionPropName] := Node^.Index;
        end
        else
        begin
          if Node^.CheckState = csCheckedNormal then
          begin
            FAnswerData.Delete(OptionPropName);
            EditNode(Node, 0);
          end;
       end;
      end
      else
      begin
        if (QuestionPropName <> '') and (QuestionPropName <> OptionPropName) then
        begin
          if not FindJSONProp(FAnswerData, QuestionPropName, CheckData) then
          begin
            CheckData := TJSONObject.Create;
            FAnswerData.Add(QuestionPropName, CheckData);
          end;
        end
        else
          CheckData := FAnswerData;
        if Node^.CheckState = csCheckedNormal then
        begin
          if ValueData <> nil then
            CheckData.Elements[OptionPropName] := ValueData.Clone
          else
            CheckData.Booleans[OptionPropName] := True;
        end
        else
        begin
          //todo: add option to configure behavior (delete or false)
          CheckData.Delete(OptionPropName);
          if (CheckData <> FAnswerData) and (CheckData.Count = 0) then
            FAnswerData.Remove(CheckData);
        end;
      end;
    end;
  end;
  inherited DoChecked(Node);
end;

procedure TJSONQuestionTreeView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  NodeData: TJSONObject;
  ParentData: TJSONObject;
  PropName: String;
begin
  inherited DoGetText(Node, Column, TextType, CellText);
  if (GetNodeLevel(Node) > 0) and GetData(Node, NodeData)
    and NodeData.Get('custom', False) and GetData(Node^.Parent, ParentData)
    and (Node^.CheckState = csCheckedNormal) then
  begin
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    if (tsEditing in TreeStates) and (Node = FocusedNode) then
      CellText := '';
    CellText := FAnswerData.Get(PropName, CellText);
  end;
end;

procedure TJSONQuestionTreeView.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  QuestionData: TJSONObject;
begin
  inherited DoInitNode(ParentNode, Node, InitStates);
  if ParentNode = nil then
  begin
    Node^.CheckType := ctNone;
    NodeHeight[Node] := 22;
  end
  else
  begin
    if GetData(ParentNode, QuestionData) then
    begin
      if QuestionData.Get('type', '') = 'checkgroup' then
        Node^.CheckType := ctCheckBox
      else
        Node^.CheckType := ctRadioButton;
    end;
    Include(InitStates, ivsMultiline);
  end;
end;

procedure TJSONQuestionTreeView.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const AText: String);
var
  NodeData: TJSONObject;
  PropName: String;
  ParentData: TJSONObject;
begin
  if (GetNodeLevel(Node) > 0) and GetData(Node, NodeData)
    and GetData(Node^.Parent, ParentData) then
  begin
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    FAnswerData.Strings[PropName] := AText;
  end;
  inherited DoNewText(Node, Column, AText);
end;

procedure TJSONQuestionTreeView.DoPaintText(Node: PVirtualNode; const ACanvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if GetNodeLevel(Node) = 0 then
    ACanvas.Font.Style := [fsBold];
  inherited DoPaintText(Node, ACanvas, Column, TextType);
end;

constructor TJSONQuestionTreeView.Create(AOwner: TComponent);
var
  Column: TVirtualJSONDataViewColumn;
begin
  inherited Create(AOwner);
  TreeOptions.AutoOptions := DefaultAutoOptions + [toAutoSpanColumns];
  TreeOptions.MiscOptions := DefaultMiscOptions + [toCheckSupport, toEditable, toEditOnDblClick] - [toToggleOnDblClick];
  TreeOptions.PaintOptions := DefaultPaintOptions + [toAlwaysHideSelection, toHideFocusRect, toHideSelection] - [toShowTreeLines, toShowRoot];
  Column := Header.Columns.Add as TVirtualJSONDataViewColumn;
  Column.PropertyName := 'text';
  Header.Options := Header.Options + [hoAutoResize];
end;

destructor TJSONQuestionTreeView.Destroy;
begin
  if FOwnsAnswerData then
    FAnswerData.Free;
  inherited Destroy;
end;

procedure TJSONQuestionTreeView.LoadAnswerData(AnswerData: TJSONObject; OwnsAnswerData: Boolean);
begin
  if FOwnsAnswerData then
    FAnswerData.Free;
  FAnswerData := AnswerData;
  FOwnsAnswerData := OwnsAnswerData;
  DoLoadAnswerData;
end;

end.

