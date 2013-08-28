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

{ TJSONQuestionTreeView }

procedure TJSONQuestionTreeView.DoLoadAnswerData;
var
  QuestionNode, OptionNode: PVirtualNode;
  QuestionData, OptionData: TJSONObject;
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
      if GetData(QuestionNode, QuestionData) then
      begin
        PropName := QuestionData.Get('prop', '');
        PropData := FAnswerData.Find(PropName);
        if PropData <> nil then
        begin
          if FindJSONProp(QuestionData, 'children', OptionsData) then
          begin
            if PropData.JSONType = jtString then
            begin
              CheckedIndex := GetJSONIndexOf(OptionsData, ['custom', True]);
            end
            else
            begin
              CheckedIndex := -1;
              for i := 0 to OptionsData.Count - 1 do
              begin
                OptionData := OptionsData.Objects[i];
                OptionValueData := OptionData.Find('value');
                if OptionValueData <> nil then
                begin
                  if CompareJSONData(OptionValueData, PropData) = 0 then
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
            OptionNode := GetFirstChild(QuestionNode);
            while OptionNode <> nil do
            begin
              if OptionNode^.Index = CheckedIndex then
              begin
                OptionNode^.Dummy := 1;
                CheckState[OptionNode] := csCheckedNormal;
                OptionNode^.Dummy := 0;
                Break;
              end;
              OptionNode := GetNextSibling(OptionNode);
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
  QuestionData, OptionData: TJSONObject;
  ValueData: TJSONData;
  PropName: String;
begin
  //hack to skip when check state is being programatically set
  if Node^.Dummy = 1 then
    Exit;
  if (GetNodeLevel(Node) = 1) and GetData(Node, OptionData)
    and GetData(Node^.Parent, QuestionData) then
  begin
    PropName := QuestionData.Get('prop', '');
    PropName := OptionData.Get('prop', PropName);
    if PropName <> '' then
    begin
      ValueData := OptionData.Find('value');
      if Node^.CheckType = ctRadioButton then
      begin
        if not OptionData.Get('custom', False) then
        begin
          if ValueData <> nil then
            FAnswerData.Elements[PropName] := ValueData.Clone
          else
            FAnswerData.Integers[PropName] := Node^.Index;
        end
        else
        begin
          if Node^.CheckState = csCheckedNormal then
            EditNode(Node, 0);
       end;
      end
      else
      begin
        if Node^.CheckState = csCheckedNormal then
        begin
          if ValueData <> nil then
            FAnswerData.Elements[PropName] := ValueData.Clone
          else
            FAnswerData.Booleans[PropName] := True;
        end
        else
          //todo: add option to configure behavior (delete or false)
          FAnswerData.Delete(PropName);
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
    and NodeData.Get('custom', False) and GetData(Node^.Parent, ParentData) then
  begin
    PropName := ParentData.Get('prop', '');
    PropName := NodeData.Get('prop', PropName);
    CellText := FAnswerData.Get(PropName, CellText);
  end;
end;

procedure TJSONQuestionTreeView.DoInitNode(ParentNode, Node: PVirtualNode;
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
    if GetData(Node, NodeData) then
    begin
      if NodeData.Get('check', False) then
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

