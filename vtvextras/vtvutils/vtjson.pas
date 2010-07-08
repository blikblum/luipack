unit VTJSON;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, fpjson;

type

  TVTJSONFormatValue = procedure (const PropName: String; PropData: TJSONData;
    var Result: String; var Handled: Boolean) of object;

  TVTJSONInspectorOption = (jioSkipNull);

  TVTJSONInspectorOptions = set of TVTJSONInspectorOption;

  TJSONInspectorTreeOptions = class(TCustomStringTreeOptions)
  private
    FJSONOptions: TVTJSONInspectorOptions;
  published
    property JSONOptions: TVTJSONInspectorOptions read FJSONOptions write FJSONOptions default [];
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
  end;

  { TVirtualJSONInspector }

  TVirtualJSONInspector = class(TCustomVirtualStringTree)
  private
    FItemDataOffset: Cardinal;
    FProperties: TStrings;
    FJSONObject: TJSONObject;
    FOnFormatValue: TVTJSONFormatValue;
    function GetItemData(Node: PVirtualNode): Pointer;
    function GetOptions: TJSONInspectorTreeOptions;
    procedure InitHeader;
    function InitJSONNode(Node: PVirtualNode; JSONData: TJSONData): Cardinal;
    procedure LoadObject;
    procedure SetProperties(const Value: TStrings);
    procedure SetJSONObject(Value: TJSONObject);
    procedure SetOptions(Value: TJSONInspectorTreeOptions);
  protected
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    function DoFormatValue(const PropName: String; PropData: TJSONData): String;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String); override;
    procedure DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reload;
    property JSONObject: TJSONObject read FJSONObject write SetJSONObject;
  published
    property Properties: TStrings read FProperties write SetProperties;
    property OnFormatValue: TVTJSONFormatValue read FOnFormatValue write FOnFormatValue;
   //inherited properties
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TJSONInspectorTreeOptions read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterGetMaxColumnWidth;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnCanSplitterResizeColumn;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    //property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    //property OnInitChildren;
    //property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollbar;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;

implementation

uses
  LuiStrUtils;

type
  TItemData = record
    DisplayText: String;
    Name: String;
    JSONData: TJSONData;
    Children: array of Integer;
  end;
  PItemData = ^TItemData;

{ TVirtualJSONInspector }

procedure TVirtualJSONInspector.LoadObject;
begin
  if FJSONObject = nil then
    Clear
  else
  begin
    BeginUpdate;
    Clear;
    RootNodeCount := InitJSONNode(RootNode, FJSONObject);
    EndUpdate;
  end;
end;

function TVirtualJSONInspector.GetItemData(Node: PVirtualNode): Pointer;
begin
  if Node = nil then
    Result := nil
  else
    Result := PByte(Node) + FItemDataOffset;
end;

function TVirtualJSONInspector.GetOptions: TJSONInspectorTreeOptions;
begin
  Result := TJSONInspectorTreeOptions(inherited TreeOptions);
end;

procedure TVirtualJSONInspector.InitHeader;
var
  Column: TVirtualTreeColumn;
begin
  Column := Header.Columns.Add;
  Column.Text := 'Property';
  Column.Width := 150;
  Column := Header.Columns.Add;
  Column.Text := 'Value';
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoVisible, hoAutoResize, hoAutoSpring];
end;

function TVirtualJSONInspector.InitJSONNode(Node: PVirtualNode;
  JSONData: TJSONData): Cardinal;
var
  ItemData: PItemData;
  ItemCount, ChildIndex, ItemIndex, i: Integer;
  PropName, PropText: String;

  function MatchFilters(Data: TJSONData): Boolean;
  begin
    Result := not ((jioSkipNull in TreeOptions.JSONOptions) and (Data.JSONType = jtNull))
  end;

begin
  ItemData := GetItemData(Node);
  ItemData^.Name := '';
  ItemData^.JSONData := JSONData;
  ItemCount := JSONData.Count;
  SetLength(ItemData^.Children, ItemCount);

  if (FProperties.Count > 0) and (JSONData.JSONType = jtObject) then
  begin
    ChildIndex := 0;
    for i := 0 to FProperties.Count - 1 do
    begin
      ExtractNameValue(FProperties[i], PropName, PropText);
      ItemIndex := TJSONObject(JSONData).IndexOfName(PropName);
      if (ItemIndex <> -1) and MatchFilters(JSONData.Items[ItemIndex]) then
      begin
        ItemData^.Children[ChildIndex] := ItemIndex;
        Inc(ChildIndex);
      end;
    end;
    SetLength(ItemData^.Children, ChildIndex);
    Result := ChildIndex;
  end
  else
  begin
    for i := 0 to ItemCount - 1 do
      ItemData^.Children[i] := i;
    Result := ItemCount;
  end;
end;

procedure TVirtualJSONInspector.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
  LoadObject;
end;

procedure TVirtualJSONInspector.SetJSONObject(Value: TJSONObject);
begin
  FJSONObject := Value;
  LoadObject;
end;

procedure TVirtualJSONInspector.SetOptions(Value: TJSONInspectorTreeOptions);
begin
  TreeOptions.Assign(Value);
end;

procedure TVirtualJSONInspector.DoCanEdit(Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 1;
end;

function TVirtualJSONInspector.DoFormatValue(const PropName: String;
  PropData: TJSONData): String;
var
  Handled: Boolean;
begin
  Handled := False;
  Result := '';
  if Assigned(FOnFormatValue) then
    FOnFormatValue(PropName, PropData, Result, Handled);
  if Handled then
    Exit;
  case PropData.JSONType of
    jtString, jtNumber, jtBoolean: Result := PropData.AsString;
    jtObject, jtArray: Result := '';
  else
    Result := PropData.AsJSON;
  end;
end;

procedure TVirtualJSONInspector.DoFreeNode(Node: PVirtualNode);
var
  ItemData: PItemData;
begin
  ItemData := GetItemData(Node);
  SetLength(ItemData^.Children, 0);
  ItemData^.Name := '';
  ItemData^.DisplayText := '';
  inherited DoFreeNode(Node);
end;

procedure TVirtualJSONInspector.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal);
var
  Data: PItemData;
begin
  Data := GetItemData(Node);
  NodeChildCount := Length(Data^.Children);
end;

procedure TVirtualJSONInspector.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  ItemData: PItemData;
begin
  ItemData := GetItemData(Node);
  case Column of
    0:
    begin
      CellText := ItemData^.DisplayText;
      if CellText = '' then
        CellText := ItemData^.Name;
    end;
    1:
    begin
      CellText := DoFormatValue(ItemData^.Name, ItemData^.JSONData);
    end;
  end;
end;

procedure TVirtualJSONInspector.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  ParentData, Data: PItemData;
  ParentJSONData: TJSONData;
  NodeChildCount: Cardinal;
  i: Integer;
begin
  ParentData := GetItemData(Node^.Parent);
  ParentJSONData := ParentData^.JSONData;
  NodeChildCount := InitJSONNode(Node, ParentJSONData.Items[Node^.Index]);
  Data := GetItemData(Node);

  case ParentJSONData.JSONType of
    jtObject:
      begin
        Data^.DisplayText := '';
        Data^.Name := TJSONObject(ParentJSONData).Names[Node^.Index];
        i := FProperties.IndexOfName(Data^.Name);
        if i <> -1 then
          Data^.DisplayText := FProperties.ValueFromIndex[i];
      end;
    jtArray:
      begin
        Data^.DisplayText := IntToStr(Node^.Index);
      end;
  end;
  if NodeChildCount > 0 then
    InitStates := InitStates + [ivsHasChildren];
end;

function TVirtualJSONInspector.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TJSONInspectorTreeOptions;
end;

constructor TVirtualJSONInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemDataOffset := AllocateInternalDataArea(SizeOf(TItemData));
  FProperties := TStringList.Create;
  FProperties.NameValueSeparator := ';';
  InitHeader;
  with TreeOptions do
  begin
    AutoOptions := AutoOptions + [toAutoSpanColumns];
    PaintOptions := PaintOptions {- [toShowTreeLines, toShowRoot]} +
      [toShowHorzGridLines, toShowVertGridLines, toPopupMode, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;
end;

destructor TVirtualJSONInspector.Destroy;
begin
  FProperties.Destroy;
  inherited Destroy;
end;

procedure TVirtualJSONInspector.Reload;
begin
  LoadObject;
end;

end.

