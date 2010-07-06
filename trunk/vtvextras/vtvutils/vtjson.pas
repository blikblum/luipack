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
    FProperties: TStrings;
    FJSONObject: TJSONObject;
    FOnFormatValue: TVTJSONFormatValue;
    procedure AddNode(const PropText: String; const PropName: String;
      const PropIndex: Integer);
    function GetOptions: TJSONInspectorTreeOptions;
    procedure InitHeader;
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
  TPropertyData = record
    PropIndex: Integer;
    PropName: String;
    PropText: String;
  end;
  PPropertyData = ^TPropertyData;

{ TVirtualJSONInspector }

procedure TVirtualJSONInspector.LoadObject;
var
  PropIndex, i: Integer;
  PropName, PropText: String;

  function MatchFilters(Data: TJSONData): Boolean;
  begin
    Result := not ((jioSkipNull in TreeOptions.JSONOptions) and (Data.JSONType = jtNull))
  end;
begin
  if FJSONObject = nil then
    Clear
  else
  begin
    BeginUpdate;
    Clear;
    if FProperties.Count > 0 then
    begin
      for i := 0 to FProperties.Count - 1 do
      begin
        ExtractNameValue(FProperties[i], PropName, PropText);
        PropIndex := FJSONObject.IndexOfName(PropName);
        if (PropIndex <> -1) and MatchFilters(FJSONObject.Items[PropIndex]) then
        begin
          AddNode(PropText, PropName, PropIndex);
        end;
      end;
    end
    else
    begin
      for i := 0 to FJSONObject.Count - 1 do
      begin
        if MatchFilters(FJSONObject.Items[i]) then
        begin
          PropName := FJSONObject.Names[i];
          AddNode(PropName, PropName, i);
        end;
      end;
    end;
    EndUpdate;
  end;
end;

procedure TVirtualJSONInspector.AddNode(const PropText: String;
  const PropName: String; const PropIndex: Integer);
var
  NodeData: PPropertyData;
  Node: PVirtualNode;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData ^ .PropIndex := PropIndex;
  NodeData ^ .PropName := PropName;
  NodeData ^ .PropText := PropText;
  ValidateNode(Node, False);
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
  else
    Result := PropData.AsJSON;
  end;
end;

procedure TVirtualJSONInspector.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PPropertyData;
begin
  NodeData := GetNodeData(Node);
  NodeData^.PropName := '';
  NodeData^.PropText := '';
  inherited DoFreeNode(Node);
end;

procedure TVirtualJSONInspector.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeData: PPropertyData;
const
  PropUnknownStrs: array[0..1] of String = ('[Unknown Property]', '[Unknown Value]');
begin
  NodeData := GetNodeData(Node);
  //check for consistency since JSONObject structure could be changed between LoadObject and now
  if FJSONObject.IndexOfName(NodeData^.PropName) <> NodeData^.PropIndex then
    CellText := PropUnknownStrs[Column]
  else
  begin
    case Column of
      0:
      begin
        CellText := NodeData^.PropText;
      end;
      1:
      begin
        CellText := DoFormatValue(NodeData^.PropName, FJSONObject.Items[NodeData^.PropIndex]);
      end;
    end;
  end;
end;

function TVirtualJSONInspector.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TJSONInspectorTreeOptions;
end;

constructor TVirtualJSONInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NodeDataSize := SizeOf(TPropertyData);
  FProperties := TStringList.Create;
  InitHeader;
  with TreeOptions do
  begin
    AutoOptions := AutoOptions + [toAutoSpanColumns];
    PaintOptions := PaintOptions - [toShowTreeLines, toShowRoot] +
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

