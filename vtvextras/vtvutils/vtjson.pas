unit VTJSON;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, fpjson, Controls;

type

  TVirtualJSONInspector = class;

  TVTJSONInspectorFormatValue = procedure (Sender: TVirtualJSONInspector; const PropName: String;
    PropData: TJSONData; var DisplayText: String) of object;

  TVTJSONInspectorFormatName = procedure (Sender: TVirtualJSONInspector;
    ParentData: TJSONData; ItemIndex: Integer; var DisplayName: String) of object;

  TVTJSONInspectorOption = (jioSkipNullProperties, jioSkipUnknownProperties);

  TVTJSONInspectorOptions = set of TVTJSONInspectorOption;

  { TJSONInspectorTreeOptions }

  TJSONInspectorTreeOptions = class(TCustomStringTreeOptions)
  private
    FJSONOptions: TVTJSONInspectorOptions;
    procedure SetJSONOptions(const Value: TVTJSONInspectorOptions);
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property JSONOptions: TVTJSONInspectorOptions read FJSONOptions write SetJSONOptions default [];
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
  end;

  { TJSONPropertyDef }

  TJSONPropertyDef = class(TCollectionItem)
  private
    FDisplayName: String;
    FName: String;
  protected
    function GetDisplayName: String; override;
    procedure SetDisplayName(const Value: String); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayName;
    property Name: String read FName write FName;
  end;

  { TJSONPropertyDefs }

  TJSONPropertyDefs = class(TCollection)
  private
    FOwner: TVirtualJSONInspector;
    function GetItem(Index: Integer): TJSONPropertyDef;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TVirtualJSONInspector);
    function Add: TJSONPropertyDef;
    function Add(const Name, DisplayName: String): TJSONPropertyDef;
    function Find(const Name: String): TJSONPropertyDef;
    property Items[Index: Integer]: TJSONPropertyDef read GetItem; default;
  end;

  { TVirtualJSONInspector }

  TVirtualJSONInspector = class(TCustomVirtualStringTree)
  private
    FItemDataOffset: Cardinal;
    FOnFormatName: TVTJSONInspectorFormatName;
    FRootData: TJSONData;
    FOnFormatValue: TVTJSONInspectorFormatValue;
    FPropertyDefs: TJSONPropertyDefs;
    function GetItemData(Node: PVirtualNode): Pointer;
    function GetOptions: TJSONInspectorTreeOptions;
    procedure InitHeader;
    function InitJSONNode(Node: PVirtualNode; JSONData: TJSONData): Cardinal;
    procedure LoadObject;
    function MatchFilters(ParentJSONData, JSONData: TJSONData; Index: Integer): Boolean;
    procedure SetRootData(Value: TJSONData);
    procedure SetOptions(Value: TJSONInspectorTreeOptions);
    procedure SetPropertyDefs(const Value: TJSONPropertyDefs);
  protected
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
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
    function GetData(Node: PVirtualNode): TJSONData;
    procedure Reload;
    property RootData: TJSONData read FRootData write SetRootData;
  published
    property PropertyDefs: TJSONPropertyDefs read FPropertyDefs write SetPropertyDefs;
    property OnFormatName: TVTJSONInspectorFormatName read FOnFormatName write FOnFormatName;
    property OnFormatValue: TVTJSONInspectorFormatValue read FOnFormatValue write FOnFormatValue;
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
    property BorderStyle default bsSingle;
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
    //property DefaultText;
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

  TCustomVirtualJSONDataView = class;

  TVTJSONDataViewFormatValue = procedure (Sender: TCustomVirtualJSONDataView; Column: TColumnIndex;
    Data: TJSONData; var DisplayText: String) of object;

  TVTJSONDataViewGetText = procedure(Sender: TCustomVirtualJSONDataView; Node: PVirtualNode; NodeData: TJSONData;
    Column: TColumnIndex; TextType: TVSTTextType; var CellText: String) of object;

  { TVirtualJSONDataViewColumn }

  TVirtualJSONDataViewColumn = class(TVirtualTreeColumn)
  private
    FPropertyName: String;
    procedure SetPropertyName(const Value: String);
  published
    property PropertyName: String read FPropertyName write SetPropertyName;
  end;

  { TCustomVirtualJSONDataView }

  TCustomVirtualJSONDataView = class(TCustomVirtualStringTree)
  private
    FCheckedData: TJSONArray;
    FData: TJSONData;
    FOnGetText: TVTJSONDataViewGetText;
    FTextProperty: String;
    FItemDataOffset: Integer;
    function GetCheckedData: TJSONArray;
    function GetItemData(Node: PVirtualNode): Pointer;
    function GetOptions: TStringTreeOptions;
    procedure LoadCheckedData;
    procedure SetData(const Value: TJSONData);
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure SetTextProperty(const Value: String);
  protected
    procedure DoChecked(Node: PVirtualNode); override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    property OnGetText: TVTJSONDataViewGetText read FOnGetText write FOnGetText;
    property TextProperty: String read FTextProperty write SetTextProperty;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
  public
    destructor Destroy; override;
    function GetData(Node: PVirtualNode): TJSONData;
    function GetData(Node: PVirtualNode; out NodeData: TJSONObject): Boolean;
    function GetNode(Data: TJSONData): PVirtualNode;
    procedure LoadData;
    property CheckedData: TJSONArray read GetCheckedData;
    property Data: TJSONData read FData write SetData;
  end;

  TVirtualJSONListView = class(TCustomVirtualJSONDataView)
  private
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnGetText;
    property TextProperty;
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
    property BorderStyle default bsSingle;
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
    //property DefaultText;
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
    property TreeOptions;
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
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
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
    property OnHeaderCheckBoxClick;
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
    property OnInitChildren;
    property OnInitNode;
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

  { TVirtualJSONTreeView }

  TVirtualJSONTreeView = class(TCustomVirtualJSONDataView)
  private
    function GetJSONData(ParentNode, Node: PVirtualNode): TJSONData;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnGetText;
    property TextProperty;
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
    property BorderStyle default bsSingle;
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
    //property DefaultText;
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
    property TreeOptions;
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
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
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
    property OnHeaderCheckBoxClick;
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
    property OnInitChildren;
    property OnInitNode;
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

type
  TItemData = record
    DisplayName: String;
    Name: String;
    JSONData: TJSONData;
    Children: array of Integer;
  end;
  PItemData = ^TItemData;

  //warning: keep JSONData as first field in TLVItemData and TTVItemData since it's
  // retrieved in TCustomVirtualJSONDataView.GetData both for the list and the tree
  TLVItemData = record
    JSONData: TJSONData;
  end;
  PLVItemData = ^TLVItemData;

  TTVItemData = record
    JSONData: TJSONData;
    ChildrenData: TJSONArray;
  end;
  PTVItemData = ^TTVItemData;

{ TVirtualJSONListView }

procedure TVirtualJSONListView.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  ItemData: PLVItemData;
  JSONData, TextPropData: TJSONData;
  JSONObject: TJSONObject absolute JSONData;
  TextPropIndex: Integer;
begin
  ItemData := GetItemData(Node);
  JSONData := ItemData^.JSONData;
  if JSONData.JSONType = jtObject then
  begin
    //todo: cache TextPropIndex ??
    //todo: add option to ignore case
    if Header.UseColumns then
    begin
      if Column <> NoColumn then
        TextPropIndex := JSONObject.IndexOfName(TVirtualJSONDataViewColumn(Header.Columns.Items[Column]).PropertyName)
      else
        TextPropIndex := -1;
    end
    else
      TextPropIndex := JSONObject.IndexOfName(FTextProperty);
    if TextPropIndex <> -1 then
    begin
      TextPropData := JSONObject.Items[TextPropIndex];
      //todo: add display options for null value
      if TextPropData.JSONType in [jtString, jtNumber, jtBoolean] then
        CellText := JSONObject.Items[TextPropIndex].AsString;
    end;
  end;
  if Assigned(FOnGetText) then
    FOnGetText(Self, Node, JSONData, Column, TextType, CellText);
end;

procedure TVirtualJSONListView.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  ItemData: PLVItemData;
begin
  ItemData := GetItemData(Node);
  ItemData^.JSONData := FData.Items[Node^.Index];
  Node^.CheckType := ctCheckBox;
  inherited DoInitNode(ParentNode, Node, InitStates);
end;

constructor TVirtualJSONListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultText := '';
  FTextProperty := 'text';
  FItemDataOffset := AllocateInternalDataArea(SizeOf(TLVItemData));
  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toShowHorzGridLines, toShowVertGridLines, toPopupMode, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toFullRowSelect];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;
end;


{ TVirtualJSONTreeView }

function TVirtualJSONTreeView.GetJSONData(ParentNode, Node: PVirtualNode): TJSONData;
var
  ParentItemData: PTVItemData;
  ParentChildrenData: TJSONArray;
begin
  if ParentNode = nil then
    //todo: don't assign FData directly when add support for FData = object
    ParentChildrenData := FData as TJSONArray
  else
  begin
    ParentItemData := GetItemData(ParentNode);
    ParentChildrenData := ParentItemData^.ChildrenData;
  end;
  Result := ParentChildrenData.Items[Node^.Index];
end;

procedure TVirtualJSONTreeView.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  ItemData: PTVItemData;
  JSONData, TextPropData: TJSONData;
  JSONObject: TJSONObject absolute JSONData;
  TextPropIndex: Integer;
begin
  ItemData := GetItemData(Node);
  JSONData := ItemData^.JSONData;
  if JSONData.JSONType = jtObject then
  begin
    if not Header.UseColumns then
      TextPropIndex := JSONObject.IndexOfName(FTextProperty)
    else
    begin
      if Column <> NoColumn then
        TextPropIndex := JSONObject.IndexOfName(TVirtualJSONDataViewColumn(Header.Columns.Items[Column]).PropertyName)
      else
        TextPropIndex := -1;
    end;

    if TextPropIndex <> -1 then
    begin
      TextPropData := JSONObject.Items[TextPropIndex];
      if TextPropData.JSONType in [jtString, jtNumber, jtBoolean] then
        CellText := TextPropData.AsString;
    end;
  end;
  if Assigned(FOnGetText) then
    FOnGetText(Self, Node, JSONData, Column, TextType, CellText);
end;

procedure TVirtualJSONTreeView.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal);
var
  ItemData: PTVItemData;
begin
  ItemData := GetItemData(Node);
  NodeChildCount := ItemData^.ChildrenData.Count;
end;

procedure TVirtualJSONTreeView.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  ItemData: PTVItemData;
  JSONData: TJSONData;
  JSONObject: TJSONObject absolute JSONData;
  ChildrenIndex: Integer;
begin
  JSONData := GetJSONData(ParentNode, Node);
  ItemData := GetItemData(Node);
  ItemData^.JSONData := JSONData;
  //todo: handle non JSONObject
  if JSONData.JSONType = jtObject then
  begin
    ChildrenIndex := JSONObject.IndexOfName('children');
    if ChildrenIndex <> -1 then
    begin
      ItemData^.ChildrenData := JSONObject.Items[ChildrenIndex] as TJSONArray;
      if ItemData^.ChildrenData.Count > 0 then
        InitStates := InitStates + [ivsHasChildren];
    end
    else
      ItemData^.ChildrenData := nil;
  end
  else
    raise Exception.Create('JSONObject expected');
  Node^.CheckType := ctCheckBox;
  inherited DoInitNode(ParentNode, Node, InitStates);
end;

constructor TVirtualJSONTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultText := '';
  FTextProperty := 'text';
  FItemDataOffset := AllocateInternalDataArea(SizeOf(TTVItemData));
  with TreeOptions do
  begin
    //PaintOptions := PaintOptions +
    //  [toShowHorzGridLines, toShowVertGridLines, toPopupMode, toHideFocusRect];
    //SelectionOptions := SelectionOptions + [toExtendedFocus, toFullRowSelect];
    //MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;

end;

{ TVirtualJSONDataViewColumn }

procedure TVirtualJSONDataViewColumn.SetPropertyName(const Value: String);
begin
  if FPropertyName = Value then exit;
  if (Text = '') or (Text = FPropertyName) then
    Text := Value;
  FPropertyName := Value;
end;


{ TCustomVirtualJSONDataView }

function TCustomVirtualJSONDataView.GetItemData(Node: PVirtualNode): Pointer;
begin
  if Node = nil then
    Result := nil
  else
    Result := PByte(Node) + FItemDataOffset;
end;

function TCustomVirtualJSONDataView.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TCustomVirtualJSONDataView.LoadCheckedData;
var
  Node: PVirtualNode;
  ItemData: PLVItemData;
begin
  FCheckedData := TJSONArray.Create;
  Node := GetFirstChecked;
  while Node <> nil do
  begin
    ItemData := GetItemData(Node);
    FCheckedData.Add(ItemData^.JSONData.Clone);
    Node := GetNextChecked(Node);
  end;
end;

procedure TCustomVirtualJSONDataView.LoadData;
begin
  if FData = nil then
    Clear
  else
  begin
    //todo handle object
    if FData.JSONType = jtArray then
    begin
      BeginUpdate;
      Clear;
      RootNodeCount := FData.Count;
      EndUpdate;
    end;
  end;
end;

function TCustomVirtualJSONDataView.GetCheckedData: TJSONArray;
begin
  if FCheckedData = nil then
    LoadCheckedData;
  Result := FCheckedData;
end;

procedure TCustomVirtualJSONDataView.SetData(const Value: TJSONData);
begin
  if FData = Value then exit;
  FData := Value;
end;

procedure TCustomVirtualJSONDataView.SetOptions(const Value: TStringTreeOptions);
begin
  TreeOptions.Assign(Value);
end;

procedure TCustomVirtualJSONDataView.SetTextProperty(const Value: String);
begin
  if FTextProperty = Value then Exit;
  FTextProperty := Value;
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    //todo: check for UseColumns?
    MainColumnChanged;
    Invalidate;
  end;
end;

procedure TCustomVirtualJSONDataView.DoChecked(Node: PVirtualNode);
begin
  FreeAndNil(FCheckedData);
  inherited DoChecked(Node);
end;

function TCustomVirtualJSONDataView.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TVirtualJSONDataViewColumn;
end;

function TCustomVirtualJSONDataView.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

destructor TCustomVirtualJSONDataView.Destroy;
begin
  FCheckedData.Free;
  inherited Destroy;
end;

function TCustomVirtualJSONDataView.GetNode(Data: TJSONData): PVirtualNode;
begin
  Result := GetFirst;
  while Result <> nil do
  begin
    if GetData(Result) = Data then
      Exit;
    Result := GetNext(Result);
  end;
  Result := nil;
end;

function TCustomVirtualJSONDataView.GetData(Node: PVirtualNode): TJSONData;
var
  ItemData: PLVItemData;
begin
  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    Result := ItemData^.JSONData;
  end
  else
    Result := nil;
end;

function TCustomVirtualJSONDataView.GetData(Node: PVirtualNode; out NodeData: TJSONObject): Boolean;
var
  ItemData: PLVItemData;
  ItemJSONData: TJSONData absolute NodeData;
begin
  Result := Node <> nil;
  if Result then
  begin
    ItemData := GetItemData(Node);
    ItemJSONData := ItemData^.JSONData;
    Result := (ItemJSONData <> nil) and (ItemJSONData.JSONType = jtObject);
  end;
  if not Result then
    NodeData := nil;
end;

{ TVirtualJSONInspector }

procedure TVirtualJSONInspector.LoadObject;
begin
  if FRootData = nil then
    Clear
  else
  begin
    BeginUpdate;
    Clear;
    RootNodeCount := InitJSONNode(RootNode, FRootData);
    EndUpdate;
  end;
end;

function TVirtualJSONInspector.MatchFilters(ParentJSONData, JSONData: TJSONData; Index: Integer): Boolean;
var
  PropName: String;
begin
  Result := True;
  if ParentJSONData.JSONType = jtObject then
  begin
    Result := not ((jioSkipNullProperties in TreeOptions.FJSONOptions) and (JSONData.JSONType = jtNull));
    if Result and (jioSkipUnknownProperties in TreeOptions.FJSONOptions) then
    begin
      PropName := TJSONObject(ParentJSONData).Names[Index];
      Result := FPropertyDefs.Find(PropName) <> nil;
    end;
  end;
  //todo: add OnFilterXXX
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
  ItemCount, ChildIndex, i: Integer;
begin
  ItemData := GetItemData(Node);
  ItemData^.Name := '';
  ItemData^.JSONData := JSONData;
  //parse children
  ItemCount := JSONData.Count;
  SetLength(ItemData^.Children, ItemCount);
  ChildIndex := 0;
  for i := 0 to ItemCount - 1 do
  begin
    if MatchFilters(JSONData, JSONData.Items[i], i) then
    begin
      ItemData^.Children[ChildIndex] := i;
      Inc(ChildIndex);
    end;
  end;
  SetLength(ItemData^.Children, ChildIndex);
  Result := ChildIndex;
end;

procedure TVirtualJSONInspector.SetRootData(Value: TJSONData);
begin
  FRootData := Value;
  LoadObject;
end;

procedure TVirtualJSONInspector.SetOptions(Value: TJSONInspectorTreeOptions);
begin
  TreeOptions.Assign(Value);
end;

procedure TVirtualJSONInspector.SetPropertyDefs(const Value: TJSONPropertyDefs);
begin
  FPropertyDefs.Assign(Value);
end;

function TVirtualJSONInspector.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  ParentItemData, ItemData: PItemData;
begin
  //todo: cache this info?
  Result := (Column = 1);
  if Result then
  begin
    ParentItemData := GetItemData(Node^.Parent);
    Result := (ParentItemData^.JSONData.JSONType in [jtArray, jtObject]);
    if Result then
    begin
      ItemData := GetItemData(Node);
      Result := (ItemData^.JSONData.JSONType in [jtArray, jtObject]);
    end;
  end;
end;

procedure TVirtualJSONInspector.DoCanEdit(Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 1;
end;

function TVirtualJSONInspector.DoFormatValue(const PropName: String;
  PropData: TJSONData): String;
begin
  case PropData.JSONType of
    jtString, jtNumber, jtBoolean: Result := PropData.AsString;
    jtObject, jtArray: Result := '';
  else
    Result := PropData.AsJSON;
  end;
  //todo: move OnFormatValue to PropertyDefs??
  if Assigned(FOnFormatValue) then
    FOnFormatValue(Self, PropName, PropData, Result);
end;

procedure TVirtualJSONInspector.DoFreeNode(Node: PVirtualNode);
var
  ItemData: PItemData;
begin
  ItemData := GetItemData(Node);
  SetLength(ItemData^.Children, 0);
  ItemData^.Name := '';
  ItemData^.DisplayName := '';
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
      CellText := ItemData^.DisplayName;
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
  ParentItemData, ItemData: PItemData;
  ItemIndex: Integer;
  ParentJSONData: TJSONData;
  NodeChildCount: Cardinal;
  PropertyDef: TJSONPropertyDef;
begin
  ParentItemData := GetItemData(Node^.Parent);
  ParentJSONData := ParentItemData^.JSONData;
  ItemIndex := ParentItemData^.Children[Node^.Index];
  NodeChildCount := InitJSONNode(Node, ParentJSONData.Items[ItemIndex]);
  ItemData := GetItemData(Node);

  ItemData^.DisplayName := '';
  if ParentJSONData.JSONType = jtObject then
  begin
    ItemData^.Name := TJSONObject(ParentJSONData).Names[ItemIndex];
    PropertyDef := FPropertyDefs.Find(ItemData^.Name);
    if PropertyDef <> nil then
      ItemData^.DisplayName := PropertyDef.DisplayName;
  end;

  if Assigned(FOnFormatName) then
    FOnFormatName(Self, ParentJSONData, ItemIndex, ItemData^.DisplayName);

  //if is array and DisplayText not set then use default value
  if (ParentJSONData.JSONType = jtArray) and (ItemData^.DisplayName = '') then
    ItemData^.DisplayName := IntToStr(ItemIndex);

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
  FPropertyDefs := TJSONPropertyDefs.Create(Self);
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
var
  ItemData: PItemData;
begin
  ItemData := GetItemData(RootNode);
  SetLength(ItemData^.Children, 0);
  FPropertyDefs.Destroy;
  inherited Destroy;
end;

function TVirtualJSONInspector.GetData(Node: PVirtualNode): TJSONData;
var
  ItemData: PItemData;
begin
  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    Result := ItemData^.JSONData;
  end
  else
    Result := nil;
end;

procedure TVirtualJSONInspector.Reload;
begin
  LoadObject;
end;

{ TJSONPropertyDef }

function TJSONPropertyDef.GetDisplayName: String;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := FName;
end;

procedure TJSONPropertyDef.SetDisplayName(const Value: String);
begin
  FDisplayName := Value;
end;

procedure TJSONPropertyDef.Assign(Source: TPersistent);
begin
  if Source is TJSONPropertyDef then
  begin
    FDisplayName := TJSONPropertyDef(Source).FDisplayName;
    FName := TJSONPropertyDef(Source).FName;
  end
  else
    inherited Assign(Source);
end;

{ TJSONPropertyDefs }

function TJSONPropertyDefs.GetItem(Index: Integer): TJSONPropertyDef;
begin
  Result := TJSONPropertyDef(inherited GetItem(Index));
end;

function TJSONPropertyDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TJSONPropertyDefs.Create(AOwner: TVirtualJSONInspector);
begin
  inherited Create(TJSONPropertyDef);
  FOwner := AOwner;
end;

function TJSONPropertyDefs.Add: TJSONPropertyDef;
begin
  Result := TJSONPropertyDef(inherited Add);
end;

function TJSONPropertyDefs.Add(const Name, DisplayName: String): TJSONPropertyDef;
begin
  Result := TJSONPropertyDef(inherited Add);
  Result.FName := Name;
  Result.FDisplayName := DisplayName;
end;

function TJSONPropertyDefs.Find(const Name: String): TJSONPropertyDef;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TJSONPropertyDef(inherited GetItem(i));
    if Name = Result.Name then
      Exit;
  end;
  Result := nil;
end;

{ TJSONInspectorTreeOptions }

procedure TJSONInspectorTreeOptions.SetJSONOptions(const Value: TVTJSONInspectorOptions);
begin
  if Value = FJSONOptions then
    Exit;
  FJSONOptions := Value;
  if not (csLoading in Owner.ComponentState) then
    TVirtualJSONInspector(Owner).Reload;
end;

procedure TJSONInspectorTreeOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TJSONInspectorTreeOptions then
  begin
    with TJSONInspectorTreeOptions(Dest) do
      JSONOptions := Self.JSONOptions;
  end;
  inherited AssignTo(Dest);
end;

end.

