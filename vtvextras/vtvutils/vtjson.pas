unit VTJSON;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, fpjson;

type

  TVirtualJSONInspector = class;

  TVTJSONFormatValue = procedure (const PropName: String; PropData: TJSONData;
    var Result: String; var Handled: Boolean) of object;

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
    FRootData: TJSONData;
    FOnFormatValue: TVTJSONFormatValue;
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
    property RootData: TJSONData read FRootData write SetRootData;
  published
    property PropertyDefs: TJSONPropertyDefs read FPropertyDefs write SetPropertyDefs;
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
  ItemIndex: Integer;
  ParentJSONData: TJSONData;
  NodeChildCount: Cardinal;
  PropertyDef: TJSONPropertyDef;
begin
  ParentData := GetItemData(Node^.Parent);
  ParentJSONData := ParentData^.JSONData;
  ItemIndex := ParentData^.Children[Node^.Index];
  NodeChildCount := InitJSONNode(Node, ParentJSONData.Items[ItemIndex]);
  Data := GetItemData(Node);

  case ParentJSONData.JSONType of
    jtObject:
      begin
        Data^.DisplayText := '';
        Data^.Name := TJSONObject(ParentJSONData).Names[ItemIndex];
        PropertyDef := FPropertyDefs.Find(Data^.Name);
        if PropertyDef <> nil then
          Data^.DisplayText := PropertyDef.DisplayName;
      end;
    jtArray:
      begin
        Data^.DisplayText := IntToStr(ItemIndex);
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
begin
  FPropertyDefs.Destroy;
  inherited Destroy;
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

