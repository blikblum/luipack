unit LuiConfigTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LuiConfig, VirtualTrees, Graphics;

const
  // Idea borrowed from the advanced demo
  // Helper message to decouple node change handling from edit handling.
  WM_STARTEDITING = LM_USER + 778;

type

  TVTLuiConfigOption = (ltoAutoLoad);

  TVTLuiConfigOptions = set of TVTLuiConfigOption;

const
  DefaultConfigOptions = [ltoAutoLoad];

type

  { TLuiConfigTreeOptions }

  TLuiConfigTreeOptions = class(TCustomStringTreeOptions)
  private
    FConfigOptions: TVTLuiConfigOptions;
  public
    constructor Create(AOwner: TBaseVirtualTree); override;
  published
    property ConfigOptions: TVTLuiConfigOptions read FConfigOptions write FConfigOptions default DefaultConfigOptions;
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
  end;

  { TLuiConfigTree }

  TLuiConfigTree = class(TCustomVirtualStringTree, ILuiConfigObserver)
  private
    FConfig: TLuiConfig;
    FConfigDataOffset: Cardinal;
    FItems: TStrings;
    FSections: TStrings;
    FVisibleSections: TStrings;
    procedure ConfigNotification(NotificationType: TLuiConfigNotificationType;
      Data: PtrInt);
    function GetOptions: TLuiConfigTreeOptions;
    procedure InitHeader;
    procedure SetConfig(const AValue: TLuiConfig);
    procedure SetOptions(const AValue: TLuiConfigTreeOptions);
    procedure SetSections(const AValue: TStrings);
    procedure WMStartEditing(var Message: TLMessage); message WM_STARTEDITING;
  protected
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DoExpanded(Node: PVirtualNode); override;
    function DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex): Boolean; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    {$if  VTMajorVersion > 4}
    function DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal): Boolean; override;
    {$else}
    procedure DoInitChildren(Node: PVirtualNode; var NodeChildCount: Cardinal); override;
    {$endif}
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const AText: String); override;
    procedure DoPaintText(Node: PVirtualNode; const ACanvas: TCanvas;
       Column: TColumnIndex; TextType: TVSTTextType); override;
    function GetConfigData(Node: PVirtualNode): Pointer;
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadTree;
  published
    property Config: TLuiConfig read FConfig write SetConfig;
    property Sections: TStrings read FSections write SetSections;
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
    property TreeOptions: TLuiConfigTreeOptions read GetOptions write SetOptions;
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
  LCLIntf;

type
  TConfigData = record
    Key: String;
  end;
  PConfigData = ^TConfigData;

{ TLuiConfigTree }

procedure TLuiConfigTree.ConfigNotification(NotificationType: TLuiConfigNotificationType; Data: PtrInt);
begin
  case NotificationType of
    lcnOpen:
      if ltoAutoLoad in TreeOptions.ConfigOptions then
        LoadTree;
    lcnClose:
      Clear;
  end;
end;

function TLuiConfigTree.GetOptions: TLuiConfigTreeOptions;
begin
  Result := TLuiConfigTreeOptions(inherited TreeOptions);
end;

procedure TLuiConfigTree.InitHeader;
var
  Column: TVirtualTreeColumn;
begin
  Column := Header.Columns.Add;
  Column.Text := 'Key';
  Column.Width := 150;
  Column := Header.Columns.Add;
  Column.Text := 'Data';
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoVisible, hoAutoResize, hoAutoSpring];
end;

procedure TLuiConfigTree.LoadTree;
begin
  BeginUpdate;
  Clear;
  if FSections.Count > 0 then
    FVisibleSections.Assign(FSections)
  else
    FConfig.ReadSections(FVisibleSections, True);
  RootNodeCount := FVisibleSections.Count;
  //todo: make full expand optional
  FullExpand(nil);
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

procedure TLuiConfigTree.SetOptions(const AValue: TLuiConfigTreeOptions);
begin
  TreeOptions.Assign(AValue);
end;

procedure TLuiConfigTree.SetSections(const AValue: TStrings);
begin
  FSections.Assign(AValue);
end;

procedure TLuiConfigTree.WMStartEditing(var Message: TLMessage);
var
  Node: PVirtualNode;
begin
  Node := Pointer(Message.WParam);
  EditNode(Node, 1);
end;

function TLuiConfigTree.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := (Node^.Parent = RootNode) and (Column = 1);
end;

procedure TLuiConfigTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := Column = 1;
end;

procedure TLuiConfigTree.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  if Assigned(Node) and (Node^.Parent <> RootNode) and
    not (tsIncrementalSearching in TreeStates) then
      PostMessage(Self.Handle, WM_STARTEDITING, Integer(Node), 0);
end;

procedure TLuiConfigTree.DoExpanded(Node: PVirtualNode);
begin
  ValidateChildren(Node, True);
  inherited DoExpanded(Node);
end;

function TLuiConfigTree.DoFocusChanging(OldNode, NewNode: PVirtualNode;
  OldColumn, NewColumn: TColumnIndex): Boolean;
begin
  //todo: make this behavior optional
  Result := (NewNode <> nil) and (NewNode^.Parent <> RootNode);
  ClearSelection;
end;

procedure TLuiConfigTree.DoFreeNode(Node: PVirtualNode);
var
  Data: PConfigData;
begin
  Data := GetConfigData(Node);
  Data^.Key := '';
  inherited DoFreeNode(Node);
end;

procedure TLuiConfigTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data, ParentData: PConfigData;
begin
  inherited DoGetText(Node, Column, TextType, CellText);
  //the user changed the text through OnGetText
  if CellText <> '' then
    Exit;
  Data := GetConfigData(Node);
  if Node^.Parent = RootNode then
  begin
    //Section
    CellText := FConfig.GetSectionText(Data^.Key);
  end
  else
  begin
    ParentData := GetConfigData(Node^.Parent);
    //Item
    if Column = 0 then
      CellText := FConfig.GetItemText(Data^.Key, ParentData^.Key)
    else
    begin
      //handle item data differently according to data type
      CellText := FConfig.ReadString(ParentData^.Key, Data^.Key);
    end;
  end;
end;


{$if VTMajorVersion > 4}
function TLuiConfigTree.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal): Boolean;
{$else}
procedure TLuiConfigTree.DoInitChildren(Node: PVirtualNode;
  var NodeChildCount: Cardinal);
{$endif}
var
  Data: PConfigData;
begin
  Data := GetConfigData(Node);
  FConfig.ReadSection(Data^.Key, FItems, True);
  NodeChildCount := FItems.Count;
  {$if  VTMajorVersion > 4}
  Result := True;
  {$endif}
end;

procedure TLuiConfigTree.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  Data: PConfigData;
begin
  Data := GetConfigData(Node);
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

procedure TLuiConfigTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const AText: String);
var
  Data, ParentData: PConfigData;
begin
  if (Node^.Parent = RootNode) or (Column <> 1) then
    Exit;
  Data := GetConfigData(Node);
  ParentData := GetConfigData(Node^.Parent);
  FConfig.WriteString(ParentData^.Key, Data^.Key, AText);
  inherited DoNewText(Node, Column, AText);
end;

procedure TLuiConfigTree.DoPaintText(Node: PVirtualNode;
  const ACanvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Node^.Parent = RootNode then
    ACanvas.Font.Style := [fsBold];
  inherited DoPaintText(Node, ACanvas, Column, TextType);
end;

function TLuiConfigTree.GetConfigData(Node: PVirtualNode): Pointer;
begin
  if (Node = RootNode) or (Node = nil) then
    Result := nil
  else
    Result := PByte(Node) + FConfigDataOffset;
end;

function TLuiConfigTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TLuiConfigTreeOptions;
end;

constructor TLuiConfigTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigDataOffset := AllocateInternalDataArea(SizeOf(TConfigData));
  DefaultText := '';
  FItems := TStringList.Create;
  FSections := TStringList.Create;
  FVisibleSections := TStringList.Create;
  Indent := 13;
  Margin := 0;
  InitHeader;
  with TreeOptions do
  begin
    AutoOptions := AutoOptions + [toAutoSpanColumns];
    PaintOptions := PaintOptions - [toShowTreeLines] +
      [toShowHorzGridLines, toShowVertGridLines, toPopupMode, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;
end;

destructor TLuiConfigTree.Destroy;
begin
  Config := nil; //necessary to remove from the config observer list
  FItems.Destroy;
  FSections.Destroy;
  FVisibleSections.Destroy;
  inherited Destroy;
end;

{ TLuiConfigTreeOptions }

constructor TLuiConfigTreeOptions.Create(AOwner: TBaseVirtualTree);
begin
  inherited Create(AOwner);
  FConfigOptions := DefaultConfigOptions;
end;

end.

