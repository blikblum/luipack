unit VTController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

type

  TVTEvent = (evGetText, evFocusChanged, evInitNode);
  TVTEvents = set of TVTEvent;
  
  { TCustomVirtualTreeController }

  TCustomVirtualTreeController = class(TComponent)
  private
    FNodeDataSize: Integer;
    //for now connect to only one tree
    FTree: TCustomVirtualStringTree;
    FEvents: TVTEvents;
    procedure ConnectEvents;
    procedure DisconnectEvents;
    //event bridges
    procedure FocusChangedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure InitNodeEvent(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

    procedure SetNodeDataSize(const AValue: Integer);
    procedure SetTree(const Value: TCustomVirtualStringTree);
  protected
    //abstract event handlers
    procedure DoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual; abstract;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String); virtual; abstract;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); virtual; abstract;
    //LCL methods
    procedure Loaded; override;
    //Specific methods
    procedure TreeChanged; virtual;
    //Specific properties
    property Events: TVTEvents read FEvents write FEvents;
    property NodeDataSize: Integer read FNodeDataSize write SetNodeDataSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Tree: TCustomVirtualStringTree read FTree write SetTree;
  end;

implementation

type
  TVTAccess = class(TCustomVirtualStringTree)
  end;

{ TCustomVirtualTreeController }

constructor TCustomVirtualTreeController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomVirtualTreeController.Destroy;
begin
  //todo: is necessary to handle the case when FTree is destroied before
  //  the controller
  if FTree <> nil then
    DisconnectEvents;
  inherited Destroy;
end;

procedure TCustomVirtualTreeController.ConnectEvents;
begin
  if evGetText in FEvents then
    TVTAccess(FTree).OnGetText := @GetTextEvent;
  if evFocusChanged in FEvents then
    TVTAccess(FTree).OnFocusChanged := @FocusChangedEvent;
  if evInitNode in FEvents then
    TVTAccess(FTree).OnInitNode := @InitNodeEvent;
end;

procedure TCustomVirtualTreeController.DisconnectEvents;
begin
  if evGetText in FEvents then
    TVTAccess(FTree).OnGetText := nil;
  if evFocusChanged in FEvents then
    TVTAccess(FTree).OnFocusChanged := nil;
  if evInitNode in FEvents then
    TVTAccess(FTree).OnInitNode := nil;
end;

procedure TCustomVirtualTreeController.FocusChangedEvent(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  DoFocusChanged(Node, Column);
end;

procedure TCustomVirtualTreeController.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  DoGetText(Node, Column, TextType, CellText);
end;

procedure TCustomVirtualTreeController.InitNodeEvent(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  DoInitNode(ParentNode, Node, InitialStates);
end;

procedure TCustomVirtualTreeController.SetNodeDataSize(const AValue: Integer);
begin
end;

procedure TCustomVirtualTreeController.SetTree(const Value: TCustomVirtualStringTree);
begin
  if FTree <> nil then
    DisconnectEvents;
  FTree := Value;
  if (FTree <> nil) and not (csLoading in ComponentState) then
    ConnectEvents;
  TreeChanged;
end;

procedure TCustomVirtualTreeController.Loaded;
begin
  inherited Loaded;
  if FTree <> nil then
    ConnectEvents;
end;

procedure TCustomVirtualTreeController.TreeChanged;
begin
  //
end;

end.

