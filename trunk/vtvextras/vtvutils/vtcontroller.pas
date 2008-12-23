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
    FTree: TVirtualStringTree;
    FEvents: TVTEvents;
    //event bridges
    procedure FocusChangedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: UTF8String);
    procedure InitNodeEvent(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

    procedure SetNodeDataSize(const AValue: Integer);
  protected
    //abstract event handlers
    procedure DoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual; abstract;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: UTF8String); virtual; abstract;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); virtual; abstract;

    property Events: TVTEvents read FEvents write FEvents;
    property NodeDataSize: Integer read FNodeDataSize write SetNodeDataSize;
    property Tree: TVirtualStringTree read FTree write FTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect(ATree: TVirtualStringTree); virtual;
    procedure Disconnect; virtual;
  end;

implementation


{ TCustomVirtualTreeController }

procedure TCustomVirtualTreeController.Connect(ATree: TVirtualStringTree);
begin
  FTree := ATree;
  if evGetText in FEvents then
    FTree.OnGetText := @GetTextEvent;
  if evFocusChanged in FEvents then
    FTree.OnFocusChanged := @FocusChangedEvent;
  if evInitNode in FEvents then
    FTree.OnInitNode := @InitNodeEvent;
end;

procedure TCustomVirtualTreeController.Disconnect;
begin
  Tree := nil;
end;

constructor TCustomVirtualTreeController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomVirtualTreeController.Destroy;
begin
  //todo: is necessary to handle the case when FTree is destroied before
  //  the controller
  if FTree = nil then
    Exit;
  if evGetText in FEvents then
    FTree.OnGetText := nil;
  FTree := nil;
  inherited Destroy;
end;

procedure TCustomVirtualTreeController.FocusChangedEvent(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  DoFocusChanged(Node, Column);
end;

procedure TCustomVirtualTreeController.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UTF8String);
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

end.

