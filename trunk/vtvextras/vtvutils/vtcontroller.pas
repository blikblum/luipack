unit VTController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

type

  TVTEvent = (evGetText, evFocusChanged);
  TVTEvents = set of TVTEvent;
  
  { TCustomVirtualTreeController }

  TCustomVirtualTreeController = class(TComponent)
  private
    FNodeDataSize: Integer;
    //for now connect to only one tree
    FTree: TVirtualStringTree;
    FEvents: TVTEvents;
    procedure FocusChangedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure SetNodeDataSize(const AValue: Integer);
  protected
    procedure DoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual; abstract;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString); virtual; abstract;
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

type
  TCustomVirtualStringTreeAccess = class (TCustomVirtualStringTree)
  end;


{ TCustomVirtualTreeController }

procedure TCustomVirtualTreeController.Connect(ATree: TVirtualStringTree);
begin
  FTree := ATree;
  if evGetText in FEvents then
    FTree.OnGetText := @GetTextEvent;
  if evFocusChanged in FEvents then
    FTree.OnFocusChanged := @FocusChangedEvent;
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
  var CellText: WideString);
begin
  DoGetText(Node, Column, TextType, CellText);
end;

procedure TCustomVirtualTreeController.SetNodeDataSize(const AValue: Integer);
begin
end;

end.

