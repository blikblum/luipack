unit VTController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

type

  TVTEvent = (evGetText);
  TVTEvents = set of TVTEvent;
  
  { TCustomVirtualTreeController }

  TCustomVirtualTreeController = class(TComponent)
  private
    FNodeDataSize: Integer;
    //for now connect to only one tree
    FTree: TVirtualStringTree;
    FEvents: TVTEvents;
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure SetNodeDataSize(const AValue: Integer);
  protected
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString); virtual; abstract;
    property Events: TVTEvents read FEvents write FEvents;
    property NodeDataSize: Integer read FNodeDataSize write SetNodeDataSize;
    property Tree: TVirtualStringTree read FTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect(ATree: TVirtualStringTree); virtual;
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

procedure TCustomVirtualTreeController.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  DoGetText(Sender, Node, Column, TextType, CellText);
end;

procedure TCustomVirtualTreeController.SetNodeDataSize(const AValue: Integer);
begin
end;

end.

