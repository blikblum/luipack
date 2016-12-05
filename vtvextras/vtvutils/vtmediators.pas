unit VTMediators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Graphics;

type

  TVTEvent = (evGetText, evFocusChanged, evInitNode, evDrawText, evMeasureItem, evResize, evChecked);
  TVTEvents = set of TVTEvent;

  { TCustomVirtualTreeMediator }

  TCustomVirtualTreeMediator = class(TComponent)
  private
    FNodeDataSize: Integer;
    FTree: TCustomVirtualStringTree;
    FDesignInitNodeEvent: TVTInitNodeEvent;
    FDesignCheckedEvent: TVTChangeEvent;
    FDesignDrawTextEvent: TVTDrawTextEvent;
    FDesignFocusChangedEvent: TVTFocusChangeEvent;
    FDesignGetTextEvent: TVSTGetTextEvent;
    FDesignMeasureItemEvent: TVTMeasureItemEvent;
    FDesignResize: TNotifyEvent;
    FEvents: TVTEvents;
    procedure ConnectEvents;
    procedure DisconnectEvents;
    //event bridges
    procedure CheckedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DrawTextEvent(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FocusChangedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure InitNodeEvent(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure MeasureItemEvent(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer);
    procedure SetNodeDataSize(const AValue: Integer);
    procedure SetTree(const Value: TCustomVirtualStringTree);
  protected
    //abstract event handlers
    procedure DoChecked(Node: PVirtualNode); virtual; abstract;
    procedure DoDrawText(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean); virtual; abstract;
    procedure DoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual; abstract;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String); virtual; abstract;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); virtual; abstract;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); virtual; abstract;
    procedure DoResize(Sender: TObject); virtual; abstract;
    //LCL methods
    procedure Loaded; override;
    //Specific methods
    procedure TreeChanged; virtual;
    //Specific properties
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

{ TCustomVirtualTreeMediator }


constructor TCustomVirtualTreeMediator.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  inherited Create(AOwner);

  Method := TMethod(@Self.DoGetText);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evGetText);

  Method := TMethod(@Self.DoFocusChanged);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evFocusChanged);

  Method := TMethod(@Self.DoInitNode);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evInitNode);

  Method := TMethod(@Self.DoDrawText);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evDrawText);

  Method := TMethod(@Self.DoMeasureItem);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evMeasureItem);

  Method := TMethod(@Self.DoResize);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evResize);

  Method := TMethod(@Self.DoChecked);
  if Method.Code <> Pointer(@system.AbstractError) then
    Include(FEvents, evChecked);
end;

destructor TCustomVirtualTreeMediator.Destroy;
begin
  //todo: is necessary to handle the case when FTree is destroied before
  //  the controller
  if FTree <> nil then
    DisconnectEvents;
  inherited Destroy;
end;

procedure TCustomVirtualTreeMediator.ConnectEvents;
begin
  if evGetText in FEvents then
  begin
    FDesignGetTextEvent := TVTAccess(FTree).OnGetText;
    TVTAccess(FTree).OnGetText := @GetTextEvent;
  end;
  if evFocusChanged in FEvents then
  begin
    FDesignFocusChangedEvent := TVTAccess(FTree).OnFocusChanged;
    TVTAccess(FTree).OnFocusChanged := @FocusChangedEvent;
  end;
  if evInitNode in FEvents then
  begin
    FDesignInitNodeEvent := TVTAccess(FTree).OnInitNode;
    TVTAccess(FTree).OnInitNode := @InitNodeEvent;
  end;
  if evDrawText in FEvents then
  begin
    FDesignDrawTextEvent := TVTAccess(FTree).OnDrawText;
    TVTAccess(FTree).OnDrawText := @DrawTextEvent;
  end;
  if evMeasureItem in FEvents then
  begin
    FDesignMeasureItemEvent := TVTAccess(FTree).OnMeasureItem;
    TVTAccess(FTree).OnMeasureItem := @MeasureItemEvent;
  end;
  if evResize in FEvents then
  begin
    FDesignResize := TVTAccess(FTree).OnResize;
    TVTAccess(FTree).OnResize := @DoResize;
  end;
  if evChecked in FEvents then
  begin
    FDesignCheckedEvent := TVTAccess(FTree).OnChecked;
    TVTAccess(FTree).OnChecked := @CheckedEvent;
  end;
end;

procedure TCustomVirtualTreeMediator.DisconnectEvents;
begin
  if evGetText in FEvents then
    TVTAccess(FTree).OnGetText := nil;
  if evFocusChanged in FEvents then
    TVTAccess(FTree).OnFocusChanged := nil;
  if evInitNode in FEvents then
    TVTAccess(FTree).OnInitNode := nil;
  if evDrawText in FEvents then
    TVTAccess(FTree).OnDrawText := nil;
  if evMeasureItem in FEvents then
    TVTAccess(FTree).OnMeasureItem := nil;
  if evResize in FEvents then
    TVTAccess(FTree).OnResize := nil;
  if evChecked in FEvents then
    TVTAccess(FTree).OnChecked := nil;
end;

procedure TCustomVirtualTreeMediator.CheckedEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  DoChecked(Node);
  if FDesignCheckedEvent <> nil then
    FDesignCheckedEvent(Sender, Node);
end;

procedure TCustomVirtualTreeMediator.DrawTextEvent(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DoDrawText(TargetCanvas, Node, Column, CellText, CellRect, DefaultDraw);
  if FDesignDrawTextEvent <> nil then
    FDesignDrawTextEvent(Sender, TargetCanvas, Node, Column, CellText, CellRect, DefaultDraw);
end;

procedure TCustomVirtualTreeMediator.FocusChangedEvent(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  DoFocusChanged(Node, Column);
  if FDesignFocusChangedEvent <> nil then
    FDesignFocusChangedEvent(Sender, Node, Column);
end;

procedure TCustomVirtualTreeMediator.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  DoGetText(Node, Column, TextType, CellText);
  if FDesignGetTextEvent <> nil then
    FDesignGetTextEvent(Sender, Node, Column, TextType, CellText);
end;

procedure TCustomVirtualTreeMediator.InitNodeEvent(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  DoInitNode(ParentNode, Node, InitialStates);
  if FDesignInitNodeEvent <> nil then
    FDesignInitNodeEvent(Sender, ParentNode, Node, InitialStates);
end;

procedure TCustomVirtualTreeMediator.MeasureItemEvent(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
begin
  DoMeasureItem(TargetCanvas, Node, NodeHeight);
  if FDesignMeasureItemEvent <> nil then
    FDesignMeasureItemEvent(Sender, TargetCanvas, Node, NodeHeight);
end;

procedure TCustomVirtualTreeMediator.SetNodeDataSize(const AValue: Integer);
begin
end;

procedure TCustomVirtualTreeMediator.SetTree(const Value: TCustomVirtualStringTree);
begin
  if FTree <> nil then
    DisconnectEvents;
  FTree := Value;
  if (FTree <> nil) and not (csLoading in ComponentState) then
    ConnectEvents;
  TreeChanged;
end;

procedure TCustomVirtualTreeMediator.Loaded;
begin
  inherited Loaded;
  if FTree <> nil then
    ConnectEvents;
end;

procedure TCustomVirtualTreeMediator.TreeChanged;
begin
  //
end;

end.

