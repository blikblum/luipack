unit StringsController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VTController, VirtualTrees;

type

  { TStringsController }

  {
  Uses a TStrings as source to display data.
  It's just a demo to show/test the general concept of a virtual controller
  It does not handle automatic data propagation,
  source TStrings destroy notification etc
  }
  
  TStringsController = class (TCustomVirtualTreeController)
  private
    FStrings: TStrings;
    procedure SetStrings(const AValue: TStrings);
  protected
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Connect(ATree: TVirtualStringTree); override;
    procedure UpdateTree;
    property Strings: TStrings read FStrings write SetStrings;
  end;

implementation

{ TStringsController }

procedure TStringsController.Connect(ATree: TVirtualStringTree);
begin
  inherited Connect(ATree);
  if FStrings <> nil then
    UpdateTree;
end;

constructor TStringsController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Events := [evGetText];
end;

procedure TStringsController.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  if not Assigned(FStrings) or (Node^.Index >= FStrings.Count) then
    Exit;
  CellText := FStrings[Node^.Index];
end;

procedure TStringsController.SetStrings(const AValue: TStrings);
begin
  FStrings := AValue;
  if Tree <> nil then
    UpdateTree;
end;

procedure TStringsController.UpdateTree;
var
  i: Integer;
begin
  if FStrings = nil then
    Exit;
  i := FStrings.Count;
  Tree.BeginUpdate;
  Tree.Clear;
  Tree.RootNodeCount := i;
  Tree.EndUpdate;
end;

end.

