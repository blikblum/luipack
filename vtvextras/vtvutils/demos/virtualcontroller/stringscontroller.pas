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
    procedure DoGetText(Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure TreeChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTree;
    property Strings: TStrings read FStrings write SetStrings;
  end;

implementation

{ TStringsController }

procedure TStringsController.TreeChanged;
begin
  UpdateTree;
end;

constructor TStringsController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Events := [evGetText];
end;

procedure TStringsController.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  if not Assigned(FStrings) or (Node^.Index >= FStrings.Count) then
    Exit;
  CellText := FStrings[Node^.Index];
end;

procedure TStringsController.SetStrings(const AValue: TStrings);
begin
  FStrings := AValue;
  UpdateTree;
end;

procedure TStringsController.UpdateTree;
begin
  if (FStrings = nil) or (Tree = nil) then
    Exit;
  Tree.BeginUpdate;
  Tree.Clear;
  Tree.RootNodeCount := FStrings.Count;
  Tree.EndUpdate;
end;

end.

