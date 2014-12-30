unit JSONFilterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, StdCtrls, VTJSON,
  fpjson, VirtualTrees;


type

  TActionLink = class(TCustomLabel)
  published
    property OnClick;
    property Caption;
  end;

  { TJSONFilterFrame }

  TJSONFilterFrame = class(TFrame)
    ListView: TVirtualJSONListView;
    procedure ListViewClick(Sender: TObject);
    procedure ListViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ListViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FActionLink: TActionLink;
    FData: TJSONData;
    FFilterProperty: String;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FSelectedData: TJSONObject;
    FMatchCount: Integer;
    FSearchTerm: String;
    FTextProperty: String;
    procedure UpdateListView;
    procedure FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure ClearCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetHasMatches: Boolean;
    procedure InitializeListView;
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Initialize;
    procedure LoadData;
    procedure SelectFirst;
    procedure SetTerm(const SearchTerm: String);
    property Data: TJSONData read FData write FData;
    property FilterProperty: String read FFilterProperty write FFilterProperty;
    property HasMatches: Boolean read GetHasMatches;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property SelectedData: TJSONObject read FSelectedData;
    property TextProperty: String read FTextProperty write FTextProperty;
  published
    property ActionLink: TActionLink read FActionLink;
  end;

implementation

uses
  strutils, LCLType, Dialogs, LCLIntf, Graphics;

{$R *.lfm}

type
  TFilterRecord = record
    PropName: String;
  end;
  PFilterRecord = ^TFilterRecord;

{ TJSONFilterFrame }

procedure TJSONFilterFrame.ListViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Sender.IsVisible[Node] := False;
end;

procedure TJSONFilterFrame.ListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) and (ListView.FocusedNode = ListView.GetFirstVisible) then
  begin
    ListView.Selected[ListView.FocusedNode] := False;
    ListView.FocusedNode := nil;
    Key := VK_UNKNOWN;
  end
  else if Key = VK_ESCAPE then
  begin
    //todo notify ESC is pressed
    //if Parent <> nil then
      //Parent.;
    //Key := VK_UNKNOWN;
  end;
end;

procedure TJSONFilterFrame.ListViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (FOnChange <> nil) then
    FOnChange(Self);
end;

procedure TJSONFilterFrame.ListViewFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  ListView.GetData(Node, FSelectedData);
  if FOnSelect <> nil then
    FOnSelect(Self);
end;

procedure TJSONFilterFrame.ListViewClick(Sender: TObject);
begin
  if ListView.FocusedNode <> nil then
  begin
    if FOnChange <> nil then
      FOnChange(Self);
  end;
end;

procedure TJSONFilterFrame.FilterCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TJSONObject;
  NodeMatches: Boolean;
begin
  NodeMatches := ListView.GetData(Node, NodeData);
  if NodeMatches then
    NodeMatches := IsWild(NodeData.Get(PFilterRecord(Data)^.PropName, ''), FSearchTerm, True);
  if NodeMatches then
    Inc(FMatchCount);
  Sender.IsVisible[Node] := NodeMatches;
end;

procedure TJSONFilterFrame.UpdateListView;
var
  FilterRecord: TFilterRecord;
begin
  if FSearchTerm <> '' then
  begin
    if FFilterProperty <> '' then
      FilterRecord.PropName := FFilterProperty
    else
      FilterRecord.PropName := FTextProperty;
    ListView.IterateSubtree(nil, @FilterCallback, @FilterRecord);
  end
  else
    ListView.IterateSubtree(nil, @ClearCallback, nil);
end;

procedure TJSONFilterFrame.ClearCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Sender.IsVisible[Node] := False;
end;

function TJSONFilterFrame.GetHasMatches: Boolean;
begin
  Result := FMatchCount > 0;
end;

procedure TJSONFilterFrame.InitializeListView;
begin
  TVirtualJSONDataViewColumn(ListView.Header.Columns[0]).PropertyName := FTextProperty;
  ListView.Data := FData;
  LoadData;
end;

procedure TJSONFilterFrame.Paint;
var
  R: types.TRect;
begin
  inherited Paint;
  R := ClientRect;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);
  Canvas.Frame3d(R, 1, bvRaised);
end;

constructor TJSONFilterFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActionLink := TActionLink.Create(Self);
  FActionLink.Cursor := crHandPoint;
  FActionLink.Anchors := [akBottom, akLeft];
  FActionLink.SetBounds(2, ListView.Height + ListView.Top + 2, FActionLink.Width, FActionLink.Height);
  FActionLink.Font.Color := clBlue;
  FActionLink.Font.Style := [fsUnderline];
  FActionLink.Parent := Self;
end;

procedure TJSONFilterFrame.Initialize;
begin
  InitializeListView;
  FActionLink.Visible := FActionLink.OnClick <> nil;
  if FActionLink.Visible then
    ListView.BorderSpacing.Bottom := 17
  else
    ListView.BorderSpacing.Bottom := 0;
end;

procedure TJSONFilterFrame.LoadData;
begin
  ListView.LoadData;
  ListView.ValidateNode(ListView.RootNode, True);
end;

procedure TJSONFilterFrame.SelectFirst;
begin
  if ListView.CanFocus then
    ListView.SetFocus;
  ListView.FocusedNode := ListView.GetFirstVisible;
  ListView.Selected[ListView.FocusedNode] := True;
end;

procedure TJSONFilterFrame.SetTerm(const SearchTerm: String);
begin
  ListView.ClearSelection;
  FMatchCount := 0;
  FSearchTerm := Trim(SearchTerm);
  if SearchTerm <> '' then
    FSearchTerm := '*' + SearchTerm + '*'
  else
    FSearchTerm := '*';
  UpdateListView;
end;

end.

