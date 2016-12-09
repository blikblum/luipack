unit MainView;

{$mode objfpc}{$H+}

//inspired by http://examples.sencha.com/extjs/6.2.0/examples/kitchensink/#grouped-grid

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, VTJSON, fpjson, VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm)
    LoadButton: TButton;
    RestaurantsView: TVirtualJSONTreeView;
    procedure LoadButtonClick(Sender: TObject);
    procedure RestaurantsViewGetGroup(Sender: TCustomVirtualJSONDataView;
      NodeData: TJSONObject; var GroupText: String);
    procedure RestaurantsViewGetText(Sender: TCustomVirtualJSONDataView; Node: PVirtualNode;
      NodeData: TJSONData; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FData: TJSONArray;
  public
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LuiJSONUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  if TryReadJSONFile('restaurants.json', FData) then
  begin
    RestaurantsView.Data := FData;
    RestaurantsView.LoadData;
  end;
end;

procedure TMainForm.RestaurantsViewGetGroup(Sender: TCustomVirtualJSONDataView;
  NodeData: TJSONObject; var GroupText: String);
begin
  GroupText := NodeData.Get('cuisine', 'Unknow');
end;

procedure TMainForm.RestaurantsViewGetText(Sender: TCustomVirtualJSONDataView;
  Node: PVirtualNode; NodeData: TJSONData; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  ObjData: TJSONObject absolute NodeData;
begin
  if Sender.NodeParent[Node] = nil then
    CellText := Format('Cuisine: %s (%d Items)', [CellText, ObjData.Get('itemcount', 0)]);
end;

destructor TMainForm.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

end.

