unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, VTJSON, RESTAPI,
  VirtualTrees, fpjson, EndPointView;

type

  { TMainForm }

  TMainForm = class(TForm, IFPObserver)
    BuildTestButton: TButton;
    Label2: TLabel;
    LoadCollectionButton: TButton;
    EndPointListView: TVirtualJSONListView;
    Label1: TLabel;
    BaseURLLabel: TLabel;
    OpenDialog1: TOpenDialog;
    procedure BuildTestButtonClick(Sender: TObject);
    procedure EndPointListViewFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure EndPointListViewFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure EndPointListViewGetText(Sender: TCustomVirtualJSONDataView; Node: PVirtualNode;
      NodeData: TJSONData; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure LoadCollectionButtonClick(Sender: TObject);
  private
    FAPI: TRESTAPI;
    FEndPointView: TEndPointFrame;
    procedure CheckAPIChanges;
    procedure CheckEndPointChanges;
    procedure UpdateAPIViews;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LuiJSONHelpers,
  TestBuilderView;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadCollectionButtonClick(Sender: TObject);
begin
  CheckAPIChanges;
  if OpenDialog1.Execute then
  begin
    FAPI.LoadFromFile(OpenDialog1.FileName);
    UpdateAPIViews;
  end;
end;

procedure TMainForm.EndPointListViewGetText(Sender: TCustomVirtualJSONDataView; Node: PVirtualNode;
  NodeData: TJSONData; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  case Column of
    0:
      begin
        CellText := NodeData.GetPath('request.method', '');
      end;
    1:
      begin
        CellText := Trim(NodeData.GetPath('name', ''));
        if CellText = '' then
        begin
          CellText := NodeData.GetPath('request.url', '');
          CellText := Copy(CellText, Length(FAPI.BaseURL) + 1, Length(CellText));
        end;
      end;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CheckEndPointChanges;
  CheckAPIChanges;
end;

procedure TMainForm.EndPointListViewFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  EndPointData: TJSONObject;
begin
  if EndPointListView.GetData(Node, EndPointData) then
    FEndPointView.SetEndPoint(EndPointData);
end;

procedure TMainForm.BuildTestButtonClick(Sender: TObject);
begin
  with TTestBuilderForm.Create(nil) do
  begin
    try
      ShowModal;
    finally
      Destroy;
    end;
  end;
end;

procedure TMainForm.EndPointListViewFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  CheckEndPointChanges;
end;

procedure TMainForm.UpdateAPIViews;
begin
  BaseURLLabel.Caption := FAPI.BaseURL;
  EndPointListView.Data := FAPI.EndPointListData;
  EndPointListView.LoadData;
end;

procedure TMainForm.CheckAPIChanges;
begin
  if FAPI.IsModified or FEndPointView.IsModified then
  begin
    if MessageDlg('Data changed', 'Do you want to save changes?', mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      if FAPI.IsModified then
        FAPI.Save;
      if FEndPointView.IsModified then
        FEndPointView.Save;
    end;
  end;
end;

procedure TMainForm.CheckEndPointChanges;
begin
  if FEndPointView.IsModified then
  begin
    if MessageDlg('Endpoint changed', 'Do you want to save the changes?', mtConfirmation, mbYesNo, 0) = mrYes then
      FEndPointView.Save;
  end;
end;

procedure TMainForm.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
  Data: Pointer);
begin
  if Operation = ooChange then
    FAPI.Modified;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAPI := TRESTAPI.Create(Self);
  FEndPointView := TEndPointFrame.Create(Self);
  FEndPointView.Parent := Self;
  FEndPointView.Align := alRight;
  FEndPointView.Visible := True;
  FEndPointView.FPOAttachObserver(Self);
end;

end.

