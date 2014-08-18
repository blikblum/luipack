unit PatientEvaluationsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  VTJSON, AdvancedLabel, VirtualTrees, fpjson, PatientEvaluationsPresenter;

type

  { TPatientEvaluationsForm }

  TPatientEvaluationsForm = class(TForm, IFPObserver)
    AddButton: TBitBtn;
    EditEvaluationLabel: TAdvancedLabel;
    DeleteEvaluationLabel: TAdvancedLabel;
    EvaluationListView: TVirtualJSONListView;
    procedure DeleteEvaluationLabelClick(Sender: TObject);
    procedure EditEvaluationLabelClick(Sender: TObject);
    procedure EvaluationListViewBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure EvaluationListViewGetText(Sender: TCustomVirtualJSONDataView;
      Node: PVirtualNode; NodeData: TJSONData; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure EvaluationListViewHotChange(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode);
  private
    FPresenter: TPatientEvaluationsPresenter;
    { private declarations }
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure SetPresenter(Value: TPatientEvaluationsPresenter);
  public
    { public declarations }
  published
    property Presenter: TPatientEvaluationsPresenter read FPresenter write SetPresenter;
  end;

var
  PatientEvaluationsForm: TPatientEvaluationsForm;

implementation

{$R *.lfm}

{ TPatientEvaluationsForm }

procedure TPatientEvaluationsForm.DeleteEvaluationLabelClick(Sender: TObject);
var
  EvaluationData: TJSONObject;
begin
  if EvaluationListView.GetData(EvaluationListView.HotNode, EvaluationData) then
    FPresenter.DeleteEvaluation(EvaluationData);
end;

procedure TPatientEvaluationsForm.EditEvaluationLabelClick(Sender: TObject);
var
  EvaluationData: TJSONObject;
begin
  if EvaluationListView.GetData(EvaluationListView.HotNode, EvaluationData) then
    FPresenter.EditEvaluation(EvaluationData);
end;

procedure TPatientEvaluationsForm.EvaluationListViewBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
begin
  if Node = Sender.HotNode then
  begin
    EraseAction := eaColor;
    ItemColor := $00E0DBDA;
  end;
end;

procedure TPatientEvaluationsForm.EvaluationListViewGetText(
  Sender: TCustomVirtualJSONDataView; Node: PVirtualNode; NodeData: TJSONData;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeObjData: TJSONObject absolute NodeData;
begin
  case Column of
    0: CellText := DateToStr(NodeObjData.Get('date', 0.0));
    1:
      begin
        case NodeObjData.Get('shiftid', 0) of
          1: CellText := 'Manhã';
          2: CellText := 'Tarde';
          3: CellText := 'Noite';
        else
          CellText := '';
        end;
      end;
  end;
end;

procedure TPatientEvaluationsForm.EvaluationListViewHotChange(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode);
const
  Column = 2;
var
  R: TRect;
begin
  if NewNode <> nil then
  begin
    R := Sender.GetDisplayRect(NewNode, Column, False);
    Inc(R.Top);
    Inc(R.Left, 2);
    EditEvaluationLabel.SetBounds(R.Left, R.Top, EditEvaluationLabel.Width, EditEvaluationLabel.Height);
    Inc(R.Left, 70);
    DeleteEvaluationLabel.SetBounds(R.Left, R.Top, DeleteEvaluationLabel.Width, DeleteEvaluationLabel.Height);
    EditEvaluationLabel.Visible := True;
    DeleteEvaluationLabel.Visible := True;
  end
  else
  begin
    EditEvaluationLabel.Visible := False;
    DeleteEvaluationLabel.Visible := False;
  end;
  if OldNode <> nil then
    Sender.InvalidateNode(OldNode);
  if NewNode <> nil then
    Sender.InvalidateNode(NewNode);
end;

procedure TPatientEvaluationsForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FPresenter.Evaluations then
  begin
    case Operation of
      ooChange, ooAddItem, ooDeleteItem:
        begin
          if (Data = nil) or (Operation in [ooAddItem, ooDeleteItem]) then
          begin
            EvaluationListView.Data := FPresenter.Evaluations.Data;
            EvaluationListView.LoadData;
          end
          else
            EvaluationListView.Invalidate;
        end;
    end;
  end;
end;

procedure TPatientEvaluationsForm.SetPresenter(
  Value: TPatientEvaluationsPresenter);
begin
  FPresenter := Value;
  FPresenter.Evaluations.FPOAttachObserver(Self);
end;

end.

