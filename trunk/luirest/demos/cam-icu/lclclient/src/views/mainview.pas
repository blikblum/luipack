unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  JvXPBar, VTJSON, AdvancedLabel, MainPresenter, fpjson, VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm, IFPObserver)
    ExportDataLabel: TAdvancedLabel;
    Label1: TLabel;
    PatientCountLabel: TLabel;
    PatientBar: TJvXPBar;
    PatientListView: TVirtualJSONListView;
    procedure ExportDataLabelClick(Sender: TObject);
    procedure PatientBarDeleteItemClick(Sender: TObject);
    procedure PatientBarEvaluationsClick(Sender: TObject);
    procedure PatientBarCadastreItemClick(Sender: TObject);
    procedure PatientListViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure PatientListViewGetText(Sender: TCustomVirtualJSONDataView;
      Node: PVirtualNode; NodeData: TJSONData; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
  private
    FPresenter: TMainPresenter;
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure PatientSelectedChange(Sender: TObject);
    procedure SetPresenter(Value: TMainPresenter);
    procedure UpdatePatientCount;
    procedure UpdatePatientSelection;
  public
  published
    property Presenter: TMainPresenter read FPresenter write SetPresenter;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LuiJSONUtils, DataExporter;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.PatientListViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: TJSONObject;
begin
  PatientListView.GetData(Node, NodeData);
  FPresenter.SelectedPatientData := NodeData;
end;

procedure TMainForm.PatientListViewGetText(Sender: TCustomVirtualJSONDataView;
  Node: PVirtualNode; NodeData: TJSONData; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
const
  PropNames: array[2..4] of String = ('birthdate', 'internmentdate', 'dischargedate');
var
  NodeObjData: TJSONObject absolute NodeData;
  ADate: TDateTime;
begin
  case Column of
    2, 3, 4:
      begin
        if FindJSONProp(NodeObjData, PropNames[Column], ADate) then
          CellText := DateToStr(ADate);
      end;
  end;
end;

procedure TMainForm.PatientBarEvaluationsClick(Sender: TObject);
begin
  FPresenter.ShowPatientEvaluations;
end;

procedure TMainForm.PatientBarDeleteItemClick(Sender: TObject);
begin
  if FPresenter.SelectedPatientData = nil then
    Exit;
  if MessageDlg('Excluir paciente',
    Format('Tem certeza que deseja excluir "%s" ?', [FPresenter.SelectedPatientData.Get('name', '')]) +
    LineEnding + 'Os dados serão excluídos permanentemente', mtConfirmation, mbYesNo, 1) = mrYes then
    FPresenter.DeletePatient;
end;

procedure TMainForm.ExportDataLabelClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  begin
    try
      if Execute then
        TCAMICUDataExporter.SaveToSpreadSheet(FPresenter.Patients, FileName);
    finally
      Destroy;
    end;
  end;
end;

procedure TMainForm.PatientBarCadastreItemClick(Sender: TObject);
begin
  FPresenter.ShowPatientCadastre;
end;

procedure TMainForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FPresenter.Patients then
  begin
    case Operation of
      ooChange, ooAddItem, ooDeleteItem:
        begin
          if (Data = nil) or (Operation in [ooAddItem, ooDeleteItem]) then
          begin
            PatientListView.Data := FPresenter.Patients.Data;
            PatientListView.LoadData;
            UpdatePatientSelection;
            UpdatePatientCount;
          end
          else
            PatientListView.Invalidate;
        end;
    end;
  end;
end;

procedure TMainForm.PatientSelectedChange(Sender: TObject);
begin
  UpdatePatientSelection;
end;

procedure TMainForm.SetPresenter(Value: TMainPresenter);
begin
  FPresenter := Value;
  FPresenter.Patients.FPOAttachObserver(Self);
  FPresenter.OnSelectedPatientChange := @PatientSelectedChange;
end;

procedure TMainForm.UpdatePatientCount;
begin
  PatientCountLabel.Caption := IntToStr(FPresenter.Patients.Count) + ' paciente(s)';
end;

procedure TMainForm.UpdatePatientSelection;
begin
  PatientListView.FocusedNode := PatientListView.GetNode(FPresenter.SelectedPatientData);
  PatientListView.Selected[PatientListView.FocusedNode] := True;
  if FPresenter.SelectedPatientData <> nil then
    PatientBar.Caption := FPresenter.SelectedPatientData.Get('name', '')
  else
    PatientBar.Caption := '';
end;

end.

