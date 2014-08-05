unit MainPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, fpjson, LuiJSONModel,
  PatientModel, Controls;

type

  { TMainPresenter }

  TMainPresenter = class(TBasePresenter)
  private
    FOnSelectedPatientChange: TNotifyEvent;
    FPatients: TPatients;
    FSelectedPatientData: TJSONObject;
    procedure SetSelectedPatientData(Value: TJSONObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPatient;
    procedure DeletePatient;
    procedure EditPatient;
    procedure Initialize; override;
    property Patients: TPatients read FPatients;
    property OnSelectedPatientChange: TNotifyEvent read FOnSelectedPatientChange write FOnSelectedPatientChange;
    property SelectedPatientData: TJSONObject read FSelectedPatientData write SetSelectedPatientData;
  end;


implementation

uses
  LuiJSONUtils;

{ TMainPresenter }

procedure TMainPresenter.SetSelectedPatientData(Value: TJSONObject);
begin
  if FSelectedPatientData = Value then Exit;
  FSelectedPatientData := Value;
  if Assigned(FOnSelectedPatientChange) then
    FOnSelectedPatientChange(Self);
end;

procedure TMainPresenter.Initialize;
begin
  inherited Initialize;
  FPatients.Fetch;
end;

constructor TMainPresenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPatients := TPatients.Create;
end;

destructor TMainPresenter.Destroy;
begin
  FPatients.Free;
  inherited Destroy;
end;

procedure TMainPresenter.AddPatient;
var
  Patient: TPatient;
begin
  Patient := FPatients.CreateItem;
  if Presentations['patient'].ShowModal(['Model', Patient]) <> mrOk then
    Patient.Free;
end;

procedure TMainPresenter.DeletePatient;
var
  Patient: TPatient;
begin
  if FSelectedPatientData = nil then
    Exit;
  Patient := FPatients.Get(FSelectedPatientData);
  Patient.Delete;
  Patient.Free;
end;

procedure TMainPresenter.EditPatient;
var
  Patient: TPatient;
begin
  if FSelectedPatientData = nil then
    Exit;
  Patient := FPatients.Get(FSelectedPatientData);
  Presentations['contact'].ShowModal(['Model', Patient]);
end;

end.

