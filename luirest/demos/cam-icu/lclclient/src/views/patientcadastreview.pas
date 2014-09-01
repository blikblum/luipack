unit PatientCadastreView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, JSONFormMediator, ZVDateTimePicker, PatientModel;

type

  { TPatientCadastreForm }

  TPatientCadastreForm = class(TForm)
    BedNumberEdit: TEdit;
    CancelButton: TBitBtn;
    GenderComboBox: TComboBox;
    PatientCadastreMediator: TJSONFormMediator;
    RegistryEdit: TEdit;
    NameEdit: TEdit;
    BirthDatePicker: TZVDateTimePicker;
    SaveButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FPatient: TPatient;
    { private declarations }
  public
    { public declarations }
  published
    property Patient: TPatient read FPatient write FPatient;
  end;

var
  PatientCadastreForm: TPatientCadastreForm;

implementation

{$R *.lfm}

{ TPatientCadastreForm }

procedure TPatientCadastreForm.FormShow(Sender: TObject);
begin
  PatientCadastreMediator.Data := FPatient.Data;
  PatientCadastreMediator.LoadData;
end;

procedure TPatientCadastreForm.SaveButtonClick(Sender: TObject);
begin
  PatientCadastreMediator.SaveData;
  FPatient.Save;
end;

end.

