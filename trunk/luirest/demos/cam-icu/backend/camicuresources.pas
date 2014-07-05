unit CAMICUResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqlite3slimconn, sqldb, fpjson;

const

  RES_PATIENT = 1;
  RES_PATIENTS = 2;
  RES_ACTIVEPATIENTS = 3;
  RES_EVALUATION = 10;
  RES_EVALUATIONS = 11;
  RES_PATIENT_EVALUATIONS = 12;
  RES_PATIENT_PREDELIRIC = 13;

type

  { TCAMICUResourceFactory }

  TCAMICUResourceFactory = class(TComponent)
  private
  public
    procedure GetResource(out Resource: TRESTResource; ResourceTag: PtrInt);
  end;

  { TCustomPatientResource }

  TCustomPatientResource = class (TSqldbJSONResource)
  protected
    procedure SetQueryData(Query: TSQLQuery; RequestData, Params: TJSONObject;
      DoPatch: Boolean = False); override;
  end;

implementation

uses
  db;

const
  PatientSelect = 'SELECT Id, Name, Registry, BedNumber, BirthDate, Gender, OriginationId, InternmentDate, InternmentTypeId, IsReInternment, IsReInternment48h, DiagnosticId, Apache2, SAPS3, DischargeDate, DischargeReasonId, VMDuration, HasICC, HasIRC, HasDCPF, HasDPOC, HasHematologyTumor, HasLocoregionalTumor, HasMetastasis, HasHAS, HasDM, HasPreviousIAM, HasAVC, HasVisualDeficit, HasAuditoryDeficit, HasDementia, HasAlcoholism, HasSmoking, HasImmunoSuppression, HasSIDA, HasRheumaticDisorder, HasPsychiatricDisorder' +
    ', (Select Count(*) from PatientEvaluation where PatientEvaluation.PatientId = Patient.Id) as EvaluationCount' +
    ', (Select Risk from PatientPreDeliric where PatientPreDeliric.PatientId = Patient.Id) as PreDeliricRisk FROM Patient';

{ TCustomPatientResource }

procedure TCustomPatientResource.SetQueryData(Query: TSQLQuery; RequestData,
  Params: TJSONObject; DoPatch: Boolean);
var
  F: TField;
begin
  inherited SetQueryData(Query, RequestData, Params, DoPatch);
  F := Query.FindField('EvaluationCount');
  if F <> nil then
    F.Clear;
  F := Query.FindField('PreDeliric');
  if F <> nil then
    F.Clear;
end;

{ TCAMICUResourceFactory }

procedure TCAMICUResourceFactory.GetResource(out Resource: TRESTResource;
  ResourceTag: PtrInt);
var
  SqldbResource: TSqldbJSONResource absolute Resource;
begin
  if Tag in [RES_PATIENT, RES_PATIENTS, RES_ACTIVEPATIENTS] then
    SqldbResource := TCustomPatientResource.Create
  else
    SqldbResource := TSqldbJSONResource.Create;
  case ResourceTag of
    RES_PATIENTS, RES_ACTIVEPATIENTS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.SelectSQL := PatientSelect;
        SqldbResource.SetDefaultSubPath('patientid', @GetResource, RES_PATIENT);
        if ResourceTag = RES_ACTIVEPATIENTS then
          SqldbResource.ConditionsSQL := 'Where DischargeDate IS NULL'
        else
        begin
          SqldbResource.RegisterSubPath('active', @GetResource, RES_ACTIVEPATIENTS);
          SqldbResource.ReadOnly := True;
        end;
      end;
    RES_PATIENT:
      begin
        SqldbResource.SelectSQL := PatientSelect;
        SqldbResource.PrimaryKeyParam := 'patientid';
        SqldbResource.RegisterSubPath('evaluations', @GetResource, RES_PATIENT_EVALUATIONS);
        SqldbResource.RegisterSubPath('predeliric', @GetResource, RES_PATIENT_PREDELIRIC);
      end;
     RES_EVALUATIONS, RES_PATIENT_EVALUATIONS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.SelectSQL := 'Select Id, Date, PatientId, RASS, DeliriumId, VentilationId, Sedation, ShiftId from PatientEvaluation';
        SqldbResource.JSONFields := '[{"name":"sedation","type":"array"}]';
        SqldbResource.SetDefaultSubPath('evaluationid', @GetResource, RES_EVALUATION);
        if ResourceTag = RES_PATIENT_EVALUATIONS then
          SqldbResource.ConditionsSQL := 'Where PatientId = :patientid'
        else
        begin
          SqldbResource.ReadOnly := True;
        end;
      end;
     RES_EVALUATION:
      begin
        SqldbResource.SelectSQL := 'Select Id, PatientId, RASS, DeliriumId, VentilationId, Sedation, ShiftId from PatientEvaluation';
        SqldbResource.PrimaryKeyParam := 'evaluationid';
        SqldbResource.JSONFields := '[{"name":"sedation","type":"array"}]';
      end;
     RES_PATIENT_PREDELIRIC:
      begin
        SqldbResource.IgnoreNotFound := True;
        SqldbResource.SelectSQL := 'Select * from PatientPreDeliric';
        SqldbResource.PrimaryKey := 'PatientId';
        SqldbResource.PrimaryKeyParam := 'patientid';
      end
    else
    begin
      FreeAndNil(SqldbResource);
      raise Exception.CreateFmt('Resource type %d not defined', [ResourceTag]);
    end;
  end;
end;

end.

