unit CAMICUResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqlite3slimconn, sqldb,
  HTTPDefs, fpjson;

const

  RES_PATIENT = 1;
  RES_PATIENTS = 2;
  RES_ACTIVEPATIENTS = 3;
  RES_EVALUATION = 10;
  RES_EVALUATIONS = 11;
  RES_PATIENT_EVALUATIONS = 12;
  RES_PATIENT_PREDELIRIC = 13;

type

  { TServiceInfoResource }

  TServiceInfoResource = class(TRESTResource)
  public
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TCAMICUResourceFactory }

  TCAMICUResourceFactory = class(TComponent)
  private
  public
    procedure GetResource(out Resource: TRESTResource; ResourceTag: PtrInt);
  end;

implementation

uses
  sqlite3dyn,
  LuiJSONUtils;

const
  PatientSelect = 'SELECT Id, Name, Registry, BedNumber, BirthDate, Gender, OriginationId, InternmentDate, InternmentTypeId, IsReInternment, IsReInternment48h, DiagnosticId, Apache2, SAPS3, DischargeDate, DischargeReasonId, VMDuration, HasICC, HasIRC, HasDCPF, HasDPOC, HasHematologyTumor, HasLocoregionalTumor, HasMetastasis, HasHAS, HasDM, HasPreviousIAM, HasAVC, HasVisualDeficit, HasAuditoryDeficit, HasDementia, HasAlcoholism, HasSmoking, HasImmunoSuppression, HasSIDA, HasRheumaticDisorder, HasPsychiatricDisorder' +
    ', (Select Count(*) from PatientEvaluation where PatientEvaluation.PatientId = Patient.Id) as EvaluationCount' +
    ', (Select Risk from PatientPreDeliric where PatientPreDeliric.PatientId = Patient.Id) as PreDeliricRisk FROM Patient';

{ TServiceInfoResource }

procedure TServiceInfoResource.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
var
  ResponseData: TJSONObject;
begin
  //todo: remove this as soon get rid of sqldb
  InitialiseSQLite('libsqlite3.so.0');
  if not TryReadJSONFile('serviceinfo.json', ResponseData) then
    ResponseData := TJSONObject.Create;
  ResponseData.Strings['sqliteversion'] := StrPas(sqlite3_libversion());
  AResponse.Contents.Text := ResponseData.AsJSON;
  ResponseData.Destroy;
end;

{ TCAMICUResourceFactory }

procedure TCAMICUResourceFactory.GetResource(out Resource: TRESTResource;
  ResourceTag: PtrInt);
var
  SqldbResource: TSqldbJSONResource absolute Resource;
begin
  SqldbResource := TSqldbJSONResource.Create;
  case ResourceTag of
    RES_PATIENTS, RES_ACTIVEPATIENTS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.SelectSQL := PatientSelect;
        SqldbResource.ConditionsSQL := 'Order By BedNumber';
        SqldbResource.InputFields := '{"exclude":["predeliricrisk","evaluationcount"]}';
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
        SqldbResource.InputFields := '{"exclude":["predeliricrisk","evaluationcount"]}';
        SqldbResource.RegisterSubPath('evaluations', @GetResource, RES_PATIENT_EVALUATIONS);
        SqldbResource.RegisterSubPath('predeliric', @GetResource, RES_PATIENT_PREDELIRIC);
      end;
     RES_EVALUATIONS, RES_PATIENT_EVALUATIONS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.SelectSQL := 'Select Id, Date, PatientId, RASS, DeliriumId, VentilationId, Sedation, ShiftId, ICDSC from PatientEvaluation';
        SqldbResource.JSONFields := '[{"name":"sedation","type":"array"},{"name":"icdsc","type":"array"}]';
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
        SqldbResource.SelectSQL := 'Select Id, Date, PatientId, RASS, DeliriumId, VentilationId, Sedation, ShiftId, ICDSC from PatientEvaluation';
        SqldbResource.PrimaryKeyParam := 'evaluationid';
        SqldbResource.JSONFields := '[{"name":"sedation","type":"array"},{"name":"icdsc","type":"array"}]';
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

initialization
  SQLiteLibraryName := 'libsqlite3.so.0';

end.

