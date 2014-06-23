unit CAMICUResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, LuiRESTSqldb, sqlite3slimconn, sqldb;

const

  RES_PATIENT = 1;
  RES_PATIENTS = 2;
  RES_ACTIVEPATIENTS = 3;
  RES_EVALUATION = 10;
  RES_EVALUATIONS = 11;
  RES_PATIENT_EVALUATIONS = 12;

type

  { TCAMICUResourceFactory }

  TCAMICUResourceFactory = class(TComponent)
  private
  public
    procedure GetResource(out Resource: TRESTResource; ResourceTag: PtrInt);
  end;

implementation

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
        SqldbResource.SelectSQL := 'SELECT Id, Name, Registry, BirthDate, Gender, OriginationId, InternmentDate, InternmentTypeId, IsReInternment, IsReInternment48h, DiagnosticId, Apache2, SAPS3, DischargeDate, DischargeReasonId, VMDuration, HasICC, HasIRC, HasDCPF, HasDPOC, HasHematologyTumor, HasLocoregionalTumor, HasMetastasis, HasHAS, HasDM, HasPreviousIAM, HasAVC, HasVisualDeficit, HasAuditoryDeficit, HasDementia, HasAlcoholism, HasSmoking, HasImmunoSuppression,  HasSIDA,  HasRheumaticDisorder,  HasPsychiatricDisorder FROM Patient';
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
        SqldbResource.SelectSQL := 'SELECT Id, Name, Registry, BirthDate, Gender, OriginationId, InternmentDate, InternmentTypeId, IsReInternment, IsReInternment48h, DiagnosticId, Apache2, SAPS3, DischargeDate, DischargeReasonId, VMDuration, HasICC, HasIRC, HasDCPF, HasDPOC, HasHematologyTumor, HasLocoregionalTumor, HasMetastasis, HasHAS, HasDM, HasPreviousIAM, HasAVC, HasVisualDeficit, HasAuditoryDeficit, HasDementia, HasAlcoholism, HasSmoking, HasImmunoSuppression,  HasSIDA,  HasRheumaticDisorder,  HasPsychiatricDisorder FROM Patient';
        SqldbResource.PrimaryKeyParam := 'patientid';
        SqldbResource.RegisterSubPath('evaluations', @GetResource, RES_PATIENT_EVALUATIONS);
      end;
     RES_EVALUATIONS, RES_PATIENT_EVALUATIONS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.SelectSQL := 'Select Id, Date, PatientId, RASS, DeliriumId, VentilationId, SedationId from PatientEvaluation';
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
        SqldbResource.SelectSQL := 'Select Id, PatientId, RASS, DeliriumId, VentilationId, SedationId from PatientEvaluation';
        SqldbResource.PrimaryKeyParam := 'evaluationid';
      end;
    else
    begin
      FreeAndNil(SqldbResource);
      raise Exception.CreateFmt('Resource type %d not defined', [ResourceTag]);
    end;
  end;
end;

end.

