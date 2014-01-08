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
    RES_PATIENTS:
      begin
        SqldbResource.IsCollection := True;
        SqldbResource.ReadOnly := True;
        SqldbResource.SelectSQL := 'Select Id, Name, BirthDate, Registry From Patient';
        SqldbResource.SetDefaultSubPath('patientid', @GetResource, RES_PATIENT);
      end;
    RES_PATIENT:
      begin
        SqldbResource.SelectSQL := 'SELECT Id, Name, Registry, BirthDate, Gender, OriginationId, InternmentDate, InternmentTypeId, IsReInternment, IsReInternment48h, DiagnosticId, Apache2, SAPS3, DischargeDate, DischargeReasonId, VMDuration, HasICC, HasIRC, HasDCPF, HasDPOC, HasHematologyTumor, HasLocoregionalTumor, HasMetastasis, HasHAS, HasDM, HasPreviousIAM, HasAVC, HasVisualDeficit, HasAuditoryDeficit, HasDementia, HasAlcoholism, HasSmoking, HasImmunoSuppression,  HasSIDA,  HasRheumaticDisorder,  HasPsychiatricDisorder FROM Patient';
        SqldbResource.ConditionsSQL := 'Where Id = :patientid';
        SqldbResource.PrimaryKeyParam := 'patientid';
      end;
  end;
end;

end.

