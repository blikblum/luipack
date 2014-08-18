unit EvaluationPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, PatientEvaluationModel, fpjson;

type

  { TEvaluationPresenter }

  TEvaluationPresenter = class(TBasePresenter)
  private
    FCollection: TPatientEvaluations;
    FEvaluation: TPatientEvaluation;
  public
    procedure SaveEvaluation;
  published
    property Collection: TPatientEvaluations read FCollection write FCollection;
    property Evaluation: TPatientEvaluation read FEvaluation write FEvaluation;
  end;

implementation

{ TEvaluationPresenter }

procedure TEvaluationPresenter.SaveEvaluation;
begin
  if FCollection <> nil then
    FCollection.SaveItem(FEvaluation)
  else
    Evaluation.Save;
end;

end.

