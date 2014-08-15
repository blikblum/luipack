unit PatientEvaluationsPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, fpjson, PatientEvaluationModel;

type

  { TPatientEvaluationsPresenter }

  TPatientEvaluationsPresenter = class(TBasePresenter)
  private
    FEvaluations: TPatientEvaluations;
    FPatientData: TJSONObject;
  protected
    function GetViewCaption(const DesignCaption: String): String; override;
    procedure Initialize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Evaluations: TPatientEvaluations read FEvaluations;
  published
    property PatientData: TJSONObject read FPatientData write FPatientData;
  end;


implementation

{ TPatientEvaluationsPresenter }

function TPatientEvaluationsPresenter.GetViewCaption(const DesignCaption: String): String;
begin
  Result := FPatientData.Get('name', '');
end;

procedure TPatientEvaluationsPresenter.Initialize;
begin
  inherited Initialize;
  FEvaluations.ParamByName('patientid').AsLargeInt := FPatientData.Int64s['id'];
  FEvaluations.Fetch;
end;

constructor TPatientEvaluationsPresenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvaluations := TPatientEvaluations.Create;
end;

end.

