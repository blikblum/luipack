unit EvaluationModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiJSONModel, fpjson;

type

  { TEvaluation }

  TEvaluation = class(TJSONModel)
  protected
    class function GetResourceName: String; override;
  end;

  { TEvaluations }

  TEvaluations = class(TJSONCollection)
  private
    FFilteredData: TJSONArray;
  protected
    class function GetItemClass: TJSONModelClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FilterByPatient(PatientId: Integer);
    property FilteredData: TJSONArray read FFilteredData;
  end;

implementation

uses
  fpexprpars;

{ TEvaluations }

class function TEvaluations.GetItemClass: TJSONModelClass;
begin
  Result := TEvaluation;
end;

procedure TEvaluations.FilterByPatient(PatientId: Integer);
var
  ItemData: TJSONObject;
  i: Integer;
begin
  FFilteredData.Clear;
  for i := 0 to Data.Count - 1 do
  begin
    ItemData := Data.Objects[i];
    if ItemData.Get('patientid', PatientId + 1) = PatientId then
      FFilteredData.Add(ItemData.Clone);
  end;
end;

constructor TEvaluations.Create;
begin
  inherited Create;
  FFilteredData := TJSONArray.Create;
end;

destructor TEvaluations.Destroy;
begin
  FFilteredData.Destroy;
  inherited Destroy;
end;

{ TEvaluation }

class function TEvaluation.GetResourceName: String;
begin
  Result := 'evaluation';
end;

end.

