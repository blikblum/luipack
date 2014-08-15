unit PatientEvaluationModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiJSONModel, fpjson;

type

  { TPatientEvaluation }

  TPatientEvaluation = class(TJSONModel)
  protected
    class function GetResourceName: String; override;
  end;

  { TPatientEvaluations }

  TPatientEvaluations = class(TJSONCollection)
  private
    function GetItem(Index: Integer): TPatientEvaluation;
  protected
    class function GetItemClass: TJSONModelClass; override;
  public
    function CreateItem: TPatientEvaluation;
    function Find(const Id: Variant): TPatientEvaluation;
    function Get(ItemData: TJSONObject): TPatientEvaluation;
    property Items[Index: Integer]: TPatientEvaluation read GetItem; default;
  end;

implementation

{ TPatientEvaluation }

class function TPatientEvaluation.GetResourceName: String;
begin
  Result := 'patientevaluation';
end;

{ TPatientEvaluations }

function TPatientEvaluations.GetItem(Index: Integer): TPatientEvaluation;
begin
  Result := TPatientEvaluation(inherited GetItem(Index));
end;

class function TPatientEvaluations.GetItemClass: TJSONModelClass;
begin
  Result := TPatientEvaluation;
end;

function TPatientEvaluations.CreateItem: TPatientEvaluation;
begin
  Result := TPatientEvaluation(inherited CreateItem);
end;

function TPatientEvaluations.Find(const Id: Variant): TPatientEvaluation;
begin
  Result := TPatientEvaluation(inherited Find(Id));
end;

function TPatientEvaluations.Get(ItemData: TJSONObject): TPatientEvaluation;
begin
  Result := TPatientEvaluation(inherited Get(ItemData));
end;

end.

