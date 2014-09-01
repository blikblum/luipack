unit DataExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PatientModel, fpjson;

type

  { TCAMICUDataExporter }

  TCAMICUDataExporter = class
  private

  public
    class procedure SaveToSpreadSheet(Patients: TPatients; const FileName: String);
  end;

implementation

uses
  LuiJSONModel, EvaluationModel, PatientEvaluationModel,
  fpspreadsheet, xlsbiff8, LuiDateUtils, LuiJSONUtils;

type

  { TExportData }

  TExportData = class
  private
    FData: TJSONArray;
    FPatients: TPatients;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    property Data: TJSONArray read FData;
    property Patients: TPatients read FPatients write FPatients;
  end;

{ TExportData }

constructor TExportData.Create;
begin
  FData := TJSONArray.Create;
end;

destructor TExportData.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure WriteWorksheetRow(Worksheet: TsWorksheet; PatientData: TJSONObject; Row: Integer);
var
  StrValue: String;

  function BooleanToNumber(const PropName: String): Integer;
  begin
    if PatientData.Get(PropName, False) then
      Result := 1
    else
      Result := 2;
  end;

begin
  //number
  Worksheet.WriteUTF8Text(Row, 0, IntToStr(Row));
  //name
  Worksheet.WriteUTF8Text(Row, 1, PatientData.Get('name', ''));
  //register
  Worksheet.WriteUTF8Text(Row, 2, PatientData.Get('registry', ''));
  //age
  Worksheet.WriteNumber(Row, 3, CalculateAge(PatientData.Get('birthdate', 0.0), PatientData.Get('internmentdate', 0.0)));
  //gender
  if FindJSONProp(PatientData, 'gender', StrValue) then
  begin
    if StrValue = 'M' then
      Worksheet.WriteNumber(Row, 4, 1)
    else if StrValue = 'F' then
      Worksheet.WriteNumber(Row, 4, 2);
  end;
  //reinternment
  Worksheet.WriteNumber(Row, 5, BooleanToNumber('isreinternment'));
  //reinternment48
  Worksheet.WriteNumber(Row, 6, BooleanToNumber('isreinternment48h'));

end;

procedure TExportData.Load;
var
  i: Integer;
  Evaluations: TPatientEvaluations;
  PatientData: TJSONObject;
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  Col, Row: Integer;
begin
  FData.Clear;
  //todo: fetch all evaluations once. Need to add feature to filter collection
  {
  Evaluations := TJSONCollection.Create(TEvaluation);
  Evaluations.Fetch;
  }
  Workbook := TsWorkbook.Create;
  Worksheet := Workbook.AddWorksheet('planilha');
  Evaluations := TPatientEvaluations.Create;
  try
    for i := 0 to FPatients.Data.Count - 1 do
    begin
      PatientData := FPatients.Data.Objects[i];


    end;

  finally
    Workbook.Free;
    Evaluations.Free;
  end;
end;

{ TCAMICUDataExporter }

class procedure TCAMICUDataExporter.SaveToSpreadSheet(Patients: TPatients;
  const FileName: String);
begin

end;

end.

