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
  fpspreadsheet, xlsbiff8, LuiDateUtils, LuiJSONUtils, Math;

type

  { TCAMICUSpreadSheet }

  TCAMICUSpreadSheet = class
  private
    FData: TJSONArray;
    FPatients: TPatients;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: String);
    property Data: TJSONArray read FData;
    property Patients: TPatients read FPatients write FPatients;
  end;

{ TExportData }

constructor TCAMICUSpreadSheet.Create;
begin
  FData := TJSONArray.Create;
end;

destructor TCAMICUSpreadSheet.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure WriteWorksheetRow(Worksheet: TsWorksheet; PatientData: TJSONObject; Evaluations: TJSONCollection; Row: Integer);
const
  EVALUATION_BASE_INDEX = 37;
var
  StrValue: String;
  EvaluationCount, i: Integer;
  EvaluationData: TJSONObject;

  function BooleanToNumber(const PropName: String): Integer;
  begin
    if PatientData.Get(PropName, False) then
      Result := 1
    else
      Result := 2;
  end;

  procedure WriteIntegerDef(Col: Integer; const PropName: String; Default: Integer);
  begin
    Worksheet.WriteNumber(Row, Col, PatientData.Get(PropName, Default));
  end;

  function GetSedation(AEvaluationData: TJSONObject): String;
  var
    SedationData: TJSONArray;
    K: Integer;
  begin
    Result := '';
    if FindJSONProp(AEvaluationData, 'sedation', SedationData) then
    begin
      if SedationData.Count > 0 then
        Result := SedationData.Items[0].AsString;
      for k := 1 to SedationData.Count - 1 do
        Result := Result + ',' + SedationData.Items[k].AsString;
    end;
  end;

begin
  //number
  Worksheet.WriteNumber(Row, 0, PatientData.Get('id', -1));
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
  //internmentdate
  //todo: cache default date
  Worksheet.WriteDateTime(Row, 7, PatientData.Get('internmentdate', StrToDate('09/09/99')));
  //icc
  Worksheet.WriteNumber(Row, 8, BooleanToNumber('hasicc'));
  //irc
  Worksheet.WriteNumber(Row, 9, BooleanToNumber('hasirc'));
  //dcpf
  Worksheet.WriteNumber(Row, 10, BooleanToNumber('hasdcpf'));
  Worksheet.WriteNumber(Row, 11, BooleanToNumber('hasdpoc'));
  Worksheet.WriteNumber(Row, 12, BooleanToNumber('hashematologytumor'));
  Worksheet.WriteNumber(Row, 13, BooleanToNumber('haslocoregionaltumor'));
  Worksheet.WriteNumber(Row, 14, BooleanToNumber('hasvisualdeficit'));
  Worksheet.WriteNumber(Row, 15, BooleanToNumber('hasdm'));
  Worksheet.WriteNumber(Row, 16, BooleanToNumber('hasdhas'));
  Worksheet.WriteNumber(Row, 17, BooleanToNumber('haspreviousiam'));
  Worksheet.WriteNumber(Row, 18, BooleanToNumber('hasalcoholism'));
  Worksheet.WriteNumber(Row, 19, BooleanToNumber('hasavc'));
  Worksheet.WriteNumber(Row, 20, BooleanToNumber('hasmetastasis'));
  Worksheet.WriteNumber(Row, 21, BooleanToNumber('hasauditorydeficit'));
  Worksheet.WriteNumber(Row, 22, BooleanToNumber('hasdementia'));
  Worksheet.WriteNumber(Row, 23, BooleanToNumber('haspsychiatricdisorder'));
  Worksheet.WriteNumber(Row, 24, BooleanToNumber('hassmoking'));
  Worksheet.WriteNumber(Row, 25, BooleanToNumber('hasimmunosuppression'));
  Worksheet.WriteNumber(Row, 26, BooleanToNumber('hassida'));
  Worksheet.WriteNumber(Row, 27, BooleanToNumber('hasrheumaticdisorder'));
  //originid
  WriteIntegerDef(28, 'originationid', 9);
  WriteIntegerDef(29, 'internmenttypeid', 9);
  //diagnosticid
  WriteIntegerDef(30, 'diagnosticid', 9);
  WriteIntegerDef(31, 'apache2', 9);
  WriteIntegerDef(32, 'saps3', 9);

  Worksheet.WriteDateTime(Row, 33, PatientData.Get('dischargedate', StrToDate('09/09/99')));
  WriteIntegerDef(34, 'dischargereasonid', 9);
  //icutime
  if PatientData.Find('dischargedate', jtNumber) <> nil then
    Worksheet.WriteNumber(Row, 35, Trunc(PatientData.Get('dischargedate', 0.0)) - Trunc(PatientData.Get('internmentdate', 0.0)))
  else
    Worksheet.WriteNumber(Row, 35, 999);
  //vm duration
  //todo: use vm info from evaluations?
  WriteIntegerDef(36, 'vmduration', 999);

  //evaluations
  EvaluationCount := Min(Evaluations.Count, 20);
  for i := 0 to EvaluationCount - 1 do
  begin
    EvaluationData := Evaluations[i].Data;
    //rass
    Worksheet.WriteNumber(Row, EVALUATION_BASE_INDEX + (i * 4), EvaluationData.Get('rass', 9));
    //delirium
    Worksheet.WriteNumber(Row, EVALUATION_BASE_INDEX + (i * 4) + 1, EvaluationData.Get('deliriumid', 9));
    //vm
    Worksheet.WriteNumber(Row, EVALUATION_BASE_INDEX + (i * 4) + 2, EvaluationData.Get('ventilationid', 9));
    //sedation
    Worksheet.WriteUTF8Text(Row, EVALUATION_BASE_INDEX + (i * 4) + 3, GetSedation(EvaluationData));
  end;
end;

const
  ColumnNames: Array[0..116] of String = (
    'Número',
    'Nome',
    'Registro',
    'Idade',
    'Sexo',
    'Reinternação',
    'Rei_em_48h',
    'Data_internação',
    'ICC',
    'IRC',
    'Cirrose',
    'DPOC',
    'Tumor_Hematológico',
    'Tumor_Locoregional',
    'Déficit_visual',
    'DM',
    'HAS',
    'IAM_prévio',
    'Alcoolismo',
    'AVC',
    'Tumor_metastático',
    'Déficit_auditivo',
    'Demência',
    'Doença_psiquiátrica',
    'Tabagismo',
    'Imunossupressão_',
    'SIDA',
    'Doença_Reumática',
    'Origem',
    'Tipo_de_internação',
    'Diagnóstico',
    'APACHE_II',
    'SAPS_3',
    'Data_de_alta',
    'Motivo_alta',
    'Tempo_de_UTI',
    'Tempo_de_VM',

    'T1_RASS',
    'T1_Delirium',
    'T1_Ventilação',
    'T1_Sedação',
    'T2_RASS',
    'T2_Delirium',
    'T2_Ventilação',
    'T2_Sedação',
    'T3_RASS',
    'T3_Delirium',
    'T3_Ventilação',
    'T3_Sedação',
    'T4_RASS',
    'T4_Delirium',
    'T4_Ventilação',
    'T4_Sedação',
    'T5_RASS',
    'T5_Delirium',
    'T5_Ventilação',
    'T5_Sedação',
    'T6_RASS',
    'T6_Delirium',
    'T6_Ventilação',
    'T6_Sedação',
    'T7_RASS',
    'T7_Delirium',
    'T7_Ventilação',
    'T7_Sedação',
    'T8_RASS',
    'T8_Delirium',
    'T8_Ventilação',
    'T8_Sedação',
    'T9_RASS',
    'T9_Delirium',
    'T9_Ventilação',
    'T9_Sedação',
    'T10_RASS',
    'T10_Delirium',
    'T10_Ventilação',
    'T10_Sedação',
    'T11_RASS',
    'T11_Delirium',
    'T11_Ventilação',
    'T11_Sedação',
    'T12_RASS',
    'T12_Delirium',
    'T12_Ventilação',
    'T12_Sedação',
    'T13_RASS',
    'T13_Delirium',
    'T13_Ventilação',
    'T13_Sedação',
    'T14_RASS',
    'T14_Delirium',
    'T14_Ventilação',
    'T14_Sedação',
    'T15_RASS',
    'T15_Delirium',
    'T15_Ventilação',
    'T15_Sedação',
    'T16_RASS',
    'T16_Delirium',
    'T16_Ventilação',
    'T16_Sedação',
    'T17_RASS',
    'T17_Delirium',
    'T17_Ventilação',
    'T17_Sedação',
    'T18_RASS',
    'T18_Delirium',
    'T18_Ventilação',
    'T18_Sedação',
    'T19_RASS',
    'T19_Delirium',
    'T19_Ventilação',
    'T19_Sedação',
    'T20_RASS',
    'T20_Delirium',
    'T20_Ventilação',
    'T20_Sedação'
    );

procedure TCAMICUSpreadSheet.SaveToFile(const FileName: String);
var
  i: Integer;
  Evaluations: TPatientEvaluations;
  PatientData: TJSONObject;
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
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
    for i := Low(ColumnNames) to High(ColumnNames) do
      Worksheet.WriteUTF8Text(0, i, ColumnNames[i]);

    for i := 0 to FPatients.Data.Count - 1 do
    begin
      PatientData := FPatients.Data.Objects[i];
      Evaluations.ParamByName('patientid').AsLargeInt := PatientData.Int64s['id'];
      if not Evaluations.Fetch then
        raise Exception.Create('Não foi possível carregar avaliações');
      WriteWorksheetRow(Worksheet, PatientData, Evaluations, i + 1);
    end;
    Workbook.WriteToFile(FileName, sfExcel8, True);
  finally
    Workbook.Free;
    Evaluations.Free;
  end;
end;

{ TCAMICUDataExporter }

class procedure TCAMICUDataExporter.SaveToSpreadSheet(Patients: TPatients;
  const FileName: String);
var
  Sheet: TCAMICUSpreadSheet;
begin
  Sheet := TCAMICUSpreadSheet.Create;
  try
    Sheet.Patients := Patients;
    Sheet.SaveToFile(FileName);
  finally
    Sheet.Destroy;
  end;

end;

end.

