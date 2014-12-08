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

const
  ColumnNames: Array[0..38] of String = (
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
    'PreDeliric',
    'Número_Avaliações'
    );

  EvaluationColumnNames: Array[0..3] of String = (
    'T%d_RASS',
    'T%d_Delirium',
    'T%d_Ventilação',
    'T%d_Sedação'
    );


type

  { TCAMICUSpreadSheet }

  TCAMICUSpreadSheet = class
  private
    FData: TJSONArray;
    FMaxEvaluationCount: Integer;
    FPatients: TPatients;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: String);
    property Data: TJSONArray read FData;
    property MaxEvaluationCount: Integer read FMaxEvaluationCount write FMaxEvaluationCount;
    property Patients: TPatients read FPatients write FPatients;
  end;

{ TExportData }

constructor TCAMICUSpreadSheet.Create;
begin
  FData := TJSONArray.Create;
  FMaxEvaluationCount := 40;
end;

destructor TCAMICUSpreadSheet.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

function GetVMDuration(EvaluationsData: TJSONArray): Integer;
var
  i, VentilationId: Integer;
begin
  Result := 0;
  for i := 0 to EvaluationsData.Count - 1 do
  begin
    VentilationId := EvaluationsData.Objects[i].Get('ventilationid', 9);
    if VentilationId = 1 then
      Inc(Result);
  end;
end;

procedure WriteWorksheetRow(Worksheet: TsWorksheet; PatientData: TJSONObject; EvaluationsData: TJSONArray; Row: Integer; MaxEvaluationCount: Integer);
var
  StrValue: String;
  EvaluationCount, i: Integer;
  EvaluationData: TJSONObject;
  EvaluationBaseIndex: Integer;

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

  procedure WriteFloatDef(Col: Integer; const PropName: String; Default: Double);
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
  Worksheet.WriteNumber(Row, 36, GetVMDuration(EvaluationsData));
  WriteFloatDef(37, 'predeliricrisk', 999);

  EvaluationBaseIndex := Length(ColumnNames);
  //evaluations
  EvaluationCount := Min(EvaluationsData.Count, MaxEvaluationCount);
  Worksheet.WriteNumber(Row, 38, EvaluationCount);
  for i := 0 to EvaluationCount - 1 do
  begin
    EvaluationData := EvaluationsData.Objects[i];
    //rass
    Worksheet.WriteNumber(Row, EvaluationBaseIndex + (i * 4), EvaluationData.Get('rass', 9));
    //delirium
    Worksheet.WriteNumber(Row, EvaluationBaseIndex + (i * 4) + 1, EvaluationData.Get('deliriumid', 9));
    //vm
    Worksheet.WriteNumber(Row, EvaluationBaseIndex + (i * 4) + 2, EvaluationData.Get('ventilationid', 9));
    //sedation
    Worksheet.WriteUTF8Text(Row, EvaluationBaseIndex + (i * 4) + 3, GetSedation(EvaluationData));
  end;
end;

procedure TCAMICUSpreadSheet.SaveToFile(const FileName: String);
var
  i, j, k, NumEvaluationColumns: Integer;
  Evaluations: TEvaluations;
  PatientData: TJSONObject;
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
begin
  FData.Clear;
  Evaluations := TEvaluations.Create;
  Evaluations.Fetch;
  Workbook := TsWorkbook.Create;
  Worksheet := Workbook.AddWorksheet('planilha');
  try
    for i := Low(ColumnNames) to High(ColumnNames) do
      Worksheet.WriteUTF8Text(0, i, ColumnNames[i]);
    i := High(ColumnNames) + 1;
    NumEvaluationColumns := Length(EvaluationColumnNames);
    for j := 0 to MaxEvaluationCount - 1 do
    begin
      for k := Low(EvaluationColumnNames) to High(EvaluationColumnNames) do
      begin
        Worksheet.WriteUTF8Text(0, i + (j * NumEvaluationColumns) + k, Format(EvaluationColumnNames[k], [j + 1]));
      end;
    end;

    for i := 0 to FPatients.Data.Count - 1 do
    begin
      PatientData := FPatients.Data.Objects[i];
      Evaluations.FilterByPatient(PatientData.Get('id', -1));
      WriteWorksheetRow(Worksheet, PatientData, Evaluations.FilteredData, i + 1, MaxEvaluationCount);
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

