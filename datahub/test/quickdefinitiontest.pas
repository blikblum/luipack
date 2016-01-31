unit QuickDefinitionTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, QuickDefinitionParser, FPTestHelpers, fpjson;

type

  { TQuickDefinitionParserTest }

  TQuickDefinitionParserTest = class(TTestCase)
  private
    function ParseDef(const SrcStr: String): TJSONData;
  protected
  published
    procedure OnlyName;
    procedure StringType;
    procedure IntegerType;
    procedure FieldTypes;
  end;

implementation

function TQuickDefinitionParserTest.ParseDef(const SrcStr: String): TJSONData;
var
  Parser: TQuickFieldsDefinitionParser;
  Src: TStrings;
begin
  Src := TStringList.Create;
  Src.Text := SrcStr;
  Parser := nil;
  Result := nil;
  try
    Parser := TQuickFieldsDefinitionParser.Create(Src);
    Result := Parser.Parse;
  finally
    Parser.Free;
    Src.Destroy;
  end;
end;

procedure TQuickDefinitionParserTest.OnlyName;
begin
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'id'])], ParseDef('id'), 'Only field name').FreeActual;
end;

{

 else if AnsiMatchText(S, ['FLOAT', 'REAL']) then
   Result := 'dftFloat'
 else if AnsiMatchText(S, ['STR', 'STRING']) then
   Result := 'dftString'
 else if AnsiMatchText(S, ['MEMO', 'TEXT']) then
   Result := 'dtMemo'
 else if AnsiMatchText(S, ['BOOL', 'BOOLEAN']) then
   Result := 'dftBoolean'
 else if AnsiMatchText(S, ['INT64', 'LARGEINT']) then
   Result := 'dftLargeInt'
 else if AnsiMatchText(S, ['DATE']) then
   Result := 'dftDate'
 else if AnsiMatchText(S, ['DATETIME']) then
   Result := 'dftDateTime'
 else if AnsiMatchText(S, ['TIME']) then
   Result := 'dftTime'
 else if AnsiMatchText(S, ['BLOB']) then
   Result := 'dftBlob'
}
const
  FieldTypesMap: array[0..3] of array[0..1] of String = (
    ('INT', 'dftIntege'),
    ('INTEGER', 'dftInteger'),
    ('STR', 'dftString'),
    ('STRING', 'dftString')
  );

procedure TQuickDefinitionParserTest.StringType;
begin
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftString'])], ParseDef('name STR'), 'Abbr string field');
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftString'])], ParseDef('name STRING'), 'String field');
end;

procedure TQuickDefinitionParserTest.IntegerType;
begin
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftInteger'])], ParseDef('name INT'), 'Abbr Integer field');
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftInteger'])], ParseDef('name INTEGER'), 'Integer field');
end;

procedure TQuickDefinitionParserTest.FieldTypes;
var
  i: Integer;
  Def, Msg: String;
  FieldData: TJSONData;
begin
  for i := Low(FieldTypesMap) to High(FieldTypesMap) do
  begin
    Def := Format('name %s', [FieldTypesMap[i][0]]);
    Msg := Format('Expected %s type for %s', [FieldTypesMap[i][1], FieldTypesMap[i][0]]);
    FieldData := TJSONObject.Create(['FieldName', 'name', 'FieldType', FieldTypesMap[i][1]]);
    CheckEqualsArray([FieldData], ParseDef(Def), Msg).FreeActual;
  end;
end;

initialization
  RegisterTest(TQuickDefinitionParserTest.Suite);
end.

