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
    procedure FieldTypes;
    procedure CompleteDefinition;
    procedure Constraints;
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
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'id', 'FieldType', 'dftString'])],
    ParseDef('id'), 'Only field name, defaults to String type').FreeActual;
end;

const
  FieldTypesMap: array[0..15] of array[0..1] of String = (
    ('INT', 'dftInteger'),
    ('INTEGER', 'dftInteger'),
    ('STR', 'dftString'),
    ('STRING', 'dftString'),
    ('FLOAT', 'dftFloat'),
    ('REAL', 'dftFloat'),
    ('MEMO', 'dftMemo'),
    ('TEXT', 'dftMemo'),
    ('BOOL', 'dftBoolean'),
    ('BOOLEAN', 'dftBoolean'),
    ('LARGEINT', 'dftLargeInt'),
    ('INT64', 'dftLargeInt'),
    ('DATE', 'dftDate'),
    ('DATETIME', 'dftDateTime'),
    ('TIME', 'dftTime'),
    ('BLOB', 'dftBlob')
  );

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

procedure TQuickDefinitionParserTest.CompleteDefinition;
begin
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftString', 'DisplayLabel',
    'Person Name'])],
    ParseDef('name str Person Name'),
    'Definition with all fields');
end;

procedure TQuickDefinitionParserTest.Constraints;
var
  ConstraintsData: TJSONObject;
begin
  ConstraintsData := TJSONObject.Create(['primaryKey', true]);
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'id', 'FieldType', 'dftInteger',
    'Constraints', ConstraintsData.AsJSON])],
    ParseDef('id int' +LineEnding + '  key'),
    'Primary key constraint'
    );
  ConstraintsData.Free;

  ConstraintsData := TJSONObject.Create(['required', true]);
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'name', 'FieldType', 'dftString',
    'Constraints', ConstraintsData.AsJSON])],
    ParseDef('name' +LineEnding + '  required'),
    'Required constraint'
    );
  ConstraintsData.Free;

  ConstraintsData := TJSONObject.Create(['minimum', 2, 'maximum', 4]);
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'age', 'FieldType', 'dftInteger',
    'Constraints', ConstraintsData.AsJSON])],
    ParseDef('age int' + LineEnding + '  min 2' + LineEnding + ' maximum 4'),
    'Minimum and maximum constraints'
    );
  ConstraintsData.Free;

  ConstraintsData := TJSONObject.Create(['minLength', 10, 'maxLength', 1000]);
  CheckEqualsArray([TJSONObject.Create(['FieldName', 'desc', 'FieldType', 'dftMemo',
    'Constraints', ConstraintsData.AsJSON])],
    ParseDef('desc memo' +LineEnding + '  minLength 10' + LineEnding + ' maxLength 1000'),
    'Min/MaxLength constraints'
    );
end;

initialization
  RegisterTest(TQuickDefinitionParserTest.Suite);
end.

