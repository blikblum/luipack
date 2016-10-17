unit JSONSchemaTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFrameworkIfaces, TestFramework, fpjson, LuiJSONSchema;

type

  { TJSONSchemaTestCase }

  TJSONSchemaTestCase = class(TTestCase)
  private
    FSpecData: TJSONArray;
    procedure EnumerateTests;
  protected
  public
    constructor Create(const SpecName: String; SpecData: TJSONArray);
  end;


  { TJSONSchemaTestProc }

  TJSONSchemaTestProc = class(TTestProc)
  private
    FData: TJSONObject;
    procedure ExecuteTest(SchemaData, TestData: TJSONObject);
    procedure ExecuteTests;
  public
    constructor Create(Data: TJSONObject);
  end;


implementation

{ TJSONSchemaTestProc }

procedure TJSONSchemaTestProc.ExecuteTest(SchemaData, TestData: TJSONObject);
var
  Description: String;
  ValidateResult: Boolean;
begin
  Description := TestData.Get('description', '');
  ValidateResult := ValidateJSON(TestData.Elements['data'], SchemaData);
  if TestData.Booleans['valid'] then
    CheckTrue(ValidateResult, Description)
  else
    CheckFalse(ValidateResult, Description);
end;

procedure TJSONSchemaTestProc.ExecuteTests;
var
  SchemaData: TJSONObject;
  TestsData: TJSONArray;
  i: Integer;
begin
  SchemaData := FData.Objects['schema'];
  TestsData := FData.Arrays['tests'];
  for i := 0 to TestsData.Count - 1 do
    ExecuteTest(SchemaData, TestsData.Objects[i]);
end;

constructor TJSONSchemaTestProc.Create(Data: TJSONObject);
begin
  inherited Create(@ExecuteTests, '', @ExecuteTests, Data.Get('description', 'jsonschema-test'));
  FData := Data;
end;

procedure TJSONSchemaTestCase.EnumerateTests;
var
  i: Integer;
begin
  for i := 0 to FSpecData.Count - 1 do
    FTestIterator.AddTest(TJSONSchemaTestProc.Create(FSpecData.Objects[i]));
end;

constructor TJSONSchemaTestCase.Create(const SpecName: String;
  SpecData: TJSONArray);
begin
  inherited Create(SpecName);
  FSpecData := SpecData;
  DisplayedName := SpecName;
  EnumerateTests;
end;


end.

