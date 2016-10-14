program LuiJSONSchemaLCLTestRunner;

{$mode objfpc}{$H+}

uses
  sysutils, Interfaces, Forms, GUITestRunner, JSONSchemaTests, fpjson, LuiJSONUtils, TestFramework;

{$R *.res}

var
  SpecsData: TJSONArray;

procedure RegisterTests;
var
  SpecsDir: String;
  SpecData: TJSONArray;
  SearchRec: TSearchRec;
begin
  SpecsDir := ExpandFileName(SetDirSeparators('JSON-Schema-Test-Suite\tests\draft4\'));

  if FindFirst(SpecsDir + '*.json', faAnyFile, SearchRec) = 0 then
  repeat
    if TryReadJSONFile(SpecsDir + SearchRec.Name, SpecData) then
    begin
      SpecsData.Add(SpecData);
      RegisterTest(TJSONSchemaTestCase.Create(ChangeFileExt(SearchRec.Name, ''), SpecData));
    end;
  until FindNext(SearchRec) <> 0;
end;

begin
  SpecsData := TJSONArray.Create;
  RegisterTests;
  Application.Initialize;
  RunRegisteredTests;
  SpecsData.Destroy;
end.

