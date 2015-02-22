program MustacheTestsGenerator;

{$mode objfpc}{$H+}

uses
  Classes, fpjson, sysutils, LuiJSONUtils, strutils;

function GetTestName(TestData: TJSONObject): String;
begin
  Result := TestData.Strings['name'];
  Result := DelChars(Result, ' ');
  Result := DelChars(Result, '-');
  Result := DelChars(Result, '(');
  Result := DelChars(Result, ')');
end;

function NormalizeJSONString(const Str: String): String;
begin
  Result := StringReplace(Str, '''', '''''', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '''+ LineEnding +''', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '''+ LineEnding +''', [rfReplaceAll]);
end;

function CreateClassDeclaration(const TestCaseName: String; SpecData: TJSONObject): String;
var
  TestsData: TJSONArray;
  TestName: String;
  i: Integer;
begin
  Result := '  ' + TestCaseName + ' = class(THandlebarsTestCase)' + LineEnding;
  Result := Result + '  published' + LineEnding;
  if FindJSONProp(SpecData, 'tests', TestsData) then
  begin
    for i := 0 to TestsData.Count - 1 do
    begin
      TestName := GetTestName(TestsData.Objects[i]);
      Result := Result + '    procedure Test' + TestName + ';' + LineEnding;
    end;
  end;
  Result := Result + '  end;' + LineEnding;
end;

function CreateClassImplementation(const TestCaseName: String; SpecData: TJSONObject): String;
var
  TestsData: TJSONArray;
  TestData, TemplateData: TJSONObject;
  TestName: String;
  i: Integer;
begin
  Result := '';
  if FindJSONProp(SpecData, 'tests', TestsData) then
  begin
    for i := 0 to TestsData.Count - 1 do
    begin
      TestData := TestsData.Objects[i];
      TemplateData := TestData.Objects['data'];
      TestName := GetTestName(TestData);
      Result := Result + Format(LineEnding + 'procedure %s.Test%s;' + LineEnding, [TestCaseName, TestName]);
      Result := Result + 'begin' +LineEnding;
      Result := Result + Format('  CheckRender(''%s'',''%s'',''%s'')',
        [NormalizeJSONString(TestData.Strings['template']),
        TemplateData.AsJSON,
        NormalizeJSONString(TestData.Strings['expected'])]);
      Result := Result + LineEnding + 'end;' + LineEnding;
    end;
  end;
end;


const
  SpecFolder = 'mustache-specs';

var
  SpecData: TJSONObject;
  SearchRec: TSearchRec;
  DeclarationSrc, ImplementationSrc, InitializationSrc, UnitSrc: TStringList;
  TestCaseName: String;

begin
  if not DirectoryExists(SpecFolder) then
  begin
    WriteLn(Format('SpecFolder "%s" not found', [SpecFolder]));
    Exit;
  end;
  DeclarationSrc := TStringList.Create;
  ImplementationSrc := TStringList.Create;
  UnitSrc := TStringList.Create;
  InitializationSrc := TStringList.Create;
  try
    if FindFirst(SpecFolder + PathDelim + '*.json', faAnyFile, SearchRec) = 0 then
    repeat
      if TryReadJSONFile(SpecFolder + PathDelim + SearchRec.Name, SpecData) then
      begin
        TestCaseName := AnsiProperCase(DelChars(ChangeFileExt(SearchRec.Name, ''), '~'), [' ']);
        TestCaseName := Format('T%sTests', [TestCaseName]);
        DeclarationSrc.Add(CreateClassDeclaration(TestCaseName, SpecData));
        ImplementationSrc.Add(CreateClassImplementation(TestCaseName, SpecData));
        InitializationSrc.Add(Format('  RegisterTest(''Mustache'', %s.Suite);', [TestCaseName]));
        SpecData.Destroy;
      end;
    until FindNext(SearchRec) <> 0;
    //interface
    UnitSrc.Add('unit MustacheTests;' + LineEnding);
    UnitSrc.Add('{$mode objfpc}{$H+}' + LineEnding);
    UnitSrc.Add('interface' + LineEnding);
    UnitSrc.Add('uses' + LineEnding + '  TestFramework, HandlebarsTestCase;' + LineEnding);
    UnitSrc.Add('type');
    UnitSrc.AddStrings(DeclarationSrc);
    //implementation
    UnitSrc.Add('implementation');
    UnitSrc.AddStrings(ImplementationSrc);
    UnitSrc.Add('initialization');
    UnitSrc.AddStrings(InitializationSrc);
    UnitSrc.Add(LineEnding + 'end.');
    UnitSrc.SaveToFile('mustachetests.pas');
  finally
    InitializationSrc.Destroy;
    UnitSrc.Destroy;
    ImplementationSrc.Destroy;
    DeclarationSrc.Destroy;
  end;
end.

