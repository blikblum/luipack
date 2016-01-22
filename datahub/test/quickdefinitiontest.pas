unit QuickDefinitionTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, QuickDefinitionParser;

type

  { TQuickDefinitionParserTest }

  TQuickDefinitionParserTest = class(TTestCase)
  private
    FParser: TQuickFieldsDefinitionParser;
    FSrc: TStrings;
    procedure CreateParser(const Src: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OnlyName;
  end;

implementation

uses
  LuiJSONUtils, fpjson;

procedure TQuickDefinitionParserTest.CreateParser(const Src: String);
begin
  FSrc := TStringList.Create;
  FSrc.Text := Src;
  FParser := TQuickFieldsDefinitionParser.Create(FSrc);
end;

procedure TQuickDefinitionParserTest.SetUp;
begin
  inherited SetUp;
end;

procedure TQuickDefinitionParserTest.TearDown;
begin
  FParser.Free;
  FSrc.Free;
end;

procedure TQuickDefinitionParserTest.OnlyName;
var
  Data: TJSONData;
begin
  CreateParser('id');
  Data := TJSONArray.Create([TJSONObject.Create(['FieldName', 'id'])]);
  Check(CompareJSONData(FParser.Parse, Data) = 0, 'Only field name');
end;

initialization
  RegisterTest(TQuickDefinitionParserTest.Suite);
end.

