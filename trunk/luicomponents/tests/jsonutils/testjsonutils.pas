unit TestJSONUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson;

type

  { TTestSetJSONPath }

  TTestSetJSONPath = class(TTestCase)
  private
    FData: TJSONData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SetSingleObjectProperty;
    procedure SetNestedObjectProperty;
  end;

implementation

uses
  LuiJSONUtils;

procedure TTestSetJSONPath.SetSingleObjectProperty;
begin
  FData := TJSONObject.Create;
  SetJSONPath(FData, 'prop', TJSONIntegerNumber.Create(2));
  AssertEquals('Item count', 1, FData.Count);
  AssertEquals('Property value', 2, TJSONObject(FData).Get('prop', 0));
end;

procedure TTestSetJSONPath.SetNestedObjectProperty;
var
  ObjData: TJSONObject;
begin
  ObjData := TJSONObject.Create;
  FData := TJSONObject.Create(['a', ObjData]);
  SetJSONPath(FData, 'a.prop', TJSONIntegerNumber.Create(2));
  AssertEquals('Nested object item count', 1, ObjData.Count);
  AssertEquals('Nested object property value', 2, ObjData.Get('prop', 0));
end;

procedure TTestSetJSONPath.SetUp;
begin

end;

procedure TTestSetJSONPath.TearDown;
begin
  FData.Free;
end;

initialization

  RegisterTest(TTestSetJSONPath);
end.

