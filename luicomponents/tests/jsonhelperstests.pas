unit JSONHelpersTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, fpjson;

type

  { TIndexOfTestCase }

  TIndexOfTestCase = class(TTestCase)
  private
    FArray: TJSONArray;
    procedure CreateArray(const Items: array of const);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SearchNull;
    procedure SearchPrimitiveTypes;
    procedure SearchJSONData;
    procedure SearchObjectItems;
  end;

implementation

uses
  LuiJSONHelpers, variants;


{ TIndexOfTestCase }

procedure TIndexOfTestCase.CreateArray(const Items: array of const);
begin
  FArray.Free;
  FArray := TJSONArray.Create(Items);
end;

procedure TIndexOfTestCase.SetUp;
begin

end;

procedure TIndexOfTestCase.TearDown;
begin
  FreeAndNil(FArray);
  inherited TearDown;
end;

procedure TIndexOfTestCase.SearchNull;
var
  V: Variant;
begin
  CreateArray([1, nil, 2, nil]);
  CheckEquals(1, FArray.IndexOf(Null));
  V := nil;
  CheckEquals(1, FArray.IndexOf(V));

  CreateArray([0, '', 1]);
  CheckEquals(-1, FArray.IndexOf(Null));
  V := nil;
  CheckEquals(-1, FArray.IndexOf(nil));
end;

procedure TIndexOfTestCase.SearchPrimitiveTypes;
begin
  CreateArray([1, TJSONIntegerNumber.Create(1),'x',TJSONIntegerNumber.Create(2), 2, '', true, '2']);
  CheckEquals(0, FArray.IndexOf(1));
  CheckEquals(2, FArray.IndexOf('x'));
  CheckEquals(3, FArray.IndexOf(2));
  CheckEquals(5, FArray.IndexOf(''));
  CheckEquals(6, FArray.IndexOf(True));
  CheckEquals(7, FArray.IndexOf('2'));
end;

procedure TIndexOfTestCase.SearchJSONData;
var
  NumData, OtherNumData: TJSONNumber;
  ObjData: TJSONObject;
begin
  //ensure no conflicts with TJSONArray method
  NumData := TJSONIntegerNumber.Create(1);
  OtherNumData := TJSONIntegerNumber.Create(1);
  ObjData := TJSONObject.Create(['x', 1]);
  CreateArray([1, 2, NumData, '', ObjData, '2']);
  CheckEquals(-1, FArray.IndexOf(OtherNumData));
  CheckEquals(2, FArray.IndexOf(NumData));
  CheckEquals(4, FArray.IndexOf(ObjData));
  OtherNumData.Destroy;
end;

procedure TIndexOfTestCase.SearchObjectItems;
begin
  CreateArray([
    TJSONObject.Create(['prop', 1]),
    TJSONArray.Create(['prop']),
    TJSONIntegerNumber.Create(2),
    'prop',
    2,
    TJSONObject.Create(['x', 1, 'prop', 2]),
    TJSONObject.Create(['prop', 3])
  ]);
  CheckEquals(0, FArray.IndexOf(['prop', 1]));
  CheckEquals(5, FArray.IndexOf(['prop', 2]));
  CheckEquals(-1, FArray.IndexOf(['unknownprop', 2]));
  CheckEquals(-1, FArray.IndexOf(['prop', '2']));
end;

initialization
  ProjectRegisterTests('JSONHelpers', [TIndexOfTestCase.Suite]);

end.

