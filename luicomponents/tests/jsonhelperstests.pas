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

  { TFindPathTestCase }

  TFindPathTestCase = class(TTestCase)
  private
    FData: TJSONData;
    procedure CreateData(const DataStr: String);
    procedure CheckEquals(Expected, Actual: TJSONtype); overload;
  protected
    procedure TearDown; override;
  published
    procedure FilterType;
  end;

implementation

uses
  LuiJSONHelpers, variants;

{ TFindPathTestCase }

procedure TFindPathTestCase.CreateData(const DataStr: String);
begin
  FData.Free;
  FData := GetJSON(DataStr);
end;

procedure TFindPathTestCase.CheckEquals(Expected, Actual: TJSONtype);
var
  ActualStr, ExpectedStr: String;
begin
  OnCheckCalled;
  if Expected <> Actual then
  begin
    WriteStr(ActualStr, Actual);
    WriteStr(ExpectedStr, Expected);
    FailNotEquals(ExpectedStr, ActualStr);
  end;
end;

procedure TFindPathTestCase.TearDown;
begin
  FreeAndNil(FData);
  inherited TearDown;
end;

procedure TFindPathTestCase.FilterType;
begin
  CreateData('{"root": {"int": 1, "str": "x", "bool": true, "null": null}}');
  CheckEquals(jtNumber, FData.FindPath('root.int', jtNumber).JSONType);
  CheckEquals(1, FData.FindPath('root.int', jtNumber).AsInteger);
  CheckNull(FData.FindPath('root.int', jtString));

  CheckEquals(jtString, FData.FindPath('root.str', jtString).JSONType);
  CheckEquals('x', FData.FindPath('root.str', jtString).AsString);
  CheckNull(FData.FindPath('root.str', jtBoolean));

  CheckEquals(jtBoolean, FData.FindPath('root.bool', jtBoolean).JSONType);
  CheckTrue(FData.FindPath('root.bool', jtBoolean).AsBoolean);
  CheckNull(FData.FindPath('root.bool', jtString));

  CheckEquals(jtNull, FData.FindPath('root.null', jtNull).JSONType);
  CheckNull(FData.FindPath('root.null', jtBoolean));
end;


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
  ProjectRegisterTests('JSONHelpers', [TIndexOfTestCase.Suite, TFindPathTestCase.Suite]);

end.

