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

  { TFindTestCase }

  TFindTestCase = class(TTestCase)
  published
    procedure FindObjectByProperties;
  end;

  { TMergeTestCase }

  TMergeTestCase = class(TTestCase)
  published
    procedure MergeFlatObject;
    procedure MergeNestedObject;
  end;

implementation

uses
  LuiJSONHelpers, variants, LuiJSONUtils;

{ TMergeTestCase }

procedure TMergeTestCase.MergeFlatObject;
var
  Data1, Data2, Expected: TJSONObject;
begin
  Data1 := TJSONObject.Create([
    'prop1', 'x',
    'prop3', nil,
    'prop4', 'y'
  ]);

  Data2 := TJSONObject.Create([
    'prop1', 'a',
    'prop2', 'z',
    'prop3', 'x',
    'prop4', 'y',
    'prop5', nil
  ]);

  Data1.Merge(Data2);

  Expected := TJSONObject.Create([
    'prop1', 'x',
    'prop2', 'z',
    'prop3', nil,
    'prop4', 'y',
    'prop5', nil
  ]);

  CheckTrue(CompareJSONData(Data1, Expected) = 0, 'Merged flat object');

  Expected.Free;
  Data1.Free;
  Data2.Free;
end;

procedure TMergeTestCase.MergeNestedObject;
var
  Data1, Data2, Expected: TJSONObject;
begin
  Data1 := TJSONObject.Create([
    'prop1', 'x',
    'prop3', nil,
    'prop4', TJSONObject.Create([
      'x', 1,
      'z', 3
    ])
  ]);

  Data2 := TJSONObject.Create([
    'prop1', TJSONObject.Create(['y', 1]),
    'prop2', TJSONObject.Create(['z', 1]),
    'prop3', 'z',
    'prop4', TJSONObject.Create([
      'x', 2,
      'y', 10,
      'z', 3
    ])
  ]);

  Data1.Merge(Data2);

  Expected := TJSONObject.Create([
    'prop1', 'x',
    'prop2', TJSONObject.Create(['z', 1]),
    'prop3', nil,
    'prop4', TJSONObject.Create([
      'x', 1,
      'y', 10,
      'z', 3
    ])
  ]);

  CheckTrue(CompareJSONData(Data1, Expected) = 0, 'Merged nested object');

  Expected.Free;
  Data1.Free;
  Data2.Free;
end;

{ TFindTestCase }

procedure TFindTestCase.FindObjectByProperties;
var
  ArrayData: TJSONArray;
  ItemData: TJSONObject;
begin
  ArrayData := TJSONArray.Create([
    TJSONObject.Create([
      'x', '1',
      'y', 'a'
    ]),
    TJSONObject.Create([
      'x', 1,
      'y', 'b'
    ])
  ]);
  try
    CheckTrue(ArrayData.Find(['x', '1'], ItemData));
    CheckNotNull(ItemData);
    CheckTrue(ArrayData.Find(['x', 1], ItemData));
    CheckNotNull(ItemData);
    ArrayData.Find(['x', '1'], ItemData);
    CheckEquals('1', ItemData.Get('x', ''));
    CheckEquals('a', ItemData.Get('y', ''));
    ArrayData.Find(['x', 1], ItemData);
    CheckEquals(1, ItemData.Get('x', 1));
    CheckEquals('b', ItemData.Get('y', ''));
    CheckFalse(ArrayData.Find(['y', 1], ItemData));
    CheckNull(ItemData);
  finally
    ArrayData.Create;
  end;
end;

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
  ProjectRegisterTests('JSONHelpers', [TIndexOfTestCase.Suite, TFindPathTestCase.Suite,
    TFindTestCase.Suite, TMergeTestCase.Suite]);

end.

