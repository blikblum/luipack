unit JSONUtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, fpjson;

type

  { TCompareDataTestCase }

  TCompareDataTestCase = class(TTestCase)
  published
    procedure CompareNull;
    procedure CompareInteger;
    procedure CompareObject;
    procedure CompareArray;
    procedure CompareComplex;
  end;

implementation

uses
  LuiJSONUtils;

procedure TCompareDataTestCase.CompareNull;
begin
  CheckEquals(0, CompareJSONData(TJSONNull.Create, TJSONNull.Create), 'Null should be equal to null');
end;

procedure TCompareDataTestCase.CompareInteger;
begin
  CheckEquals(0, CompareJSONData(TJSONIntegerNumber.Create(1), TJSONIntegerNumber.Create(1)), '1 should be equal to 1');
  CheckEquals(-1, CompareJSONData(TJSONIntegerNumber.Create(1), TJSONIntegerNumber.Create(2)), '1 should be less then 2');
  CheckEquals(1, CompareJSONData(TJSONIntegerNumber.Create(2), TJSONIntegerNumber.Create(1)), '2 should be less then 1');
end;

procedure TCompareDataTestCase.CompareObject;
var
  Obj1, Obj2: TJSONObject;
begin
  //single property
  Obj1 := TJSONObject.Create(['name', 'test']);
  Obj2 := TJSONObject.Create(['name', 'test']);
  CheckEquals(0, CompareJSONData(Obj1, Obj2), 'Objects with only one equal property');

  Obj1 := TJSONObject.Create(['name', 'test', 'id', 0]);
  Obj2 := TJSONObject.Create(['name', 'test', 'id', 0]);
  CheckEquals(0, CompareJSONData(Obj1, Obj2), 'Objects with two properties, same order');

  Obj1 := TJSONObject.Create(['id', 0, 'name', 'test']);
  Obj2 := TJSONObject.Create(['name', 'test', 'id', 0]);
  CheckEquals(0, CompareJSONData(Obj1, Obj2), 'Objects with two properties, different order');

  Obj1 := TJSONObject.Create(['name', 'test']);
  Obj2 := TJSONObject.Create(['name', 'test', 'id', 0]);
  CheckNotEquals(0, CompareJSONData(Obj1, Obj2), 'Objects with different number of properties');

  Obj1 := TJSONObject.Create(['name', 'testX', 'id', 0]);
  Obj2 := TJSONObject.Create(['name', 'test', 'id', 0]);
  CheckNotEquals(0, CompareJSONData(Obj1, Obj2), 'Objects with same properties, different values');
end;

procedure TCompareDataTestCase.CompareArray;
var
  Array1, Array2: TJSONArray;
begin
  Array1 := TJSONArray.Create([1]);
  Array2 := TJSONArray.Create([1]);
  CheckEquals(0, CompareJSONData(Array1, Array2), 'Arrays with one item of equal value');

  Array1 := TJSONArray.Create(['a', 1]);
  Array2 := TJSONArray.Create(['a', 1]);
  CheckEquals(0, CompareJSONData(Array1, Array2), 'Arrays with number of items equals and values equals');

  Array1 := TJSONArray.Create([1]);
  Array2 := TJSONArray.Create([1, 1]);
  CheckNotEquals(0, CompareJSONData(Array1, Array2), 'Arrays with number of items differents and values equals');

  Array1 := TJSONArray.Create(['a', 'b']);
  Array2 := TJSONArray.Create(['b', 'a']);
  CheckNotEquals(0, CompareJSONData(Array1, Array2), 'Arrays with number of items equals and values in different positions');
end;

procedure TCompareDataTestCase.CompareComplex;
var
  Data1, Data2: TJSONData;
begin
  Data1 := TJSONObject.Create([
    'name', 'Luiz',
    'id', 0,
    'items', TJSONArray.Create([
       'a', 1, nil
      ])
  ]);
  //same object, prop order different
  Data2 := TJSONObject.Create([
    'id', 0,
    'name', 'Luiz',
    'items', TJSONArray.Create([
       'a', 1, nil
      ])
  ]);
  CheckEquals(0, CompareJSONData(Data1, Data2), 'Complex Object data with order prop different');

  Data1 := TJSONObject.Create([
    'name', 'Luiz',
    'id', 0,
    'items', TJSONArray.Create([
       'a', 1, nil
      ])
  ]);
  //same object, array item order different
  Data2 := TJSONObject.Create([
    'name', 'Luiz',
    'id', 0,
    'items', TJSONArray.Create([
       'a', nil, 1
      ])
  ]);
  CheckNotEquals(0, CompareJSONData(Data1, Data2), 'Complex Object data with Array order item different');

  Data1 := TJSONArray.Create([
    2,
    'b',
    TJSONObject.Create([
      'name', 'Luiz',
      'id', 0,
      'items', TJSONArray.Create([
         'a', nil, 1
        ])
    ])
  ]);
  //same object, prop order different
  Data2 := TJSONArray.Create([
    2,
    'b',
    TJSONObject.Create([
      'id', 0,
      'name', 'Luiz',
      'items', TJSONArray.Create([
         'a', nil, 1
        ])
    ])
  ]);
  CheckEquals(0, CompareJSONData(Data1, Data2), 'Complex Array data with order prop different');

  Data1 := TJSONArray.Create([
    2,
    'b',
    TJSONObject.Create([
      'name', 'Luiz',
      'id', 0,
      'items', TJSONArray.Create([
         'a', nil, 1
        ])
    ])
  ]);
  //same object, sub array with different value
  Data2 := TJSONArray.Create([
    2,
    'b',
    TJSONObject.Create([
      'id', 0,
      'name', 'Luiz',
      'items', TJSONArray.Create([
         'a', nil, 2
        ])
    ])
  ]);
  CheckNotEquals(0, CompareJSONData(Data1, Data2), 'Complex Array data sub array with different value');
end;

initialization
  ProjectRegisterTests('JSONUtils', [TCompareDataTestCase.Suite]);
end.

