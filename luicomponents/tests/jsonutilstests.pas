unit JSONUtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, fpjson;

type

  { TCompareDataTestCase }

  TCompareDataTestCase = class(TTestCase)
  private

  protected
    procedure TearDown; override;
  published
    procedure CompareNull;
    procedure CompareInteger;
  end;

implementation

uses
  LuiJSONUtils, Math;

procedure TCompareDataTestCase.TearDown;
begin
  inherited TearDown;
end;

procedure TCompareDataTestCase.CompareNull;
begin
  CheckEquals(0, CompareJSONData(TJSONNull.Create, TJSONNull.Create), 'Null shoud be equal to null');
end;

procedure TCompareDataTestCase.CompareInteger;
begin
  CheckEquals(0, CompareJSONData(TJSONIntegerNumber.Create(1), TJSONIntegerNumber.Create(1)), '1 shoud be equal to 1');
  CheckEquals(-1, CompareJSONData(TJSONIntegerNumber.Create(1), TJSONIntegerNumber.Create(2)), '1 shoud be less then 2');
  CheckEquals(1, CompareJSONData(TJSONIntegerNumber.Create(2), TJSONIntegerNumber.Create(1)), '2 shoud be less then 1');
end;

initialization
  ProjectRegisterTests('JSONUtils', [TCompareDataTestCase.Suite]);
end.

