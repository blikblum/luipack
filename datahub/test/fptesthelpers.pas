unit FPTestHelpers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONUtils, TestFramework;

type


  { TJSONTestArgs }

  TJSONTestArgs = record
    FActualData: TJSONData;
    FExpectedData: TJSONData;
  public
    procedure Create(ExpectedData, ActualData: TJSONData);
    function FreeAll: TJSONTestArgs;
    function FreeActual: TJSONTestArgs;
    function FreeExpected: TJSONTestArgs;
  end;

  { TTestCaseHelper }

  TTestCaseHelper = class helper for TTestCase
  private
    function DoCheckEquals(ExpectedData, ActualData: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
  public
    function CheckEquals(Expected, Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
    function CheckEquals(const Expected: String; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
    function CheckEqualsObject(const Expected: array of const; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
    function CheckEqualsArray(const Expected: array of const; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
  end;

implementation

{ TJSONTestArgs }

procedure TJSONTestArgs.Create(ExpectedData, ActualData: TJSONData);
begin
  FActualData := ActualData;
  FExpectedData := ExpectedData;
end;

function TJSONTestArgs.FreeAll: TJSONTestArgs;
begin
  FreeAndNil(FActualData);
  FreeAndNil(FExpectedData);
  Result := Self;
end;

function TJSONTestArgs.FreeActual: TJSONTestArgs;
begin
  FreeAndNil(FActualData);
  Result := Self;
end;

function TJSONTestArgs.FreeExpected: TJSONTestArgs;
begin
  FreeAndNil(FExpectedData);
  Result := Self;
end;

{ TTestCaseHelper }

function TTestCaseHelper.DoCheckEquals(ExpectedData, ActualData: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
var
  ExpectedJSON, ActualJSON: String;
begin
  OnCheckCalled;
  Result.Create(ExpectedData, ActualData);
  if CompareJSONData(ExpectedData, ActualData) <> 0 then
  begin
    ExpectedJSON := '';
    if ExpectedData <> nil then
      ExpectedJSON := ExpectedData.AsJSON;
    if ActualData <> nil then
      ActualJSON := ActualData.AsJSON;
    FailNotEquals(ExpectedJSON, ActualJSON, ErrorMsg, CallerAddr);
  end;
end;

function TTestCaseHelper.CheckEquals(Expected, Actual: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(Expected, Actual, ErrorMsg);
end;

function TTestCaseHelper.CheckEquals(const Expected: String; Actual: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
var
  ExpectedData: TJSONData;
begin
  if not TryStrToJSON(Expected, ExpectedData) then
    raise Exception.Create('Malformed expected JSON string');
  Result := DoCheckEquals(ExpectedData, Actual, ErrorMsg).FreeExpected;
end;

function TTestCaseHelper.CheckEqualsObject(const Expected: array of const;
  Actual: TJSONData; const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(TJSONObject.Create(Expected), Actual, ErrorMsg).FreeExpected;
end;

function TTestCaseHelper.CheckEqualsArray(const Expected: array of const;
  Actual: TJSONData; const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(TJSONArray.Create(Expected), Actual, ErrorMsg).FreeExpected;
end;


end.

