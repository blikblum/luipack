unit FPTestHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONUtils, TestFramework;

type

  { TTestCaseHelper }

  TTestCaseHelper = class helper for TTestCase
  private
    procedure DoCheckEquals(Expected, Actual: TJSONData; const ErrorMsg: string = '';
      FreeExpected: Boolean = False; FreeActual: Boolean = False);
  public
    procedure CheckEquals(const Expected: String; Actual: TJSONData; const ErrorMsg: string = '');
    procedure CheckEqualsFreeActual(const Expected: String; Actual: TJSONData; const ErrorMsg: string = '');
  end;

implementation

{ TTestCaseHelper }

procedure TTestCaseHelper.DoCheckEquals(Expected, Actual: TJSONData;
  const ErrorMsg: string; FreeExpected: Boolean; FreeActual: Boolean);
var
  ExpectedJSON, ActualJSON: String;
begin
  OnCheckCalled;
  if CompareJSONData(Expected, Actual) <> 0 then
  begin
    ExpectedJSON := '';
    if Expected <> nil then
      ExpectedJSON := Expected.AsJSON;
    if Actual <> nil then
      ActualJSON := Actual.AsJSON;
    FailNotEquals(ExpectedJSON, ActualJSON, ErrorMsg, CallerAddr);
  end;
  if FreeExpected then
    Expected.Free;
  if FreeActual then
    Actual.Free;
end;

procedure TTestCaseHelper.CheckEquals(const Expected: String;
  Actual: TJSONData; const ErrorMsg: string);
var
  ExpectedData: TJSONData;
begin
  if not TryStrToJSON(Expected, ExpectedData) then
    raise Exception.Create('Malformed expected JSON string');
  DoCheckEquals(ExpectedData, Actual, ErrorMsg, True, False);
end;

procedure TTestCaseHelper.CheckEqualsFreeActual(const Expected: String;
  Actual: TJSONData; const ErrorMsg: string);
var
  ExpectedData: TJSONData;
begin
  if not TryStrToJSON(Expected, ExpectedData) then
    raise Exception.Create('Malformed expected JSON string');
  DoCheckEquals(ExpectedData, Actual, ErrorMsg, True, True);
end;


end.

