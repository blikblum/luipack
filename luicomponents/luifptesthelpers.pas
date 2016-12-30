unit LuiFPTestHelpers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, fpjson, TestFramework;

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

  { TTestProcHelper }

  TTestProcHelper = class helper for TTestProc
  private
    function DoCheckEquals(ExpectedData, ActualData: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
  public
    function CheckEquals(Expected, Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs; overload;
    function CheckEquals(const Expected: String; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs; overload;
    //todo: remove ASAP (the overloaded method does not work at least in fpc 2.6.4 - test in 3.0)
    function CheckEqualsJSON(Expected, Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
    function CheckEqualsJSON(const Expected: String; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs; overload;
    function CheckEqualsObject(const Expected: array of const; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
    function CheckEqualsArray(const Expected: array of const; Actual: TJSONData; const ErrorMsg: string = ''): TJSONTestArgs;
  end;

  { TProcedureCalls }

  TProcedureCalls = class
  private
    FCalls: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ProcName: String; const Params: array of const);
    function Called(const ProcName: String): Boolean;
    function Called(const ProcName: String; Count: Integer): Boolean;
    function CalledOnce(const ProcName: String): Boolean;
    function CalledWith(const ProcName: String; const Params: array of const): Boolean;
    function Reset: TProcedureCalls;
  end;


implementation

uses
  LuiJSONHelpers, LuiJSONUtils;

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

{ TTestProcHelper }

function TTestProcHelper.DoCheckEquals(ExpectedData, ActualData: TJSONData;
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

function TTestProcHelper.CheckEquals(Expected, Actual: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(Expected, Actual, ErrorMsg);
end;

function TTestProcHelper.CheckEquals(const Expected: String; Actual: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
var
  ExpectedData: TJSONData;
begin
  if not TryStrToJSON(Expected, ExpectedData) then
    raise Exception.Create('Malformed expected JSON string');
  Result := DoCheckEquals(ExpectedData, Actual, ErrorMsg).FreeExpected;
end;

function TTestProcHelper.CheckEqualsJSON(Expected, Actual: TJSONData; const ErrorMsg: string): TJSONTestArgs;
begin
  Result := CheckEquals(Expected, Actual, ErrorMsg);
end;

function TTestProcHelper.CheckEqualsJSON(const Expected: String; Actual: TJSONData;
  const ErrorMsg: string): TJSONTestArgs;
var
  ExpectedData: TJSONData;
begin
  if not TryStrToJSON(Expected, ExpectedData) then
    raise Exception.Create('Malformed expected JSON string');
  Result := DoCheckEquals(ExpectedData, Actual, ErrorMsg).FreeExpected;
end;

function TTestProcHelper.CheckEqualsObject(const Expected: array of const;
  Actual: TJSONData; const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(TJSONObject.Create(Expected), Actual, ErrorMsg).FreeExpected;
end;

function TTestProcHelper.CheckEqualsArray(const Expected: array of const;
  Actual: TJSONData; const ErrorMsg: string): TJSONTestArgs;
begin
  Result := DoCheckEquals(TJSONArray.Create(Expected), Actual, ErrorMsg).FreeExpected;
end;

{ TProcedureCalls }

constructor TProcedureCalls.Create;
begin
  FCalls := TJSONObject.Create;
end;

destructor TProcedureCalls.Destroy;
begin
  FCalls.Destroy;
  inherited Destroy;
end;

procedure TProcedureCalls.Add(const ProcName: String; const Params: array of const);
var
  ParamsData, MethodCallsData: TJSONArray;
begin
  if not FCalls.Find(ProcName, MethodCallsData) then
  begin
    MethodCallsData := TJSONArray.Create;
    FCalls.Arrays[ProcName] := MethodCallsData;
  end;
  ParamsData := TJSONArray.Create(Params);
  MethodCallsData.Add(ParamsData);
end;

function TProcedureCalls.Called(const ProcName: String): Boolean;
var
  MethodCallsData: TJSONArray;
begin
  Result := FCalls.Find(ProcName, MethodCallsData);
  if Result then
    Result := MethodCallsData.Count >= 1;
end;

function TProcedureCalls.Called(const ProcName: String; Count: Integer): Boolean;
var
  MethodCallsData: TJSONArray;
begin
  Result := FCalls.Find(ProcName, MethodCallsData);
  if Result then
    Result := MethodCallsData.Count = Count;
end;

function TProcedureCalls.CalledOnce(const ProcName: String): Boolean;
begin
  Result := Called(ProcName, 1);
end;

function TProcedureCalls.CalledWith(const ProcName: String;
  const Params: array of const): Boolean;
var
  MethodCallsData, ParamsData: TJSONArray;
  i: Integer;
begin
  Result := FCalls.Find(ProcName, MethodCallsData);
  ParamsData := TJSONArray.Create(Params);
  try
    if Result then
      begin
        for i := 0 to MethodCallsData.Count - 1 do
        begin
          Result := CompareJSONData(ParamsData, MethodCallsData.Items[i]) = 0;
          if Result then
            break;
        end;
      end;
  finally
    ParamsData.Destroy;
  end;
end;

function TProcedureCalls.Reset: TProcedureCalls;
begin
  FCalls.Clear;
  Result := Self;
end;


end.

