unit ScannerTests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, HandlebarsScanner;

type

  { TScannerTests }

  TScannerTests = class(TTestCase)
  private
    FScanner: THandlebarsScanner;
    procedure CheckEquals(expected, actual: THandlebarsToken; const ErrorMsg: string = ''); overload;
    procedure CreateScanner(const Source: String);
  protected
    procedure TearDown; override;
  published
    procedure EmptyTemplate;
    procedure OnlyContent;
  end;


implementation

uses
  sysutils;

procedure TScannerTests.CheckEquals(expected, actual: THandlebarsToken; const ErrorMsg: string);
var
  ExpectedStr, ActualStr: String;
begin
  OnCheckCalled;
  if expected <> actual then
  begin
    WriteStr(ExpectedStr, expected);
    WriteStr(ActualStr, actual);
    FailNotEquals(ExpectedStr, ActualStr, ErrorMsg);
  end;
end;

procedure TScannerTests.CreateScanner(const Source: String);
begin
  FScanner.Free;
  FScanner := THandlebarsScanner.Create(Source);
end;

procedure TScannerTests.TearDown;
begin
  FreeAndNil(FScanner);
  inherited TearDown;
end;

procedure TScannerTests.EmptyTemplate;
begin
  CreateScanner('');
  CheckEquals(tkEOF, FScanner.FetchToken);
  CheckEquals(tkEOF, FScanner.CurToken);
  CheckEquals('', FScanner.CurTokenString);
  //again
  CheckEquals(tkEOF, FScanner.FetchToken);
  CheckEquals(tkEOF, FScanner.CurToken);
  CheckEquals('', FScanner.CurTokenString);
end;

procedure TScannerTests.OnlyContent;
begin
  CreateScanner('xx');
  CheckEquals(tkContent, FScanner.FetchToken);
  CheckEquals(tkContent, FScanner.CurToken);
  CheckEquals('xx', FScanner.CurTokenString);
  CheckEquals(tkEOF, FScanner.FetchToken);

  CreateScanner(' yy ');
  CheckEquals(tkContent, FScanner.FetchToken);
  CheckEquals(tkContent, FScanner.CurToken);
  CheckEquals(' yy ', FScanner.CurTokenString);
  CheckEquals(tkEOF, FScanner.FetchToken);
end;

initialization
  RegisterTest('Scanner', TScannerTests.Suite);

end.

