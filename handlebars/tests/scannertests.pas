unit ScannerTests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, HandlebarsScanner;

type

  TTokenInfo = record
    Content: String;
    Value: THandlebarsToken;
  end;

  TTokenArray = array of THandlebarsToken;

  { TTokenList }

  TTokenList = class
  private
    FValues: TTokenArray;
    FContents: array of String;
    function GetItems(Index: Integer): TTokenInfo;
  public
    procedure Add(Value: THandlebarsToken; const Content: String);
    property Values: TTokenArray read FValues;
    property Items[Index: Integer]: TTokenInfo read GetItems; default;
  end;

  { TScannerTests }

  TScannerTests = class(TTestCase)
  private
    FTokens: TTokenList;
    FScanner: THandlebarsScanner;
    procedure CheckEquals(expected, actual: THandlebarsToken; const ErrorMsg: string = ''); overload;
    procedure CheckEquals(expected, actual: array of THandlebarsToken; const ErrorMsg: string = ''); overload;
    procedure CheckEquals(expectedValue: THandlebarsToken; const ExpectedContent: String;
      const actual: TTokenInfo; const ErrorMsg: string = ''); overload; overload;
    procedure CreateScanner(const Source: String);
    procedure CreateTokens(const Source: String);
  protected
    procedure TearDown; override;
  published
    procedure EmptyTemplate;
    procedure OnlyContent;
  end;


implementation

uses
  sysutils;

function SameTokens(const Array1, Array2: array of THandlebarsToken): Boolean;
var
  ItemCount, i: Integer;
begin
  ItemCount := Length(Array1);
  Result := ItemCount = Length(Array2);
  if Result then
  begin
    for i := 0 to ItemCount - 1 do
    begin
      if Array1[i] <> Array2[i] then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TokenArrayToStr(const Tokens: array of THandlebarsToken): String;
var
  i: Integer;
  TokenStr: String;
begin
  Result := '[';
  for i := 0 to Length(Tokens) - 1 do
  begin
    WriteStr(TokenStr, Tokens[i]);
    Result := Result + TokenStr;
    if i < Length(Tokens) - 1 then
      Result := Result + ',';
  end;
  Result := Result + ']';
end;

{ TTokenList }

function TTokenList.GetItems(Index: Integer): TTokenInfo;
var
  Count: Integer;
begin
  Count := Length(FValues);
  if Index > Count - 1 then
    raise Exception.CreateFmt('There''s no token at index %d', [Index]);
  Result.Value := FValues[Index];
  Result.Content := FContents[Index];
end;

procedure TTokenList.Add(Value: THandlebarsToken; const Content: String);
var
  Count: Integer;
begin
  Count := Length(FValues);
  SetLength(FValues, Count + 1);
  SetLength(FContents, Count + 1);
  FValues[Count] := Value;
  FContents[Count] := Content;
end;

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

procedure TScannerTests.CheckEquals(expected, actual: array of THandlebarsToken;
  const ErrorMsg: string);
var
  ExpectedStr, ActualStr: String;
begin
  OnCheckCalled;
  if not SameTokens(expected, actual) then
  begin
    ActualStr := TokenArrayToStr(actual);
    ExpectedStr := TokenArrayToStr(expected);
    FailNotEquals(ExpectedStr, ActualStr, ErrorMsg);
  end;
end;

procedure TScannerTests.CheckEquals(expectedValue: THandlebarsToken; const ExpectedContent: String;
  const actual: TTokenInfo; const ErrorMsg: string);
var
  ActualStr, ExpectedStr: String;
begin
  OnCheckCalled;
  if expectedValue <> actual.Value then
  begin
    WriteStr(ExpectedStr, expectedValue);
    WriteStr(ActualStr, actual.Value);
    FailNotEquals(ExpectedStr, ActualStr, ErrorMsg);
  end else if ExpectedContent <> actual.Content then
  begin
    FailNotEquals(ExpectedContent, actual.Content, ErrorMsg);
  end;
end;

procedure TScannerTests.CreateScanner(const Source: String);
begin
  FScanner.Free;
  FScanner := THandlebarsScanner.Create(Source);
end;

procedure TScannerTests.CreateTokens(const Source: String);
var
  Scanner: THandlebarsScanner;
  Value: THandlebarsToken;
  Content: String;
begin
  FTokens.Free;
  FTokens := TTokenList.Create;
  Scanner := THandlebarsScanner.Create(Source);
  try
    Value := Scanner.FetchToken;
    while Value <> tkEOF do
    begin
      Content := Scanner.CurTokenString;
      FTokens.Add(Value, Content);
      Value := Scanner.FetchToken;
    end;
  finally
    Scanner.Destroy;
  end;
end;

procedure TScannerTests.TearDown;
begin
  FreeAndNil(FTokens);
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
  CreateTokens('xx');
  CheckEquals([tkContent], FTokens.Values);
  CheckEquals(tkContent, 'xx', FTokens[0]);

  CreateTokens(' yy ');
  CheckEquals([tkContent], FTokens.Values);
  CheckEquals(tkContent, ' yy ', FTokens[0]);
end;

initialization
  RegisterTest('Scanner', TScannerTests.Suite);

end.

