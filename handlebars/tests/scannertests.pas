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
    //adapted from handlebars.js/spec/tokenizer.js
    procedure SimpleMustache;
    procedure UnescapeWithAmpersand;
    procedure UnescapeWithTripleMustache;
    procedure EscapeDelimiters;
    procedure EscapeMultipleDelimiters;
    procedure EscapeTripleStash;
    procedure EscapeEscapeCharacter;
    procedure EscapeMultipleEscapeCharacters;
    procedure EscapeAfterEscapedEscapeCharacther;
    procedure EscapeAfterEscapedMustache;
    procedure EscapeEscapeCharacterOnTripleStash;
    procedure SimplePath;
    procedure SimplePathWithDots;
    procedure PathLiteralsWithBrackets;
    procedure MultiplePathLiteralsWithBrackets;
    procedure ScapedLiteralsInBrackets;
    procedure SingleDotPath;
    procedure ParentPath;
    procedure PathWithThis;
    procedure SimpleMustacheWithSpaces;
    procedure SimpleMustacheWithLineBreaks;
    procedure RawContent;
    procedure SimplePartial;
    procedure PartialWithContext;
    procedure PartialWithPath;
    procedure PartialBlock;
    procedure Comments;
    procedure SimpleBlock;
    procedure Directives;
    procedure Inverse;
    procedure InverseWithId;
    procedure Params;
    procedure StringParam;
    procedure StringParamWithSpace;
    procedure StringParamWithQuote;
    procedure NumberParam;
    procedure BooleanParam;
    procedure UndefinedAndNullParam;
    procedure HashArguments;
    procedure AtIdentifier;
    procedure InvalidMustache;
    procedure SubExpression;
    procedure NestedSubExpression;
    procedure NestedSubExpressionLiteral;
    procedure BlockParam;
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

procedure TScannerTests.SimpleMustache;
begin
  CreateTokens('{{foo}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkOpen, '{{', FTokens[0]);
  CheckEquals(tkID, 'foo', FTokens[1]);
  CheckEquals(tkClose, '}}', FTokens[2]);
end;

procedure TScannerTests.UnescapeWithAmpersand;
begin
  CreateTokens('{{&bar}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkOPEN, '{{&', FTokens[0]);
  CheckEquals(tkID, 'bar', FTokens[1]);
end;

procedure TScannerTests.UnescapeWithTripleMustache;
begin
  CreateTokens('{{{bar}}}');
  CheckEquals([tkOPENUNESCAPED, tkID, tkCLOSEUNESCAPED], FTokens.Values);

  CheckEquals(tkID, 'bar', FTokens[1]);
end;

procedure TScannerTests.EscapeDelimiters;
begin
  CreateTokens('{{foo}} \\{{bar}} {{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, ' ', FTokens[3]);
  CheckEquals(tkCONTENT, '{{bar}} ', FTokens[4]);
end;

procedure TScannerTests.EscapeMultipleDelimiters;
begin
  CreateTokens('{{foo}} \\{{bar}} \\{{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkCONTENT, tkCONTENT], FTokens.Values);

  CheckEquals(tkCONTENT, ' ', FTokens[3]);
  CheckEquals(tkCONTENT, '{{bar}} ', FTokens[4]);
  CheckEquals(tkCONTENT, '{{baz}}', FTokens[5]);
end;

procedure TScannerTests.EscapeTripleStash;
begin
  CreateTokens('{{foo}} \\{{{bar}}} {{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, '{{{bar}}} ', FTokens[4]);
end;

procedure TScannerTests.EscapeEscapeCharacter;
begin
  CreateTokens('{{foo}} \\\\{{bar}} {{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, ' \\', FTokens[3]);
  CheckEquals(tkID, 'bar', FTokens[5]);;
end;

procedure TScannerTests.EscapeMultipleEscapeCharacters;
begin
  CreateTokens('{{foo}} \\\\{{bar}} \\\\{{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, ' \\', FTokens[3]);
  CheckEquals(tkID, 'bar', FTokens[5]);
  CheckEquals(tkCONTENT, ' \\', FTokens[7]);
  CheckEquals(tkID, 'baz', FTokens[9]);
end;

procedure TScannerTests.EscapeAfterEscapedEscapeCharacther;
begin
  CreateTokens('{{foo}} \\\\{{bar}} \\{{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPEN, tkID, tkCLOSE, tkCONTENT, tkCONTENT, tkCONTENT], FTokens.Values);

  CheckEquals(tkCONTENT, ' \\', FTokens[3]);
  CheckEquals(tkOPEN, '{{', FTokens[4]);
  CheckEquals(tkID, 'bar', FTokens[5]);
  CheckEquals(tkCONTENT, ' ', FTokens[7]);
  CheckEquals(tkCONTENT, '{{baz}}', FTokens[8]);
end;

procedure TScannerTests.EscapeAfterEscapedMustache;
begin
  CreateTokens('{{foo}} \\{{bar}} \\\\{{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkCONTENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, '{{bar}} ', FTokens[4]);
  CheckEquals(tkCONTENT, '\\', FTokens[5]);
  CheckEquals(tkOPEN, '{{', FTokens[6]);
  CheckEquals(tkID, 'baz', FTokens[7]);
end;

procedure TScannerTests.EscapeEscapeCharacterOnTripleStash;
begin
  CreateTokens('{{foo}} \\\\{{{bar}}} {{baz}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE, tkCONTENT, tkOPENUNESCAPED, tkID, tkCLOSEUNESCAPED, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkCONTENT, ' \\', FTokens[3]);
  CheckEquals(tkID, 'bar', FTokens[5]);
end;

procedure TScannerTests.SimplePath;
begin
  CreateTokens('{{foo/bar}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkId, 'foo', FTokens[1]);
  CheckEquals(tkSep, '/', FTokens[2]);
  CheckEquals(tkId, 'bar', FTokens[3]);
end;

procedure TScannerTests.SimplePathWithDots;
begin
  CreateTokens('{{foo.bar}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{foo.bar.baz}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.PathLiteralsWithBrackets;
begin
  CreateTokens('{{foo.[bar]}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);

  CheckEquals(tkId, 'foo', FTokens[1]);
  CheckEquals(tkSep, '.', FTokens[2]);
  CheckEquals(tkId, '[bar]', FTokens[3]);
end;

procedure TScannerTests.MultiplePathLiteralsWithBrackets;
begin
  CreateTokens('{{foo.[bar]}}{{foo.[baz]}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE, tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.ScapedLiteralsInBrackets;
begin
  CreateTokens('{{foo.[bar\\]]}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkId, '[bar]]', FTokens[3]);
end;

procedure TScannerTests.SingleDotPath;
begin
  CreateTokens('{{.}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkId, '.', FTokens[1]);

  CreateTokens('{{ . }}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkId, '.', FTokens[1]);
end;

procedure TScannerTests.ParentPath;
begin
  CreateTokens('{{../foo/bar}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, '..', FTokens[1]);

  CreateTokens('{{../foo.bar}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, '..', FTokens[1]);
end;

procedure TScannerTests.PathWithThis;
begin
  CreateTokens('{{this/foo}}');
  CheckEquals([tkOPEN, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'this', FTokens[1]);
  CheckEquals(tkID, 'foo', FTokens[3]);
end;

procedure TScannerTests.SimpleMustacheWithSpaces;
begin
  CreateTokens('{{  foo  }}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkOpen, '{{', FTokens[0]);
  CheckEquals(tkID, 'foo', FTokens[1]);
  CheckEquals(tkClose, '}}', FTokens[2]);
end;

procedure TScannerTests.SimpleMustacheWithLineBreaks;
begin
  CreateTokens('{{  foo  ' + LineEnding + '   bar }}');
  CheckEquals([tkOPEN, tkID, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);
end;

procedure TScannerTests.RawContent;
begin
  CreateTokens('foo {{ bar }} baz');
  CheckEquals([tkCONTENT, tkOPEN, tkID, tkCLOSE, tkCONTENT], FTokens.Values);
  CheckEquals(tkCONTENT, 'foo ', FTokens[0]);
  CheckEquals(tkCONTENT, ' baz', FTokens[4]);
end;

procedure TScannerTests.SimplePartial;
begin
  CreateTokens('{{> foo}}');
  CheckEquals([tkOPENPARTIAL, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{>foo}}');
  CheckEquals([tkOPENPARTIAL, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{>foo  }}');
  CheckEquals([tkOPENPARTIAL, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.PartialWithContext;
begin
  CreateTokens('{{> foo bar }}');
  CheckEquals([tkOPENPARTIAL, tkID, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.PartialWithPath;
begin
  CreateTokens('{{>foo/bar.baz  }}');
  CheckEquals([tkOPENPARTIAL, tkID, tkSEP, tkID, tkSEP, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.PartialBlock;
begin
  CreateTokens('{{#> foo}}');
  CheckEquals([tkOPENPARTIALBLOCK, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.Comments;
begin
  CreateTokens('foo {{! this is a comment }} bar {{ baz }}');
  CheckEquals([tkCONTENT, tkCOMMENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkCOMMENT, '{{! this is a comment }}', FTokens[1]);

  CreateTokens('foo {{!-- this is a {{comment}} --}} bar {{ baz }}');
  CheckEquals([tkCONTENT, tkCOMMENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkCOMMENT, '{{!-- this is a {{comment}} --}}', FTokens[1]);

  CreateTokens('foo {{!-- this is a'+LineEnding+'{{comment}}'+LineEnding+'--}} bar {{ baz }}');
  CheckEquals([tkCONTENT, tkCOMMENT, tkCONTENT, tkOPEN, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkCOMMENT, '{{!-- this is a'+LineEnding+'{{comment}}'+LineEnding+'--}}', FTokens[1]);
end;

procedure TScannerTests.SimpleBlock;
begin
  CreateTokens('{{#foo}}content{{/foo}}');
  CheckEquals([tkOPENBLOCK, tkID, tkCLOSE, tkCONTENT, tkOPENENDBLOCK, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.Directives;
begin
  CreateTokens('{{#*foo}}content{{/foo}}');
  CheckEquals([tkOPENBLOCK, tkID, tkCLOSE, tkCONTENT, tkOPENENDBLOCK, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{*foo}}');
  CheckEquals([tkOPEN, tkID, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.Inverse;
begin
  CreateTokens('{{^}}');
  CheckEquals([tkINVERSE], FTokens.Values);

  CreateTokens('{{else}}');
  CheckEquals([tkINVERSE], FTokens.Values);

  CreateTokens('{{ else }}');
  CheckEquals([tkINVERSE], FTokens.Values);
end;

procedure TScannerTests.InverseWithId;
begin
  CreateTokens('{{^foo}}');
  CheckEquals([tkOPENINVERSE, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);

  CreateTokens('{{^ foo  }}');
  CheckEquals([tkOPENINVERSE, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);
end;

procedure TScannerTests.Params;
begin
  CreateTokens('{{ foo bar baz }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);
  CheckEquals(tkID, 'bar', FTokens[2]);
  CheckEquals(tkID, 'baz', FTokens[3]);
end;

procedure TScannerTests.StringParam;
begin
  CreateTokens('{{ foo bar \"baz\" }}');
  CheckEquals([tkOPEN, tkID, tkID, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkSTRING, 'baz', FTokens[3]);

  CreateTokens('{{ foo bar \''baz\'' }}');
  CheckEquals([tkOPEN, tkID, tkID, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkSTRING, 'baz', FTokens[3]);
end;

procedure TScannerTests.StringParamWithSpace;
begin
  CreateTokens('{{ foo bar "baz bat" }}');
  CheckEquals([tkOPEN, tkID, tkID, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkSTRING, 'baz bat', FTokens[3]);
end;

procedure TScannerTests.StringParamWithQuote;
begin
  CreateTokens('{{ foo "bar\\"baz" }}');
  CheckEquals([tkOPEN, tkID, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkSTRING, 'bar"baz', FTokens[2]);

  CreateTokens('{{ foo ''bar\\''baz'' }}');
  CheckEquals([tkOPEN, tkID, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkSTRING, 'bar''baz', FTokens[2]);
end;

procedure TScannerTests.NumberParam;
begin
  CreateTokens('{{ foo 1 }}');
  CheckEquals([tkOPEN, tkID, tkNUMBER, tkCLOSE], FTokens.Values);
  CheckEquals(tkNUMBER, '1', FTokens[2]);

  CreateTokens('{{ foo 1.1 }}');
  CheckEquals([tkOPEN, tkID, tkNUMBER, tkCLOSE], FTokens.Values);
  CheckEquals(tkNUMBER, '1.1', FTokens[2]);

  CreateTokens('{{ foo -1 }}');
  CheckEquals([tkOPEN, tkID, tkNUMBER, tkCLOSE], FTokens.Values);
  CheckEquals(tkNUMBER, '-1', FTokens[2]);

  CreateTokens('{{ foo -1.1 }}');
  CheckEquals([tkOPEN, tkID, tkNUMBER, tkCLOSE], FTokens.Values);
  CheckEquals(tkNUMBER, '-1.1', FTokens[2]);
end;

procedure TScannerTests.BooleanParam;
begin
  CreateTokens('{{ foo true }}');
  CheckEquals([tkOPEN, tkID, tkBOOLEAN, tkCLOSE], FTokens.Values);
  CheckEquals(tkBOOLEAN, 'true', FTokens[2]);

  CreateTokens('{{ foo false }}');
  CheckEquals([tkOPEN, tkID, tkBOOLEAN, tkCLOSE], FTokens.Values);
  CheckEquals(tkBOOLEAN, 'false', FTokens[2]);
end;

procedure TScannerTests.UndefinedAndNullParam;
begin
  CreateTokens('{{ foo undefined null }}');
  CheckEquals([tkOPEN, tkID, tkUNDEFINED, tkNULL, tkCLOSE], FTokens.Values);
  CheckEquals(tkUNDEFINED, 'undefined', FTokens[2]);
  CheckEquals(tkNULL, 'null', FTokens[3]);
end;

procedure TScannerTests.HashArguments;
begin
  CreateTokens('{{ foo bar=baz }}');
  CheckEquals([tkOPEN, tkID, tkID, tkEQUALS, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=bat }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=1 }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkNUMBER, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=true }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkBOOLEAN, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=false }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkBOOLEAN, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar'+LineEnding+'  baz=bat }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=\"bat\" }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkSTRING, tkCLOSE], FTokens.Values);

  CreateTokens('{{ foo bar baz=\"bat\" bam=wot }}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkSTRING, tkID, tkEQUALS, tkID, tkCLOSE], FTokens.Values);

  CreateTokens('{{foo omg bar=baz bat=\"bam\"}}');
  CheckEquals([tkOPEN, tkID, tkID, tkID, tkEQUALS, tkID, tkID, tkEQUALS, tkSTRING, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'omg', FTokens[2]);
end;

procedure TScannerTests.AtIdentifier;
begin
  CreateTokens('{{ @foo }}');
  CheckEquals([tkOPEN, tkDATA, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[2]);

  CreateTokens('{{ foo @bar }}');
  CheckEquals([tkOPEN, tkID, tkDATA, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'bar', FTokens[3]);

  CreateTokens('{{ foo bar=@baz }}');
  CheckEquals([tkOPEN, tkID, tkID, tkEQUALS, tkDATA, tkID, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'baz', FTokens[5]);
end;

procedure TScannerTests.InvalidMustache;
begin
  CreateTokens('{{foo}');
  CheckEquals([tkOPEN, tkID], FTokens.Values);

  CreateTokens('{{foo & }}');
  CheckEquals([tkOPEN, tkID], FTokens.Values);
end;

procedure TScannerTests.SubExpression;
begin
  CreateTokens('{{foo (bar)}}');
  CheckEquals([tkOPEN, tkID, tkOPENSEXPR, tkID, tkCLOSESEXPR, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);
  CheckEquals(tkID, 'bar', FTokens[3]);

  CreateTokens('{{foo (a-x b-y)}}');
  CheckEquals([tkOPEN, tkID, tkOPENSEXPR, tkID, tkID, tkCLOSESEXPR, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'foo', FTokens[1]);
  CheckEquals(tkID, 'a-x', FTokens[3]);
  CheckEquals(tkID, 'b-y', FTokens[4]);
end;

procedure TScannerTests.NestedSubExpression;
begin
  CreateTokens('{{foo (bar (lol rofl)) (baz)}}');
  CheckEquals([tkOPEN, tkID, tkOPENSEXPR, tkID, tkOPENSEXPR, tkID, tkID, tkCLOSESEXPR, tkCLOSESEXPR, tkOPENSEXPR, tkID, tkCLOSESEXPR, tkCLOSE], FTokens.Values);
  CheckEquals(tkID, 'bar', FTokens[3]);
  CheckEquals(tkID, 'lol', FTokens[5]);
  CheckEquals(tkID, 'rofl', FTokens[6]);
  CheckEquals(tkID, 'baz', FTokens[10]);
end;

procedure TScannerTests.NestedSubExpressionLiteral;
begin
  CreateTokens('{{foo (bar (lol true) false) (baz 1) (blah ''b'') (blorg \"c\")}}');
  CheckEquals([tkOPEN, tkID, tkOPENSEXPR, tkID, tkOPENSEXPR, tkID, tkBOOLEAN, tkCLOSESEXPR, tkBOOLEAN, tkCLOSESEXPR, tkOPENSEXPR, tkID, tkNUMBER, tkCLOSESEXPR, tkOPENSEXPR, tkID, tkSTRING, tkCLOSESEXPR, tkOPENSEXPR, tkID, tkSTRING, tkCLOSESEXPR, tkCLOSE], FTokens.Values);
end;

procedure TScannerTests.BlockParam;
begin
  CreateTokens('{{#foo as |bar|}}');
  CheckEquals([tkOPENBLOCK, tkID, tkOPENBLOCKPARAMS, tkID, tkCLOSEBLOCKPARAMS, tkCLOSE], FTokens.Values);

  CreateTokens('{{#foo as |bar baz|}}');
  CheckEquals([tkOPENBLOCK, tkID, tkOPENBLOCKPARAMS, tkID, tkID, tkCLOSEBLOCKPARAMS, tkCLOSE], FTokens.Values);

  CreateTokens('{{#foo as | bar baz |}}');
  CheckEquals([tkOPENBLOCK, tkID, tkOPENBLOCKPARAMS, tkID, tkID, tkCLOSEBLOCKPARAMS, tkCLOSE], FTokens.Values);

  CreateTokens('{{#foo as as | bar baz |}}');
  CheckEquals([tkOPENBLOCK, tkID, tkID, tkOPENBLOCKPARAMS, tkID, tkID, tkCLOSEBLOCKPARAMS, tkCLOSE], FTokens.Values);

  CreateTokens('{{else foo as |bar baz|}}');
  CheckEquals([tkOPENINVERSECHAIN, tkID, tkOPENBLOCKPARAMS, tkID, tkID, tkCLOSEBLOCKPARAMS, tkCLOSE], FTokens.Values);
end;

initialization
  RegisterTest('Scanner', TScannerTests.Suite);

end.

