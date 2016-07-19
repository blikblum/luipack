unit ParserTests;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, TestFramework, HandlebarsParser;

type

  { TParserTests }

  TParserTests = class(TTestCase)
  published
    procedure SimpleMustaches;
    procedure SimpleMustachesWithData;
    procedure SimpleMustachesWithDataPath;
    procedure MustachesWithPath;
    procedure MustachesWithThis;
    procedure MustachesWithHyphen;
    procedure MustachesWithEscapedBrackets;
    procedure MustachesWithEscapedEscapeChar;
    procedure MustachesWithParams;
    procedure MustachesWithStringParams;
    procedure MustachesWithNumberParams;
    procedure MustachesWithBooleanParams;
    procedure MustachesWithUndefinedOrNull;
    procedure MustachesWithDataParams;
    procedure MustachesWithHashArguments;
    procedure ContentsFollowedByMustache;
    procedure Partial;
    procedure PartialWithContext;
    procedure PartialWithHash;
    procedure PartialWithContextAndHash;
    procedure PartialWithComplexName;
    procedure PartialBlock;
    procedure BlockMismatch;
    procedure PartialBlockWithArguments;
    procedure Comment;
    procedure MultiLineComment;
    procedure InvertSection;
    procedure MultipleInvertSections;
    procedure EmptyBlock;
    procedure EmptyBlockWithEmptyInverse;
    procedure NonEmptyBlockWithEmptyInverse;
    procedure EmptyBlockWithNonEmptyInverse;
    procedure StandaloneInverse;
    procedure ThrowsOnOldInvert;
    procedure BlockWithParams;
    procedure InverseBlockWithParams;
    procedure ChainedInverseBlockWithParams;
    procedure ParseErrors;
    procedure InvalidPaths;
    procedure ReportLineNumbers;
    procedure Directives;
  end;

implementation

uses
  sysutils, strutils;

type

  { ISubExpression }

  ISubExpression = interface
    ['{0D038222-BD76-4D77-94F0-681B8E259821}']
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TMustacheSubExpressionAdapter }

  TMustacheSubExpressionAdapter = class(TInterfacedObject, ISubExpression)
  private
    FMustache: THandlebarsMustacheStatement;
  public
    constructor Create(Mustache: THandlebarsMustacheStatement);
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TDecoratorSubExpressionAdapter }

  TDecoratorSubExpressionAdapter = class(TInterfacedObject, ISubExpression)
  private
    FDecorator: THandlebarsDecorator;
  public
    constructor Create(Decorator: THandlebarsDecorator);
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TBlockSubExpressionAdapter }

  TBlockSubExpressionAdapter = class(TInterfacedObject, ISubExpression)
  private
    FBlock: THandlebarsBlockStatement;
  public
    constructor Create(Block: THandlebarsBlockStatement);
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TDecoratorBlockSubExpressionAdapter }

  TDecoratorBlockSubExpressionAdapter = class(TInterfacedObject, ISubExpression)
  private
    FBlock: THandlebarsDecoratorBlock;
  public
    constructor Create(Block: THandlebarsDecoratorBlock);
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TSubExpressionAdapter }

  TSubExpressionAdapter = class(TInterfacedObject, ISubExpression)
  private
    FSubExpression: THandlebarsSubExpression;
  public
    constructor Create(SubExpression: THandlebarsSubExpression);
    function GetHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
    function GetPath: THandlebarsExpression;
    property Hash: THandlebarsHash read GetHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read GetPath;
  end;

  { TASTPrinter }

  TASTPrinter = record
    FPadding: Integer;
    function BooleanToStr(Bool: THandlebarsBooleanLiteral): String;
    function BlockToStr(Block: THandlebarsBlockStatement): String;
    function CommentToStr(Comment: THandlebarsCommentStatement): String;
    function ContentToStr(Content: THandlebarsContentStatement): String;
    function DecoratorToStr(Decorator: THandlebarsDecorator): String;
    function DecoratorBlockToStr(Block: THandlebarsDecoratorBlock): String;
    function HashPairToStr(Pair: THandlebarsHashPair): String;
    function HashToStr(Hash: THandlebarsHash): String;
    function Pad(const Str: String): String;
    function PathExpressionToStr(Path: THandlebarsPathExpression): String;
    function PartialBlockToStr(Partial: THandlebarsPartialBlockStatement): String;
    function PartialToStr(Partial: THandlebarsPartialStatement): String;
    function ProgramToStr(AProgram: THandlebarsProgram): String;
    function MustacheToStr(Mustache: THandlebarsMustacheStatement): String;
    function NodeToStr(Node: THandlebarsNode): String;
    function NumberToStr(Number: THandlebarsNumberLiteral): String;
    function StringToStr(Str: THandlebarsStringLiteral): String;
    function SubExpressionToStr(SubExpression: ISubExpression): String;
  end;


  //not supported by fpc 2.6.4
  {

  TStringArrayHelper = record helper for TStringArray
    function Join(const Separator: String): String;
    procedure Push(const S: String);
  end;
  }


function Join(StrArray: TStringArray; const Separator: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(StrArray) - 1 do
  begin
    Result := Result + StrArray[i];
    if i < Length(StrArray) -1 then
      Result := Result + Separator;
  end;
end;

procedure Push(var StrArray: TStringArray; const S: String);
var
  Len: Integer;
begin
  Len := Length(StrArray);
  SetLength(StrArray, Len + 1);
  StrArray[Len] := S;
end;

function ASTFor(const Template: String): String;
var
  Parser: THandlebarsParser;
  AProgram: THandlebarsProgram;
  Printer: TASTPrinter;
begin
  Result := '';
  Printer.FPadding := 0;
  Parser := THandlebarsParser.Create(Template);
  try
    AProgram := Parser.Parse;
    Result := Printer.ProgramToStr(AProgram);
  finally
    Parser.Destroy;
  end;
end;

{ TDecoratorBlockSubExpressionAdapter }

constructor TDecoratorBlockSubExpressionAdapter.Create(Block: THandlebarsDecoratorBlock);
begin
  FBlock := Block;
end;

function TDecoratorBlockSubExpressionAdapter.GetHash: THandlebarsHash;
begin
  Result := FBlock.Hash;
end;

function TDecoratorBlockSubExpressionAdapter.GetParamCount: Integer;
begin
  Result := FBlock.ParamCount;
end;

function TDecoratorBlockSubExpressionAdapter.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := FBlock.Params[Index];
end;

function TDecoratorBlockSubExpressionAdapter.GetPath: THandlebarsExpression;
begin
  Result := FBlock.Path;
end;

{ TBlockSubExpressionAdapter }

constructor TBlockSubExpressionAdapter.Create(Block: THandlebarsBlockStatement);
begin
  FBlock := Block;
end;

function TBlockSubExpressionAdapter.GetHash: THandlebarsHash;
begin
  Result := FBlock.Hash;
end;

function TBlockSubExpressionAdapter.GetParamCount: Integer;
begin
  Result := FBlock.ParamCount;
end;

function TBlockSubExpressionAdapter.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := FBlock.Params[Index];
end;

function TBlockSubExpressionAdapter.GetPath: THandlebarsExpression;
begin
  Result := FBlock.Path;
end;

{ TDecoratorSubExpressionAdapter }

constructor TDecoratorSubExpressionAdapter.Create(Decorator: THandlebarsDecorator);
begin
  FDecorator := Decorator;
end;

function TDecoratorSubExpressionAdapter.GetHash: THandlebarsHash;
begin
  Result := FDecorator.Hash;
end;

function TDecoratorSubExpressionAdapter.GetParamCount: Integer;
begin
  Result := FDecorator.ParamCount;
end;

function TDecoratorSubExpressionAdapter.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := FDecorator.Params[Index];
end;

function TDecoratorSubExpressionAdapter.GetPath: THandlebarsExpression;
begin
  Result := FDecorator.Path;
end;

{ TSubExpressionAdapter }

constructor TSubExpressionAdapter.Create(SubExpression: THandlebarsSubExpression);
begin
  FSubExpression := SubExpression;
end;

function TSubExpressionAdapter.GetHash: THandlebarsHash;
begin
  Result := FSubExpression.Hash;
end;

function TSubExpressionAdapter.GetParamCount: Integer;
begin
  Result := FSubExpression.ParamCount;
end;

function TSubExpressionAdapter.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := FSubExpression.Params[Index];
end;

function TSubExpressionAdapter.GetPath: THandlebarsExpression;
begin
  Result := FSubExpression.Path;
end;

{ TMustacheSubExpressionAdapter }

constructor TMustacheSubExpressionAdapter.Create(Mustache: THandlebarsMustacheStatement);
begin
  FMustache := Mustache;
end;

function TMustacheSubExpressionAdapter.GetHash: THandlebarsHash;
begin
  Result := FMustache.Hash;
end;

function TMustacheSubExpressionAdapter.GetParamCount: Integer;
begin
  Result := FMustache.ParamCount;
end;

function TMustacheSubExpressionAdapter.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := FMustache.Params[Index];
end;

function TMustacheSubExpressionAdapter.GetPath: THandlebarsExpression;
begin
  Result := FMustache.Path;
end;

{ TStringArrayHelper }
{
function TStringArrayHelper.Join(const Separator: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(Self) - 1 do
  begin
    Result := Result + Self[i];
    if i < Length(Self) -1 then
      Result := Result + Separator;
  end;
end;

procedure TStringArrayHelper.Push(const S: String);
var
  Len: Integer;
begin
  Len := Length(Self);
  SetLength(Self, Len + 1);
  Self[Len] := S;
end;

}

{ TASTPrinter }

function TASTPrinter.BooleanToStr(Bool: THandlebarsBooleanLiteral): String;
begin
  Result := 'BOOLEAN{' + LowerCase(BoolToStr(Bool.Value, True)) + '}';
end;

function TASTPrinter.BlockToStr(Block: THandlebarsBlockStatement): String;
begin
  Result := Pad('BLOCK:');
  Inc(FPadding);
  Result += Pad(SubExpressionToStr(TBlockSubExpressionAdapter.Create(Block)));
  if (Block.TheProgram <> nil) then
  begin
    Result += Pad('PROGRAM:');
    Inc(FPadding);
    Result += ProgramToStr(Block.TheProgram);
    Dec(FPadding);
  end;
  if (block.Inverse <> nil) then
  begin
    if (block.TheProgram <> nil) then
      Inc(FPadding);
    Result += Pad('{{^}}');
    Inc(FPadding);
    Result += ProgramToStr(block.inverse);
    Dec(FPadding);
    if (block.TheProgram <> nil) then
      Dec(FPadding);
  end;
  Dec(FPadding);
end;

function TASTPrinter.CommentToStr(Comment: THandlebarsCommentStatement): String;
begin
  Result := Pad('{{! ''' + Comment.Value + ''' }}');
end;

function TASTPrinter.ContentToStr(Content: THandlebarsContentStatement): String;
begin
  Result := Pad('CONTENT[ ''' + Content.Value + ''' ]');
end;

function TASTPrinter.DecoratorToStr(Decorator: THandlebarsDecorator): String;
begin
  Result := Pad('{{ DIRECTIVE ' + SubExpressionToStr(TDecoratorSubExpressionAdapter.Create(Decorator)) + ' }}');
end;

function TASTPrinter.DecoratorBlockToStr(Block: THandlebarsDecoratorBlock): String;
begin
  Result := Pad('DIRECTIVE BLOCK:');
  Inc(FPadding);
  Result += Pad(SubExpressionToStr(TDecoratorBlockSubExpressionAdapter.Create(Block)));
  if (Block.TheProgram <> nil) then
  begin
    Result += Pad('PROGRAM:');
    Inc(FPadding);
    Result += ProgramToStr(Block.TheProgram);
    Dec(FPadding);
  end;
  Dec(FPadding);
end;

function TASTPrinter.HashPairToStr(Pair: THandlebarsHashPair): String;
begin
  Result := Pair.Key + '=' + NodeToStr(Pair.Value);
end;

function TASTPrinter.HashToStr(Hash: THandlebarsHash): String;
var
  Pairs: TStringArray;
  i: Integer;
begin
  for i := 0 to Hash.PairCount - 1 do
    Push(Pairs, HashPairToStr(Hash.Pairs[i]));

  Result := 'HASH{' + Join(Pairs, ', ') + '}';
end;

function TASTPrinter.Pad(const Str: String): String;
begin
  Result := PadLeft(Str, FPadding * 2) + '\n';
end;

function TASTPrinter.PathExpressionToStr(Path: THandlebarsPathExpression): String;
var
  PathStr: String;
begin
  PathStr := Join(Path.Parts, '/');
  Result := IfThen(Path.Data, '@') + 'PATH:' + PathStr;
end;

function TASTPrinter.PartialBlockToStr(Partial: THandlebarsPartialBlockStatement): String;
var
  Content, Original: String;
begin
  if Partial.Name is THandlebarsPathExpression then
    Original := THandlebarsPathExpression(Partial.Name).Original
  else
    Original := '';

  Content := 'PARTIAL BLOCK:' + Original;
  if (Partial.ParamCount > 0) then
    Content += ' ' + NodeToStr(Partial.Params[0]);

  if (Partial.Hash <> nil)  then
    content += ' ' + NodeToStr(Partial.Hash);

  content += ' ' + Pad('PROGRAM:');
  Inc(FPadding);
  Content += NodeToStr(Partial.TheProgram);
  Dec(FPadding);

  Result  := Pad('{{> ' + Content + ' }}');
end;

function TASTPrinter.PartialToStr(Partial: THandlebarsPartialStatement): String;
var
  Content, Original: String;
begin
  if Partial.Name is THandlebarsPathExpression then
    Original := THandlebarsPathExpression(Partial.Name).Original
  else
    Original := '';
  Content := 'PARTIAL:' + Original;
  if Partial.ParamCount > 0  then
    Content += ' ' + NodeToStr(Partial.Params[0]);

  if (Partial.Hash <> nil) then
    Content += ' ' + NodeToStr(Partial.Hash);

  Result := Pad('{{> ' + Content + ' }}');
end;

function TASTPrinter.ProgramToStr(AProgram: THandlebarsProgram): String;
var
  BlockParamsStr: String;
  i: Integer;
begin
  Result := '';
  if AProgram.BlockParams <> nil then
  begin
    BlockParamsStr := 'BLOCK PARAMS: [';
    for i := 0 to Length(AProgram.BlockParams) - 1 do
      BlockParamsStr += ' ' + AProgram.BlockParams[i];
    BlockParamsStr += ' ]';
    Result += Pad(BlockParamsStr);
  end;

  for i := 0 to AProgram.BodyCount - 1 do
    Result += NodeToStr(AProgram.Body[i]);

  Dec(FPadding);
end;

function TASTPrinter.MustacheToStr(Mustache: THandlebarsMustacheStatement): String;
begin
  Result := Pad('{{ ' + SubExpressionToStr(TMustacheSubExpressionAdapter.Create(Mustache)) + ' }}');
end;

function TASTPrinter.NodeToStr(Node: THandlebarsNode): String;
begin
  case Node.NodeType of
    'BooleanLiteral':
      Result := BooleanToStr(THandlebarsBooleanLiteral(Node));
    'BlockStatement':
      Result := BlockToStr(THandlebarsBlockStatement(Node));
    'CommentStatement':
      Result := CommentToStr(THandlebarsCommentStatement(Node));
    'ContentStatement':
      Result := ContentToStr(THandlebarsContentStatement(Node));
    'Decorator':
      Result := DecoratorToStr(THandlebarsDecorator(Node));
    'DecoratorBlock':
      Result := DecoratorBlockToStr(THandlebarsDecoratorBlock(Node));
    'Hash':
      Result := HashToStr(THandlebarsHash(Node));
    'HashPair':
      Result := HashPairToStr(THandlebarsHashPair(Node));
    'MustacheStatement':
      Result := MustacheToStr(THandlebarsMustacheStatement(Node));
    'NumberLiteral':
      Result := NumberToStr(THandlebarsNumberLiteral(Node));
    'NullLiteral':
      Result := 'NULL';
    'PartialBlockStatement':
      Result := PartialBlockToStr(THandlebarsPartialBlockStatement(Node));
    'PartialStatement':
      Result := PartialToStr(THandlebarsPartialStatement(Node));
    'PathExpression':
      Result := PathExpressionToStr(THandlebarsPathExpression(Node));
    'Program':
      Result := ProgramToStr(THandlebarsProgram(Node));
    'StringLiteral':
      Result := StringToStr(THandlebarsStringLiteral(Node));
    'UndefinedLiteral':
      Result := 'UNDEFINED';
  else
    raise Exception.CreateFmt('Unknown node type: %s', [Node.NodeType]);
  end;
end;

function TASTPrinter.NumberToStr(Number: THandlebarsNumberLiteral): String;
begin
  Result := 'NUMBER{' + FloatToStr(Number.Value) + '}';
end;

function TASTPrinter.StringToStr(Str: THandlebarsStringLiteral): String;
begin
  Result := '"' + Str.Value + '"';
end;

function TASTPrinter.SubExpressionToStr(SubExpression: ISubExpression): String;
var
  Params: TStringArray;
  HashStr, ParamsStr: String;
  i: Integer;
begin
  for i := 0 to SubExpression.ParamCount - 1 do
    Push(Params, NodeToStr(SubExpression.Params[i]));

  ParamsStr :='[' + Join(Params, ', ') + ']';

  HashStr := '';
  if SubExpression.Hash <> nil then
    HashStr := ' ' + NodeToStr(SubExpression.Hash);
  Result := NodeToStr(SubExpression.Path) + ' ' + ParamsStr + HashStr;
end;

{ TParserTests }

procedure TParserTests.SimpleMustaches;
begin
  CheckEquals('{{ NUMBER{123} [] }}\n', ASTFor('{{123}}'));
  CheckEquals('{{ "foo" [] }}\n', ASTFor('{{"foo"}}'));
  CheckEquals('{{ BOOLEAN{false} [] }}\n', ASTFor('{{false}}'));
  CheckEquals('{{ BOOLEAN{true} [] }}\n', ASTFor('{{true}}'));
  CheckEquals('{{ PATH:foo [] }}\n', ASTFor('{{foo}}'));
  CheckEquals('{{ PATH:foo? [] }}\n', ASTFor('{{foo?}}'));
  CheckEquals('{{ PATH:foo_ [] }}\n', ASTFor('{{foo_}}'));
  CheckEquals('{{ PATH:foo- [] }}\n', ASTFor('{{foo-}}'));
  CheckEquals('{{ PATH:foo: [] }}\n', ASTFor('{{foo:}}'));
end;

procedure TParserTests.SimpleMustachesWithData;
begin
  CheckEquals('{{ @PATH:foo [] }}\n', ASTFor('{{@foo}}'));
end;

procedure TParserTests.SimpleMustachesWithDataPath;
begin
  CheckEquals('{{ @PATH:foo [] }}\n', ASTFor('{{@../foo}}'));
end;

procedure TParserTests.MustachesWithPath;
begin
  CheckEquals('{{ PATH:foo/bar [] }}\n', ASTFor('{{foo/bar}}'));
end;

procedure TParserTests.MustachesWithThis;
begin
  CheckEquals('{{ PATH:foo [] }}\n', ASTFor('{{this/foo}}'));
end;

procedure TParserTests.MustachesWithHyphen;
begin
  CheckEquals('{{ PATH:foo-bar [] }}\n', ASTFor('{{foo-bar}}'));
end;

procedure TParserTests.MustachesWithEscapedBrackets;
begin
  CheckEquals('{{ PATH:foo[] [] }}\n', ASTFor('{{[foo[\]]}}'));
end;

procedure TParserTests.MustachesWithEscapedEscapeChar;
begin
  CheckEquals('{{ PATH:foo\ [] }}\n', ASTFor('{{[foo\\]}}'));
end;

procedure TParserTests.MustachesWithParams;
begin
  CheckEquals('{{ PATH:foo [PATH:bar] }}\n', ASTFor('{{foo bar}}'));
end;

procedure TParserTests.MustachesWithStringParams;
begin
  CheckEquals('{{ PATH:foo [PATH:bar, "baz"] }}\n', ASTFor('{{foo bar "baz" }}'));
end;

procedure TParserTests.MustachesWithNumberParams;
begin
  CheckEquals('{{ PATH:foo [NUMBER{1}] }}\n', ASTFor('{{foo 1}}'));
end;

procedure TParserTests.MustachesWithBooleanParams;
begin
  CheckEquals('{{ PATH:foo [BOOLEAN{true}] }}\n', ASTFor('{{foo true}}'));
  CheckEquals('{{ PATH:foo [BOOLEAN{false}] }}\n', ASTFor('{{foo false}}'));
end;

procedure TParserTests.MustachesWithUndefinedOrNull;
begin
  //path
  CheckEquals('{{ UNDEFINED [] }}\n', ASTFor('{{undefined}}'));
  CheckEquals('{{ NULL [] }}\n', ASTFor('{{null}}'));
  //params
  CheckEquals('{{ PATH:foo [UNDEFINED, NULL] }}\n', ASTFor('{{foo undefined null}}'));
end;

procedure TParserTests.MustachesWithDataParams;
begin
  CheckEquals('{{ PATH:foo [@PATH:bar] }}\n', ASTFor('{{foo @bar}}'));
end;

procedure TParserTests.MustachesWithHashArguments;
begin
  CheckEquals('{{ PATH:foo [] HASH{bar=PATH:baz} }}\n', ASTFor('{{foo bar=baz}}'));
  CheckEquals('{{ PATH:foo [] HASH{bar=NUMBER{1}} }}\n', ASTFor('{{foo bar=1}}'));
  CheckEquals('{{ PATH:foo [] HASH{bar=BOOLEAN{true}} }}\n', ASTFor('{{foo bar=true}}'));
  CheckEquals('{{ PATH:foo [] HASH{bar=BOOLEAN{false}} }}\n', ASTFor('{{foo bar=false}}'));
  CheckEquals('{{ PATH:foo [] HASH{bar=@PATH:baz} }}\n', ASTFor('{{foo bar=@baz}}'));

  CheckEquals('{{ PATH:foo [] HASH{bar=PATH:baz, bat=PATH:bam} }}\n', ASTFor('{{foo bar=baz bat=bam}}'));
  CheckEquals('{{ PATH:foo [] HASH{bar=PATH:baz, bat="bam"} }}\n', ASTFor('{{foo bar=baz bat="bam"}}'));

  CheckEquals('{{ PATH:foo [] HASH{bat="bam"} }}\n', ASTFor('{{foo bat=''bam''}}'));

  CheckEquals('{{ PATH:foo [PATH:omg] HASH{bar=PATH:baz, bat="bam"} }}\n', ASTFor('{{foo omg bar=baz bat="bam"}}'));
  CheckEquals('{{ PATH:foo [PATH:omg] HASH{bar=PATH:baz, bat="bam", baz=NUMBER{1}} }}\n', ASTFor('{{foo omg bar=baz bat="bam" baz=1}}'));
  CheckEquals('{{ PATH:foo [PATH:omg] HASH{bar=PATH:baz, bat="bam", baz=BOOLEAN{true}} }}\n', ASTFor('{{foo omg bar=baz bat="bam" baz=true}}'));
  CheckEquals('{{ PATH:foo [PATH:omg] HASH{bar=PATH:baz, bat="bam", baz=BOOLEAN{false}} }}\n', ASTFor('{{foo omg bar=baz bat="bam" baz=false}}'));
end;

procedure TParserTests.ContentsFollowedByMustache;
begin
  CheckEquals('CONTENT[ ''foo bar '' ]\n{{ PATH:baz [] }}\n', ASTFor('foo bar {{baz}}'));
end;

procedure TParserTests.Partial;
begin
  CheckEquals('{{> PARTIAL:foo }}\n', ASTFor('{{> foo }}'));
  CheckEquals('{{> PARTIAL:foo }}\n', ASTFor('{{> "foo" }}'));
  CheckEquals('{{> PARTIAL:1 }}\n', ASTFor('{{> 1 }}'));
end;

procedure TParserTests.PartialWithContext;
begin
  CheckEquals('{{> PARTIAL:foo PATH:bar }}\n', ASTFor('{{> foo bar}}'));
end;

procedure TParserTests.PartialWithHash;
begin
  CheckEquals('{{> PARTIAL:foo HASH{bar=PATH:bat} }}\n', ASTFor('{{> foo bar=bat}}'));
end;

procedure TParserTests.PartialWithContextAndHash;
begin
  CheckEquals('{{> PARTIAL:foo PATH:bar HASH{bat=PATH:baz} }}\n', ASTFor('{{> foo bar bat=baz}}'));
end;

procedure TParserTests.PartialWithComplexName;
begin
  CheckEquals('{{> PARTIAL:shared/partial?.bar }}\n', ASTFor('{{> shared/partial?.bar}}'));
end;

procedure TParserTests.PartialBlock;
begin
  CheckEquals('{{> PARTIAL BLOCK:foo PROGRAM:\n  CONTENT[ ''bar'' ]\n }}\n', ASTFor('{{#> foo}}bar{{/foo}}'));
end;

procedure TParserTests.BlockMismatch;
begin
  //shouldThrow(function() {
  //  ASTFor('{{#> goodbyes}}{{/hellos}}');
  //}, Error, (/goodbyes doesn't match hellos/));
end;

procedure TParserTests.PartialBlockWithArguments;
begin
  CheckEquals('{{> PARTIAL BLOCK:foo PATH:context HASH{hash=PATH:value} PROGRAM:\n  CONTENT[ ''bar'' ]\n }}\n', ASTFor('{{#> foo context hash=value}}bar{{/foo}}'));
end;

procedure TParserTests.Comment;
begin
  CheckEquals('{{! '' this is a comment '' }}\n', ASTFor('{{! this is a comment }}'));
end;

procedure TParserTests.MultiLineComment;
begin
  CheckEquals('{{! '''+LineEnding+'this is a multi-line comment'+LineEnding+''' }}\n', ASTFor('{{!'+LineEnding+'this is a multi-line comment'+LineEnding+'}}'));
end;

procedure TParserTests.InvertSection;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    CONTENT[ '' bar '' ]\n  {{^}}\n    CONTENT[ '' baz '' ]\n', ASTFor('{{#foo}} bar {{^}} baz {{/foo}}'));
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    CONTENT[ '' bar '' ]\n  {{^}}\n    CONTENT[ '' baz '' ]\n', ASTFor('{{#foo}} bar {{else}} baz {{/foo}}'));
end;

procedure TParserTests.MultipleInvertSections;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    CONTENT[ '' bar '' ]\n  {{^}}\n    BLOCK:\n      PATH:if [PATH:bar]\n      PROGRAM:\n      {{^}}\n        CONTENT[ '' baz '' ]\n', ASTFor('{{#foo}} bar {{else if bar}}{{else}} baz {{/foo}}'));
end;

procedure TParserTests.EmptyBlock;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n', ASTFor('{{#foo}}{{/foo}}'));
end;

procedure TParserTests.EmptyBlockWithEmptyInverse;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n  {{^}}\n', ASTFor('{{#foo}}{{^}}{{/foo}}'));
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n  {{^}}\n', ASTFor('{{#foo}}{{else}}{{/foo}}'));
end;

procedure TParserTests.NonEmptyBlockWithEmptyInverse;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    CONTENT[ '' bar '' ]\n  {{^}}\n', ASTFor('{{#foo}} bar {{^}}{{/foo}}'));
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    CONTENT[ '' bar '' ]\n  {{^}}\n', ASTFor('{{#foo}} bar {{else}}{{/foo}}'));
end;

procedure TParserTests.EmptyBlockWithNonEmptyInverse;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n  {{^}}\n    CONTENT[ '' bar '' ]\n', ASTFor('{{#foo}}{{^}} bar {{/foo}}'));
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n  {{^}}\n    CONTENT[ '' bar '' ]\n', ASTFor('{{#foo}}{{else}} bar {{/foo}}'));
end;

procedure TParserTests.StandaloneInverse;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  {{^}}\n    CONTENT[ ''bar'' ]\n', ASTFor('{{^foo}}bar{{/foo}}'));
end;

procedure TParserTests.ThrowsOnOldInvert;
begin
  //shouldThrow(function() {
  //      ASTFor('{{else foo}}bar{{/foo}}');
  //    }, Error);
end;

procedure TParserTests.BlockWithParams;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n    BLOCK PARAMS: [ bar baz ]\n    CONTENT[ ''content'' ]\n', ASTFor('{{#foo as |bar baz|}}content{{/foo}}'));
end;

procedure TParserTests.InverseBlockWithParams;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  {{^}}\n    BLOCK PARAMS: [ bar baz ]\n    CONTENT[ ''content'' ]\n', ASTFor('{{^foo as |bar baz|}}content{{/foo}}'));
end;

procedure TParserTests.ChainedInverseBlockWithParams;
begin
  CheckEquals('BLOCK:\n  PATH:foo []\n  PROGRAM:\n  {{^}}\n    BLOCK:\n      PATH:foo []\n      PROGRAM:\n        BLOCK PARAMS: [ bar baz ]\n        CONTENT[ ''content'' ]\n', ASTFor('{{#foo}}{{else foo as |bar baz|}}content{{/foo}}'));
end;

procedure TParserTests.ParseErrors;
begin
  //shouldThrow(function() {
  //  ASTFor('foo{{^}}bar');
  //}, Error, /Parse error on line 1/);
  //shouldThrow(function() {
  //  ASTFor('{{foo}');
  //}, Error, /Parse error on line 1/);
  //shouldThrow(function() {
  //  ASTFor('{{foo &}}');
  //}, Error, /Parse error on line 1/);
  //shouldThrow(function() {
  //  ASTFor('{{#goodbyes}}{{/hellos}}');
  //}, Error, /goodbyes doesn't match hellos/);
  //
  //shouldThrow(function() {
  //  ASTFor('{{{{goodbyes}}}} {{{{/hellos}}}}');
  //}, Error, /goodbyes doesn't match hellos/);
end;

procedure TParserTests.InvalidPaths;
begin
  //shouldThrow(function() {
  //  ASTFor('{{foo/../bar}}');
  //}, Error, /Invalid path: foo\/\.\. - 1:2/);
  //shouldThrow(function() {
  //  ASTFor('{{foo/./bar}}');
  //}, Error, /Invalid path: foo\/\. - 1:2/);
  //shouldThrow(function() {
  //  ASTFor('{{foo/this/bar}}');
  //}, Error, /Invalid path: foo\/this - 1:2/);
end;

procedure TParserTests.ReportLineNumbers;
begin
//  shouldThrow(function() {
//    ASTFor('hello\nmy\n{{foo}');
//  }, Error, /Parse error on line 3/);
//  shouldThrow(function() {
//    ASTFor('hello\n\nmy\n\n{{foo}');
//  }, Error, /Parse error on line 5/);
//});
//
//it('knows how to report the correct line number in errors when the first character is a newline', function() {
//  shouldThrow(function() {
//    ASTFor('\n\nhello\n\nmy\n\n{{foo}');
//  }, Error, /Parse error on line 7/);
end;

procedure TParserTests.Directives;
begin
  CheckEquals('DIRECTIVE BLOCK:\n  PATH:foo []\n  PROGRAM:\n', ASTFor('{{#* foo}}{{/foo}}'));
  CheckEquals('{{ DIRECTIVE PATH:foo [] }}\n', ASTFor('{{* foo}}'));
  //it('should fail if directives have inverse', function() {
  //  shouldThrow(function() {
  //    ASTFor('{{#* foo}}{{^}}{{/foo}}');
  //  }, Error, /Unexpected inverse/);
  //});
end;

initialization
  RegisterTest('Parser', TParserTests.Suite);

end.

