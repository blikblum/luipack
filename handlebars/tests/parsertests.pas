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
  end;

implementation

uses
  sysutils, strutils;

type

  { TASTPrinter }

  TASTPrinter = record
    FPadding: Integer;
    function Pad(const Str: String): String;
    function PathExpressionToStr(Path: THandlebarsPathExpression): String;
    function ProgramToStr(AProgram: THandlebarsProgram): String;
    function MustacheToStr(Mustache: THandlebarsMustacheStatement): String;
    function InnerMustacheToStr(Mustache: THandlebarsMustacheStatement): String;
    function NodeToStr(Node: THandlebarsNode): String;
    function SubExpressionToStr(SubExpression: THandlebarsSubExpression): String;
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

procedure Push(StrArray: TStringArray; const S: String);
var
  Len: Integer;
begin
  Len := Length(StrArray);
  SetLength(StrArray, Len + 1);
  StrArray[Len] := S;
end;

function ASTFor(const Template: String): String;
begin
  Result := '';
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

  for i := 0 to AProgram.Body.Count - 1 do
    Result += NodeToStr(AProgram.Body[i]);

  Dec(FPadding);
end;

function TASTPrinter.MustacheToStr(Mustache: THandlebarsMustacheStatement): String;
begin
  Result := Pad('{{ ' + InnerMustacheToStr(Mustache) + ' }}');
end;

function TASTPrinter.InnerMustacheToStr(Mustache: THandlebarsMustacheStatement): String;
//same as subexpression. in JS it's possible to do duck typing...
var
  Params: TStringArray;
  HashStr, ParamsStr: String;
  i: Integer;
begin
  for i := 0 to Mustache.ParamCount - 1 do
    Push(Params, NodeToStr(Mustache.Params[i]));

  ParamsStr :='[' + Join(Params, ', ') + ']';

  HashStr := IfThen(Mustache.Hash <> nil, NodeToStr(Mustache.Hash));
  Result := NodeToStr(Mustache.Path) + ' ' + ParamsStr + HashStr;
end;

function TASTPrinter.NodeToStr(Node: THandlebarsNode): String;
begin
  case Node.NodeType of
    'Program':
      Result := ProgramToStr(THandlebarsProgram(Node));
  else
    raise Exception.CreateFmt('Unknow node type: %s', [Node.NodeType]);
  end;
end;

function TASTPrinter.SubExpressionToStr(SubExpression: THandlebarsSubExpression): String;
var
  Params: TStringArray;
  HashStr, ParamsStr: String;
  i: Integer;
begin
  for i := 0 to SubExpression.ParamCount - 1 do
    Push(Params, NodeToStr(SubExpression.Params[i]));

  ParamsStr :='[' + Join(Params, ', ') + ']';

  HashStr := IfThen(SubExpression.Hash <> nil, NodeToStr(SubExpression.Hash));
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

end.

