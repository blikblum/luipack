unit BasicTests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, HandlebarsTestCase;

type

  { TBasicTests }

  TBasicTests = class(THandlebarsTestCase)
  published
    procedure TestMostBasic;
  end;

implementation

{ TBasicTests }

procedure TBasicTests.TestMostBasic;
begin
  CheckRender('{{foo}}', ['foo', 'foo'], 'foo');
end;

initialization
  RegisterTest(TBasicTests.Suite);

end.

