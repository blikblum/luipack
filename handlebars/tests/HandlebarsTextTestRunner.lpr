program HandlebarsTextTestRunner;

{$mode objfpc}{$H+}

uses
  TextTestRunner,
  //tests
  BasicTests,
  MustacheTests, ScannerTests;

begin
  RunRegisteredTests;
end.

