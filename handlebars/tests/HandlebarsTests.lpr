program HandlebarsTests;

{$mode objfpc}{$H+}

uses
  TextTestRunner,
  //tests
  BasicTests,
  MustacheTests;

begin
  RunRegisteredTests;
end.

