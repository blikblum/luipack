program HandlebarsGUITestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner,
  BasicTests, MustacheTests, ScannerTests, luicomponents;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

