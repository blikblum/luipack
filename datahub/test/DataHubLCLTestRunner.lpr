program DataHubLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, QuickDefinitionTest, luicomponents, 
FPTestHelpers;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

