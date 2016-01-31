program LuiComponentsLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, TestFramework,
   RTTIUtilsTests, JSONUtilsTests;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

