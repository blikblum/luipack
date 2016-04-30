program LuiComponentsLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, TestFramework,
   RTTIUtilsTests, JSONUtilsTests, IocContainerTests;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

