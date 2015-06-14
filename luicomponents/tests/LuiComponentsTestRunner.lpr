program LuiComponentsTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, RTTIUtilsTests, luicomponents;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

