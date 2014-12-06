program JSONUtilsTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestJSONUtils, luicomponents;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

