program JSONModelLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GUITestRunner, JSONModelTests, luicomponents;

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

