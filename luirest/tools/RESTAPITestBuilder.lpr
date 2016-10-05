program RESTAPITestBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, vtvutils_package, luicontrols, MainView, RESTAPI, EndPointView, 
JSONSchemaBuilder, JSONSchemaBuilderView
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TJSONSchemaBuilderForm, JSONSchemaBuilderForm);
  Application.Run;
end.

