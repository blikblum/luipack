program CAMICUAdmin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils, Forms, Controls, MainView, CAMICUApp,
  CAMICUAppSetup, MainPresenter, PatientModel,
  //needed to register
  JSONZVDateTimeMediator, laz_fpspreadsheet,
  PatientCadastreView, DataExporter, EvaluationModel
  ;

{$R *.res}

var
  App: TCAMICUApp;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  //bootstrap app
  App := TCAMICUApp.Create(Application);
  ConfigureApp(App);
  App.Initialize;
  while not App.ConnectToService do
  begin
    if App.Presentations['appconfig'].ShowModal(['Config', App.Config]) <> mrOK then
    begin
      Application.ShowMainForm := False;
      Application.Terminate;
      break;
    end;
  end;
  if not Application.Terminated then
  begin
    Application.CreateForm(TMainForm, MainForm);
    MainForm.Caption := Format('%s (%s)', [MainForm.Caption, App.Config.BaseURL]);
    //setup MainView manually
    MainForm.Presenter := TMainPresenter.Create(Application);
    MainForm.Presenter.Initialize;
  end;
  Application.Run;
end.

