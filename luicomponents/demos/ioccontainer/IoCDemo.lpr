program IoCDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainView, AppServices, MyService, MyServiceDummyImpl;

{$R *.res}

procedure GetMyServiceDummyInstance(out Instance: TObject);
begin
  Instance := TMyServiceDummyImplementation.Create;
end;

begin
  Services.Register(IMyService, @GetMyServiceDummyInstance);
  Services.Register(IMySecondService, TMySecondServiceDummyComponent);
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

