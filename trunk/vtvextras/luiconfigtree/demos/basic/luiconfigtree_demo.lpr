program luiconfigtree_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, fMain, LResources;

{$IFDEF WINDOWS}{$R luiconfigtree_demo.rc}{$ENDIF}

begin
  {$I luiconfigtree_demo.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

