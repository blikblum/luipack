program simplerest_demo;

{$mode objfpc}{$H+}

uses
  fpCGI, MainREST, RESTResources, HTTPDefs, fphttp;

type

  { TCalback }

  TCallback = object
    procedure GetModuleClass(Sender : TObject; ARequest : TRequest;
       Var ModuleClass : TCustomHTTPModuleClass);
  end;

{ TCalback }

procedure TCallback.GetModuleClass(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := TMainRESTModule;
end;

var
  Callback: TCallback;
begin
  Application.Initialize;
  Application.OnGetModule := @Callback.GetModuleClass;
  Application.Run;
end.

