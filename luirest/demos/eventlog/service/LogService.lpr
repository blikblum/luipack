program LogService;

{$mode objfpc}{$H+}

uses
  LuiRESTCGI, HTTPDefs, fphttp, MainService;

type
  TCallback = object
    procedure GetModuleClass(Sender : TObject; ARequest : TRequest;
      Var ModuleClass : TCustomHTTPModuleClass);
  end;

procedure TCallback.GetModuleClass(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := TLogServiceModule;
end;

var
  Callback: TCallback;
begin
  Application.OnGetModule := @Callback.GetModuleClass;
  Application.Initialize;
  Application.Run;
end.

