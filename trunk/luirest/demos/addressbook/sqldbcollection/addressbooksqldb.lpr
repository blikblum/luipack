program addressbooksqldb;

{$mode objfpc}{$H+}

uses
  LuiRESTCGI, HTTPDefs, fphttp, MainService, AddressBookResources;

type
  TCallback = object
    procedure GetModuleClass(Sender : TObject; ARequest : TRequest;      Var ModuleClass : TCustomHTTPModuleClass);
  end;

procedure TCallback.GetModuleClass(Sender: TObject; ARequest: TRequest;  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := TMainServiceModule;
end;

var
  Callback: TCallback;
begin
  Application.OnGetModule := @Callback.GetModuleClass;
  Application.Initialize;
  Application.Run;
end.

