program addressbookservice;

{$mode objfpc}{$H+}

uses
  LuiRESTCGI, luicomponents, MainREST, HTTPDefs, fphttp, AddressBookResources;

type

  { TCallback }

  TCallback = object
    procedure GetModuleClass(Sender : TObject; ARequest : TRequest;
       Var ModuleClass : TCustomHTTPModuleClass);
  end;

{ TCallback }

procedure TCallback.GetModuleClass(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := TMainRESTModule;
end;

var
  Callback: TCallback;

begin
  Application.OnGetModule := @Callback.GetModuleClass;
  Application.Initialize;
  Application.Run;
end.

