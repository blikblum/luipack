unit AddressBookSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AddressBookApp;

procedure ConfigureApp(App: TAddressBookApp);

implementation

uses
  PresentationManager,
  AppConfigView;

procedure ConfigureApp(App: TAddressBookApp);
var
  Presentations: TPresentationManager;
begin
  Presentations := App.Presentations;
  Presentations.Register('appconfig', TAppConfigViewForm);
end;

end.

