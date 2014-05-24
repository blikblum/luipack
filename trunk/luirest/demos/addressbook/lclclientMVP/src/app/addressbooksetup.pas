unit AddressBookSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AddressBookApp;

procedure ConfigureApp(App: TAddressBookApp);

implementation

uses
  PresentationManager,
  AppConfigView,
  ContactPresenter, ContactView,
  ContactPhoneView,
  CategoriesView
  ;

procedure ConfigureApp(App: TAddressBookApp);
var
  Presentations: TPresentationManager;
begin
  Presentations := App.Presentations;
  Presentations.Register('appconfig', TAppConfigViewForm);
  Presentations.Register('contact', TContactForm, TContactPresenter);
  Presentations.Register('contactphone', TContactPhoneForm);
  Presentations.Register('categories', TCategoriesForm);
end;

end.

