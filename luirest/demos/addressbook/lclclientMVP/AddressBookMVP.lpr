program AddressBookMVP;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Controls, MainView, AddressBookApp, AddressBookSetup, MainPresenter,
  ContactModel, CategoryModel, ContactView, ContactPresenter, CategoriesView,
  ContactPhoneModel, ContactPhoneView;

{$R *.res}

var
  App: TAddressBookApp;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  //bootstrap app
  App := TAddressBookApp.Create(Application);
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
    //setup MainView manually
    MainForm.Presenter := TMainPresenter.Create(Application);
    MainForm.Presenter.Initialize;
  end;
  Application.CreateForm(TContactForm, ContactForm);
  Application.CreateForm(TCategoriesForm, CategoriesForm);
  Application.CreateForm(TContactPhoneForm, ContactPhoneForm);
  Application.Run;
end.

