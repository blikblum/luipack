unit MainREST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, AddressBookResources;

type

  { TMainRESTModule }

  TMainRESTModule = class(TRESTServiceModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    FResourceFactory: TAddressBookResourceFactory;
  public
    { public declarations }
  end;

var
  MainRESTModule: TMainRESTModule;

implementation

{$R *.lfm}

{ TMainRESTModule }

procedure TMainRESTModule.DataModuleCreate(Sender: TObject);
begin
  FResourceFactory := TAddressBookResourceFactory.Create(Self);
  FResourceFactory.Database := 'addressbookdata.db';
  Resources.Register('contacts', @FResourceFactory.CreateResource, RES_CONTACTS);
end;

initialization
  RegisterHTTPModule('TMainRESTModule', TMainRESTModule);
end.

