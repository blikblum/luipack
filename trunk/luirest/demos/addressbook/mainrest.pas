unit MainREST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, fphttp, HTTPDefs, LuiREST, Sqlite3DS;

type

  { TMainRESTModule }

  TMainRESTModule = class(TRESTServiceModule)
    AddressBookDataset: TSqlite3Dataset;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleCreateResource(Resource: TCustomRESTResource);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainRESTModule: TMainRESTModule;

implementation

uses
  AddressBookResources, LuiRTTIUtils;

{$R *.lfm}

{ TMainRESTModule }

procedure TMainRESTModule.DataModuleCreate(Sender: TObject);
begin
  RegisterResource('contacts', TContacts);
end;

procedure TMainRESTModule.DataModuleCreateResource(Resource: TCustomRESTResource);
begin
  SetObjectProperties(Resource, ['Dataset', AddressBookDataset]);
end;

initialization
  RegisterHTTPModule('TMainRESTModule', TMainRESTModule);
end.

