unit MainREST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, Sqlite3DS;

type

  { TMainRESTModule }

  TMainRESTModule = class(TRESTServiceModule)
    AddressBookDataset: TSqlite3Dataset;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleResourceLoad(Resource: TRESTResource; ResourceTag: PtrInt);
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
  AddressBookDataset.ExecSQL('PRAGMA foreign_keys = ON');
  Resources.Register('contacts', TContacts, 0);
end;

procedure TMainRESTModule.DataModuleResourceLoad(Resource: TRESTResource; ResourceTag: PtrInt);
begin
  SetObjectProperties(Resource, ['Dataset', AddressBookDataset]);
end;

initialization
  RegisterHTTPModule('TMainRESTModule', TMainRESTModule);
end.

