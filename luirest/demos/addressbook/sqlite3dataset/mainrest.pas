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
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse);
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
  Resources.Register('categories', TCategories, 0);
  Resources.Register('info', TServiceInfoResource, 0);
end;

procedure TMainRESTModule.DataModuleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse);
begin
  if ARequest.QueryFields.Values['format'] = 'xml' then
  begin
    ResponseFormatter := TXMLRESTResponseFormatter;
    ContentType := 'application/xml; charset=UTF-8';
    TSqlite3DatasetResource.DatasetFormatter := TXMLDatasetResourceFormatter;
  end
  else
  begin
    ResponseFormatter := TJSONRESTResponseFormatter;
    ContentType := 'application/json; charset=UTF-8';
    TSqlite3DatasetResource.DatasetFormatter := TJSONDatasetResourceFormatter;
  end;
end;

procedure TMainRESTModule.DataModuleResourceLoad(Resource: TRESTResource; ResourceTag: PtrInt);
begin
  SetObjectProperties(Resource, ['Dataset', AddressBookDataset]);
end;

end.

