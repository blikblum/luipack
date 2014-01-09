unit MainService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, AddressBookResources;

type

  { TMainServiceModule }

  TMainServiceModule = class(TRESTServiceModule)
  private
    FResourceFactory: TAddressBookResourceFactory;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainServiceModule: TMainServiceModule;

implementation

{$R *.lfm}

{ TMainServiceModule }

constructor TMainServiceModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResourceFactory := TAddressBookResourceFactory.Create(Self);
  FResourceFactory.Database := 'addressbookdata.db';
  Resources.Register('contacts', @FResourceFactory.CreateResource, RES_CONTACTS);
  Resources.Register('categories', @FResourceFactory.CreateResource, RES_CATEGORIES);
end;

end.

