unit MainPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, fpjson, SimpleJSONModel, ContactModel, CategoryModel;

type

  { TMainPresenter }

  TMainPresenter = class(TBasePresenter)
  private
    FCategories: TSimpleJSONCollection;
    FContacts: TContacts;
    FSelectedContactData: TJSONObject;
    procedure SetSelectedContactData(Value: TJSONObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddContact;
    procedure DeleteContact;
    procedure EditContact;
    procedure Initialize; override;
    function GetContactCategory(ContactData: TJSONObject): String;
    property Contacts: TContacts read FContacts;
    property SelectedContactData: TJSONObject read FSelectedContactData write SetSelectedContactData;
  end;


implementation

uses
  LuiJSONUtils;

{ TMainPresenter }

procedure TMainPresenter.SetSelectedContactData(Value: TJSONObject);
begin
  if FSelectedContactData = Value then Exit;
  FSelectedContactData := Value;
end;

procedure TMainPresenter.Initialize;
begin
  inherited Initialize;
  FCategories.Fetch;
  FContacts.Fetch;
end;

constructor TMainPresenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCategories := TSimpleJSONCollection.Create(TCategory);
  FContacts := TContacts.Create;
end;

destructor TMainPresenter.Destroy;
begin
  FCategories.Free;
  FContacts.Free;
  inherited Destroy;
end;

procedure TMainPresenter.AddContact;
var
  Contact: TContact;
begin
  Contact := FContacts.Add;
  Contact.Free;
end;

procedure TMainPresenter.DeleteContact;
var
  Contact: TContact;
begin
  if FSelectedContactData = nil then
    Exit;
  Contact := FContacts.GetItem(FSelectedContactData);
  Contact.Delete;
  Contact.Free;
end;

procedure TMainPresenter.EditContact;
var
  Contact: TContact;
begin
  if FSelectedContactData = nil then
    Exit;
  Contact := FContacts.GetItem(FSelectedContactData);

  Contact.Free;
end;

function TMainPresenter.GetContactCategory(ContactData: TJSONObject): String;
var
  CategoryData: TJSONObject;
  CategoryId: Int64;
begin
  Result := '';
  if FindJSONProp(ContactData, 'categoryid', CategoryId) then
  begin
    CategoryData := FindJSONObject(FCategories.Data, ['id', CategoryId]);
    if CategoryData <> nil then
      Result := CategoryData.Get('name');
  end;
end;

end.

