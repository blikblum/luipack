unit MainPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, fpjson, SimpleJSONModel,
  ContactModel, CategoryModel, ContactPhoneModel;

type

  { TMainPresenter }

  TMainPresenter = class(TBasePresenter)
  private
    FCategories: TSimpleJSONCollection;
    FContactPhones: TContactPhones;
    FContacts: TContacts;
    FSelectedContactData: TJSONObject;
    FSelectedPhoneData: TJSONObject;
    procedure SetSelectedContactData(Value: TJSONObject);
    procedure SetSelectedPhoneData(Value: TJSONObject);
    procedure UpdateContactPhones;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddContact;
    procedure AddPhone;
    procedure DeleteContact;
    procedure DeletePhone;
    procedure EditContact;
    procedure EditPhone;
    procedure Initialize; override;
    function GetContactCategory(ContactData: TJSONObject): String;
    property ContactPhones: TContactPhones read FContactPhones;
    property Contacts: TContacts read FContacts;
    property SelectedContactData: TJSONObject read FSelectedContactData write SetSelectedContactData;
    property SelectedPhoneData: TJSONObject read FSelectedPhoneData write SetSelectedPhoneData;
  end;


implementation

uses
  LuiJSONUtils;

{ TMainPresenter }

procedure TMainPresenter.SetSelectedContactData(Value: TJSONObject);
begin
  if FSelectedContactData = Value then Exit;
  FSelectedContactData := Value;
  UpdateContactPhones;
end;

procedure TMainPresenter.SetSelectedPhoneData(Value: TJSONObject);
begin
  if FSelectedPhoneData = Value then Exit;
  FSelectedPhoneData := Value;
end;

procedure TMainPresenter.UpdateContactPhones;
begin
  if FSelectedContactData <> nil then
  begin
    FContactPhones.ParamByName('contactid').AsLargeInt := FSelectedContactData.Int64s['id'];
    FContactPhones.Fetch;
  end;
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
  FContactPhones := TContactPhones.Create;
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
  Presentations['contact'].ShowModal(['Contact', Contact]);
  Contact.Free;
end;

procedure TMainPresenter.AddPhone;
var
  Phone: TContactPhone;
begin
  Phone := FContactPhones.Add;
  Presentations['contactphone'].ShowModal(['Phone', Phone]);
  Phone.Free;
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

procedure TMainPresenter.DeletePhone;
var
  Phone: TContactPhone;
begin
  if FSelectedPhoneData = nil then
    Exit;
  Phone := FContactPhones.GetItem(FSelectedPhoneData);
  Phone.Delete;
  Phone.Free;
end;

procedure TMainPresenter.EditContact;
var
  Contact: TContact;
begin
  if FSelectedContactData = nil then
    Exit;
  Contact := FContacts.GetItem(FSelectedContactData);
  Presentations['contact'].ShowModal(['Contact', Contact]);
  Contact.Free;
end;

procedure TMainPresenter.EditPhone;
var
  Phone: TContactPhone;
begin
  if FSelectedPhoneData = nil then
    Exit;
  Phone := FContactPhones.GetItem(FSelectedPhoneData);
  Presentations['contactphone'].ShowModal(['Phone', Phone]);
  Phone.Free;
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

