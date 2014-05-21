unit JSONResourceView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Grids,
  LuiRESTClient, LuiDataClasses, fpjson, Dialogs;

type

  { TJSONResourceViewFrame }

  TJSONResourceViewFrame = class(TFrame)
    AddContactButton: TButton;
    AddPhoneButton: TButton;
    BaseURLEdit: TLabeledEdit;
    ContactsGrid: TStringGrid;
    DeleteContactButton: TButton;
    DeletePhoneButton: TButton;
    EditContactButton: TButton;
    EditPhoneButton: TButton;
    Label1: TLabel;
    LoadDataButton: TButton;
    PhonesGrid: TStringGrid;
    PhonesLabel: TLabel;
    Resources: TRESTResourceClient;
    procedure AddContactButtonClick(Sender: TObject);
    procedure AddPhoneButtonClick(Sender: TObject);
    procedure ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure DeletePhoneButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure EditPhoneButtonClick(Sender: TObject);
    procedure LoadDataButtonClick(Sender: TObject);
    procedure ResourcesError(Sender: TObject; const ResourcePath: String;
      ErrorType: TRESTErrorType; ErrorCode: Integer; const ErrorMessage: String;
      var Handled: Boolean);
  private
    FContacts: IJSONArrayResource;
    FContactPhones: IJSONArrayResource;
    function GetSelectedContact: TJSONObject;
    function GetSelectedPhone: TJSONObject;
    procedure UpdateContactPhones(ContactData: TJSONObject);
    procedure UpdateContactsView;
    procedure UpdatePhonesView;
    property SelectedContact: TJSONObject read GetSelectedContact;
    property SelectedPhone: TJSONObject read GetSelectedPhone;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  JSONPhoneView, JSONContactView,
  LuiJSONUtils;

{$R *.lfm}

{ TJSONResourceViewFrame }

procedure TJSONResourceViewFrame.LoadDataButtonClick(Sender: TObject);
begin
  Resources.BaseURL := BaseURLEdit.Text;
  if FContacts.Fetch then
    UpdateContactsView;
end;

procedure TJSONResourceViewFrame.ResourcesError(Sender: TObject; const ResourcePath: String;
  ErrorType: TRESTErrorType; ErrorCode: Integer; const ErrorMessage: String; var Handled: Boolean);
var
  ErrorTypeStr: String;
begin
  WriteStr(ErrorTypeStr, ErrorType);
  Handled := True;
  ShowMessageFmt('%s - %d - %s',[ErrorTypeStr, ErrorCode, ErrorMessage]);
end;
constructor TJSONResourceViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FContacts := Resources.GetJSONArray('contact');
  FContactPhones := Resources.GetJSONArray('contactphone');
end;

procedure TJSONResourceViewFrame.ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ContactData: TJSONObject;
begin
  if FContacts.Data = nil then
    Exit;
  if (aRow > 0) and (aRow <= FContacts.Data.Count) then
  begin
    ContactData := FContacts.Data.Objects[aRow - 1];
    UpdateContactPhones(ContactData);
  end;
end;

procedure TJSONResourceViewFrame.DeleteContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  ContactResource: IJSONObjectResource;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  ContactResource := Resources.GetJSONObject('contact');
  ContactResource.SetData(ContactData, False);
  if ContactResource.Delete then
  begin
    FContacts.Data.Remove(ContactData);
    UpdateContactsView;
  end;
end;

procedure TJSONResourceViewFrame.DeletePhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  PhoneResource: IJSONObjectResource;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  PhoneResource := Resources.GetJSONObject('contactphone');
  PhoneResource.SetData(PhoneData, False);
  PhoneResource.ParamByName('contactid').AsInteger := ContactData.Integers['id'];
  if PhoneResource.Delete then
  begin
    FContactPhones.Data.Remove(PhoneData);
    UpdatePhonesView;
  end;
end;

procedure TJSONResourceViewFrame.EditContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  ContactResource: IJSONObjectResource;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  if TJSONContactViewForm.EditData(GetParentForm(Self), Resources, ContactData) then
  begin
    ContactResource := Resources.GetJSONObject('contact');
    ContactResource.SetData(ContactData, False);
    if ContactResource.Save then
      UpdateContactsView;
  end;
end;

procedure TJSONResourceViewFrame.EditPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  PhoneResource: IJSONObjectResource;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  if TJSONPhoneViewForm.EditData(GetParentForm(Self), PhoneData) then
  begin
    PhoneResource := Resources.GetJSONObject('contactphone');
    PhoneResource.ParamByName('contactid').AsInteger := ContactData.Integers['id'];
    PhoneResource.SetData(PhoneData, False);
    if PhoneResource.Save then
      UpdatePhonesView;
  end;
end;

procedure TJSONResourceViewFrame.AddContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  ContactResource: IJSONObjectResource;
begin
  ContactData := TJSONObject.Create(['name', 'New Contact']);
  if TJSONContactViewForm.EditData(GetParentForm(Self), Resources, ContactData) then
  begin
    ContactResource := Resources.GetJSONObject('contact');
    ContactResource.SetData(ContactData, True);
    if ContactResource.Save then
    begin
      FContacts.Data.Add(ContactData.Clone);
      UpdateContactsView;
    end;
  end;
end;

procedure TJSONResourceViewFrame.AddPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  PhoneResource: IJSONObjectResource;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  PhoneData := TJSONObject.Create(['number', 'New Number']);
  if TJSONPhoneViewForm.EditData(GetParentForm(Self), PhoneData) then
  begin
    PhoneResource := Resources.GetJSONObject('contactphone');
    PhoneResource.ParamByName('contactid').AsInteger := ContactData.Integers['id'];
    PhoneResource.SetData(PhoneData, True);
    if PhoneResource.Save then
    begin
      FContactPhones.Data.Add(PhoneData.Clone);
      UpdatePhonesView;
    end;
  end;
end;

function TJSONResourceViewFrame.GetSelectedContact: TJSONObject;
begin
  if (ContactsGrid.Selection.Top > 0) and (ContactsGrid.Selection.Top <= FContacts.Data.Count) then
    Result := FContacts.Data.Objects[ContactsGrid.Selection.Top - 1]
  else
    Result := nil;
end;

function TJSONResourceViewFrame.GetSelectedPhone: TJSONObject;
begin
  if (PhonesGrid.Selection.Top > 0) and (PhonesGrid.Selection.Top <= FContactPhones.Data.Count) then
    Result := FContactPhones.Data.Objects[PhonesGrid.Selection.Top - 1]
  else
    Result := nil;
end;

procedure TJSONResourceViewFrame.UpdateContactsView;
var
  i, CategoryId, CategoryIndex: Integer;
  ContactData: TJSONObject;
  Categories: IJSONArrayResource;
  CategoryName: String;
begin
  Categories := Resources.GetJSONArray('category');
  Categories.Fetch;
  ContactsGrid.RowCount := FContacts.Data.Count + 1;
  for i := 0 to FContacts.Data.Count - 1 do
  begin
    ContactData := FContacts.Data.Objects[i];
    ContactsGrid.Cells[0, i + 1] := ContactData.Strings['id'];
    ContactsGrid.Cells[1, i + 1] := ContactData.Get('name', '');
    CategoryName := '';
    CategoryId := ContactData.Get('categoryid', -1);
    if CategoryId <> -1 then
    begin
      CategoryIndex := GetJSONIndexOf(Categories.Data, ['id', CategoryId]);
      if CategoryIndex <> -1 then
        CategoryName := Categories.Data.Objects[CategoryIndex].Strings['name'];
    end;
    ContactsGrid.Cells[2, i + 1] := CategoryName;
  end;
end;

procedure TJSONResourceViewFrame.UpdateContactPhones(ContactData: TJSONObject);
begin
  if ContactData = nil then
    Exit;
  PhonesLabel.Caption := ContactData.Strings['name'] + ' Phones';
  FContactPhones.ParamByName('contactid').AsInteger := ContactData.Integers['id'];
  if FContactPhones.Fetch then
    UpdatePhonesView;
end;

procedure TJSONResourceViewFrame.UpdatePhonesView;
var
  i: Integer;
  PhoneData: TJSONObject;
begin
  PhonesGrid.RowCount := FContactPhones.Data.Count + 1;
  for i := 0 to FContactPhones.Data.Count -1 do
  begin
    PhoneData := FContactPhones.Data.Objects[i];
    PhonesGrid.Cells[0, i + 1] := PhoneData.Strings['id'];
    PhonesGrid.Cells[1, i + 1] := PhoneData.Get('number', '');
  end;
end;


end.

