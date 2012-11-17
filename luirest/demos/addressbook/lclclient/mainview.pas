unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, fpjson, httpsend;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddContactButton: TButton;
    EditContactButton: TButton;
    DeleteContactButton: TButton;
    AddPhoneButton: TButton;
    EditPhoneButton: TButton;
    DeletePhoneButton: TButton;
    Label1: TLabel;
    PhonesLabel: TLabel;
    LoadDataButton: TButton;
    BaseURLEdit: TLabeledEdit;
    ContactsGrid: TStringGrid;
    PhonesGrid: TStringGrid;
    procedure AddContactButtonClick(Sender: TObject);
    procedure AddPhoneButtonClick(Sender: TObject);
    procedure ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure DeletePhoneButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure EditPhoneButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadDataButtonClick(Sender: TObject);
  private
    function DeleteData(const ResourcePath: String): Boolean;
    function GetData(var Data: TJSONArray; const ResourcePath: String): Boolean;
    function SaveData(Data: TJSONObject; const Method, ResourcePath: String): Boolean;
    function GetBaseURL: String;
    function GetSelectedContact: TJSONObject;
    function GetSelectedPhone: TJSONObject;
    procedure UpdateContactsView;
    procedure UpdateContactPhones(ContactData: TJSONObject);
    procedure UpdatePhonesView;
  private
    { private declarations }
    FContacts: TJSONArray;
    FContactPhones: TJSONArray;
    FHttpClient: THTTPSend;
    property BaseURL: String read GetBaseURL;
    property SelectedContact: TJSONObject read GetSelectedContact;
    property SelectedPhone: TJSONObject read GetSelectedPhone;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  LuiJSONUtils, ContactView, PhoneView;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHttpClient.Destroy;
  FContactPhones.Free;
  FContacts.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FHttpClient := THTTPSend.Create;
end;

procedure TMainForm.ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ContactData: TJSONObject;
begin
  if (aRow > 0) and (aRow <= FContacts.Count) then
  begin
    ContactData := FContacts.Objects[aRow - 1];
    UpdateContactPhones(ContactData);
  end;
end;

procedure TMainForm.DeleteContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  if DeleteData(Format('contacts/%d', [ContactData.Integers['id']])) then
  begin
    FContacts.Remove(ContactData);
    UpdateContactsView;
  end;
end;

procedure TMainForm.DeletePhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  if DeleteData(Format('contacts/%d/phones/%d',
    [ContactData.Integers['id'], PhoneData.Integers['id']])) then
  begin
    FContactPhones.Remove(PhoneData);
    UpdatePhonesView;
  end;
end;

procedure TMainForm.EditContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  if TContactViewForm.EditData(Self, ContactData) then
  begin
    if SaveData(ContactData, 'PUT', Format('contacts/%d',
      [ContactData.Integers['id']])) then
    begin
      PhonesLabel.Caption := ContactData.Strings['name'] + ' Phones';
      UpdateContactsView;
    end;
  end;
end;

procedure TMainForm.EditPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  if TPhoneViewForm.EditData(Self, PhoneData) then
  begin
    if SaveData(PhoneData, 'PUT', Format('contacts/%d/phones/%d',
      [ContactData.Integers['id'], PhoneData.Integers['id']])) then
    begin
      UpdatePhonesView;
    end;
  end;
end;

procedure TMainForm.AddContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  OwnsData: Boolean;
begin
  ContactData := TJSONObject.Create(['name', 'New Contact']);
  OwnsData := True;
  if TContactViewForm.EditData(Self, ContactData) then
  begin
    if SaveData(ContactData, 'POST', 'contacts') then
    begin
      OwnsData := False;
      FContacts.Add(ContactData);
      UpdateContactsView;
    end;
  end;
  if OwnsData then
    ContactData.Destroy;
end;

procedure TMainForm.AddPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  OwnsData: Boolean;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  PhoneData := TJSONObject.Create(['number', 'New Number']);
  OwnsData := True;
  if TPhoneViewForm.EditData(Self, PhoneData) then
  begin
    if SaveData(PhoneData, 'POST', Format('contacts/%d/phones', [ContactData.Integers['id']])) then
    begin
      OwnsData := False;
      FContactPhones.Add(PhoneData);
      UpdatePhonesView;
    end;
  end;
  if OwnsData then
    PhoneData.Destroy;
end;

procedure TMainForm.LoadDataButtonClick(Sender: TObject);
begin
  if GetData(FContacts, 'contacts') then
  begin
    UpdateContactsView;
    BaseURLEdit.Enabled := False;
    LoadDataButton.Enabled := False;
  end;
end;

function TMainForm.DeleteData(const ResourcePath: String): Boolean;
begin
  Result := False;
  FHttpClient.Clear;
  if FHttpClient.HTTPMethod('DELETE', BaseURL + ResourcePath) then
  begin
    Result := FHttpClient.ResultCode < 300;
  end;
  if not Result then
    ShowMessageFmt('Error deleting %s', [ResourcePath]);
end;

function TMainForm.GetData(var Data: TJSONArray; const ResourcePath: String): Boolean;
begin
  Result := False;
  FHttpClient.Clear;
  if FHttpClient.HTTPMethod('GET', BaseURL + ResourcePath) then
  begin
    FreeAndNil(Data);
    Data := StreamToJSONData(FHttpClient.Document) as TJSONArray;
    Result := True;
  end
  else
    ShowMessageFmt('Error loading %s', [ResourcePath ]);
end;

function TMainForm.SaveData(Data: TJSONObject; const Method, ResourcePath: String): Boolean;
var
  JSONStr: String;
  TempData: TJSONData;
begin
  Result := False;
  JSONStr := Data.AsJSON;
  FHttpClient.Clear;
  FHttpClient.Document.Write(JSONStr[1], Length(JSONStr));
  if FHttpClient.HTTPMethod(Method, BaseURL + ResourcePath) then
  begin
    if FHttpClient.ResultCode < 300 then
    begin
      TempData := StreamToJSONData(FHttpClient.Document);
      Data.Clear;
      CopyJSONObject(TempData as TJSONObject, Data);
      TempData.Destroy;
      Result := True;
    end
    else
      ShowMessageFmt('Error saving (%s) resource %s: %s', [Method, ResourcePath, FHttpClient.ResultString]);
  end
  else
    ShowMessageFmt('Error saving (%s) to %s', [Method, ResourcePath]);
end;

function TMainForm.GetBaseURL: String;
begin
  Result := BaseURLEdit.Text;
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
end;

function TMainForm.GetSelectedContact: TJSONObject;
begin
  if (ContactsGrid.Selection.Top > 0) and (ContactsGrid.Selection.Top <= FContacts.Count) then
    Result := FContacts.Objects[ContactsGrid.Selection.Top - 1]
  else
    Result := nil;
end;

function TMainForm.GetSelectedPhone: TJSONObject;
begin
  if (PhonesGrid.Selection.Top > 0) and (PhonesGrid.Selection.Top <= FContactPhones.Count) then
    Result := FContactPhones.Objects[PhonesGrid.Selection.Top - 1]
  else
    Result := nil;
end;

procedure TMainForm.UpdateContactsView;
var
  i: Integer;
  ContactData: TJSONObject;
begin
  ContactsGrid.RowCount := FContacts.Count + 1;
  for i := 0 to FContacts.Count -1 do
  begin
    ContactData := FContacts.Objects[i];
    ContactsGrid.Cells[0, i + 1] := ContactData.Strings['id'];
    ContactsGrid.Cells[1, i + 1] := ContactData.Strings['name'];
  end;
end;

procedure TMainForm.UpdateContactPhones(ContactData: TJSONObject);
begin
  if ContactData = nil then
    Exit;
  PhonesLabel.Caption := ContactData.Strings['name'] + ' Phones';
  if GetData(FContactPhones, Format('contacts/%d/phones', [ContactData.Integers['id']])) then
    UpdatePhonesView;
end;

procedure TMainForm.UpdatePhonesView;
var
  i: Integer;
  PhoneData: TJSONObject;
begin
  PhonesGrid.RowCount := FContactPhones.Count + 1;
  for i := 0 to FContactPhones.Count -1 do
  begin
    PhoneData := FContactPhones.Objects[i];
    PhonesGrid.Cells[0, i + 1] := PhoneData.Strings['id'];
    PhonesGrid.Cells[1, i + 1] := PhoneData.Strings['number'];
  end;
end;

end.

