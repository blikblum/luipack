unit JSONServiceView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Grids,
  fpjson, AddressBookResources, LuiRESTClient, Dialogs;

type

  { TJSONServiceViewFrame }

  TJSONServiceViewFrame = class(TFrame)
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
    RESTClient: TRESTClient;
    procedure AddContactButtonClick(Sender: TObject);
    procedure AddPhoneButtonClick(Sender: TObject);
    procedure ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure DeletePhoneButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure EditPhoneButtonClick(Sender: TObject);
    procedure LoadDataButtonClick(Sender: TObject);
    procedure RESTClientResponseError(const ResourcePath: String; ResourceTag: PtrInt;
      Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream;
      var ValidData: Boolean);
    procedure RESTClientResponseSuccess(const ResourcePath: String; ResourceTag: PtrInt;
      Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream;
      var ValidData: Boolean);
    procedure SocketError(Sender: TObject; ErrorCode: Integer;
      const ErrorDescription: String);
  private
    FContacts: TJSONArray;
    FContactPhones: TJSONArray;
    function GetSelectedContact: TJSONObject;
    function GetSelectedPhone: TJSONObject;
    procedure UpdateContactPhones(ContactData: TJSONObject);
    procedure UpdateContactsView;
    procedure UpdatePhonesView;
    property SelectedContact: TJSONObject read GetSelectedContact;
    property SelectedPhone: TJSONObject read GetSelectedPhone;
  public
    destructor Destroy; override;
  end;

implementation

uses
  LuiJSONUtils,
  ContactView, PhoneView;

{$R *.lfm}

procedure TJSONServiceViewFrame.ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ContactData: TJSONObject;
begin
  if FContacts = nil then
    Exit;
  if (aRow > 0) and (aRow <= FContacts.Count) then
  begin
    ContactData := FContacts.Objects[aRow - 1];
    UpdateContactPhones(ContactData);
  end;
end;

procedure TJSONServiceViewFrame.DeleteContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  ResourcePath: String;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  ResourcePath := Format('contacts/%d', [ContactData.Integers['id']]);
  if RESTClient.Delete(ResourcePath, RES_CONTACT) then
  begin
    FContacts.Remove(ContactData);
    UpdateContactsView;
  end;
end;

procedure TJSONServiceViewFrame.DeletePhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  ResourcePath: String;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  ResourcePath := Format('contacts/%d/phones/%d', [ContactData.Integers['id'], PhoneData.Integers['id']]);
  if RESTClient.Delete(ResourcePath, RES_CONTACTPHONE) then
  begin
    FContactPhones.Remove(PhoneData);
    UpdatePhonesView;
  end;
end;

procedure TJSONServiceViewFrame.EditContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
  ResourcePath: String;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  if TContactViewForm.EditData(GetParentForm(Self), ContactData) then
  begin
    ResourcePath := Format('contacts/%d', [ContactData.Integers['id']]);
    RESTClient.Put(ResourcePath, RES_CONTACT, ContactData.AsJSON);
  end;
end;

procedure TJSONServiceViewFrame.EditPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  ResourcePath: String;
begin
  ContactData := SelectedContact;
  PhoneData := SelectedPhone;
  if (ContactData = nil) or (PhoneData = nil) then
    Exit;
  if TPhoneViewForm.EditData(GetParentForm(Self), PhoneData) then
  begin
    ResourcePath := Format('contacts/%d/phones/%d', [ContactData.Integers['id'], PhoneData.Integers['id']]);
    RESTClient.Put(ResourcePath, RES_CONTACTPHONE, PhoneData.AsJSON);
  end;
end;

procedure TJSONServiceViewFrame.AddContactButtonClick(Sender: TObject);
var
  ContactData: TJSONObject;
begin
  ContactData := TJSONObject.Create(['name', 'New Contact']);
  if TContactViewForm.EditData(GetParentForm(Self), ContactData) then
    RESTClient.Post('contacts', RES_CONTACTS, ContactData.AsJSON);
  ContactData.Destroy;
end;

procedure TJSONServiceViewFrame.AddPhoneButtonClick(Sender: TObject);
var
  PhoneData, ContactData: TJSONObject;
  ResourcePath: String;
begin
  ContactData := SelectedContact;
  if ContactData = nil then
    Exit;
  PhoneData := TJSONObject.Create(['number', 'New Number']);
  if TPhoneViewForm.EditData(GetParentForm(Self), PhoneData) then
  begin
    ResourcePath := Format('contacts/%d/phones', [ContactData.Integers['id']]);
    RESTClient.Post(ResourcePath, RES_CONTACTPHONES, PhoneData.AsJSON);
  end;
  PhoneData.Destroy;
end;

procedure TJSONServiceViewFrame.LoadDataButtonClick(Sender: TObject);
begin
  RESTClient.BaseURL := BaseURLEdit.Text;
  RESTClient.Get('contacts', RES_CONTACTS);
end;

procedure TJSONServiceViewFrame.RESTClientResponseError(const ResourcePath: String;
  ResourceTag: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream;
  var ValidData: Boolean);
var
  ResponseData: TJSONData;
  Message: String;
begin
  Message := '';
  ResponseData := nil;
  try
    ResponseData := StreamToJSON(ResponseStream);
  except
    SetLength(Message, ResponseStream.Size);
    ResponseStream.Write(Message[1], ResponseStream.Size);
  end;
  if (ResponseData <> nil) and (ResponseData.JSONType = jtObject) then
  begin
    Message := TJSONObject(ResponseData).Get('message', '');
    if Message <> '' then
      Message := LineEnding + Message;
  end;
  ShowMessageFmt('Server response error%s', [Message]);
end;

procedure TJSONServiceViewFrame.RESTClientResponseSuccess(const ResourcePath: String;
  ResourceTag: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream;
  var ValidData: Boolean);
var
  ResponseData: TJSONData;
begin
  ResponseData := StreamToJSON(ResponseStream);
  case Method of
    hmtGet: ValidData := ResponseData <> nil;
    hmtPost, hmtPut:
    begin
      ValidData := ResponseData <> nil;
      if ValidData then
      begin
        ValidData := ResponseData.JSONType = jtObject;
        if not ValidData then
          ShowMessageFmt('Wrong data format. Expected jtObject got %s',
            [JSONTypeName(ResponseData.JSONType)]);
      end;
    end;
  end;
  if not ValidData then
    Exit;
  case ResourceTag of
    RES_CONTACTS:
      begin
        case Method of
          hmtGet:
          begin
            if ResponseData.JSONType = jtArray then
            begin
              FContacts.Free;
              FContacts := TJSONArray(ResponseData);
              UpdateContactsView;
            end;
          end;
          hmtPost:
          begin
            FContacts.Add(ResponseData);
            UpdateContactsView;
          end;
        end;
      end;
    RES_CONTACT:
      begin
        case Method of
          hmtPut:
          begin
            PhonesLabel.Caption := '"' + TJSONObject(ResponseData).Get('name', '') + '" Phones';
            UpdateContactsView;
          end;
        end;
      end;
    RES_CONTACTPHONES:
      begin
        case Method of
          hmtGet:
          begin
            if ResponseData.JSONType = jtArray then
            begin
              FContactPhones.Free;
              FContactPhones := TJSONArray(ResponseData);
              UpdatePhonesView;
            end;
          end;
          hmtPost:
          begin
            FContactPhones.Add(ResponseData);
            UpdatePhonesView;
          end;
        end;
      end;
    RES_CONTACTPHONE:
      begin
        case Method of
          hmtPut:
          begin
            UpdatePhonesView;
          end;
        end;
      end;
  end;
end;


procedure TJSONServiceViewFrame.SocketError(Sender: TObject; ErrorCode: Integer;
  const ErrorDescription: String);
begin
  ShowMessageFmt('Socket Error: "%s"', [ErrorDescription]);
end;

function TJSONServiceViewFrame.GetSelectedContact: TJSONObject;
begin
  if (ContactsGrid.Selection.Top > 0) and (ContactsGrid.Selection.Top <= FContacts.Count) then
    Result := FContacts.Objects[ContactsGrid.Selection.Top - 1]
  else
    Result := nil;
end;

function TJSONServiceViewFrame.GetSelectedPhone: TJSONObject;
begin
  if (PhonesGrid.Selection.Top > 0) and (PhonesGrid.Selection.Top <= FContactPhones.Count) then
    Result := FContactPhones.Objects[PhonesGrid.Selection.Top - 1]
  else
    Result := nil;
end;

procedure TJSONServiceViewFrame.UpdateContactsView;
var
  i: Integer;
  ContactData: TJSONObject;
begin
  ContactsGrid.RowCount := FContacts.Count + 1;
  for i := 0 to FContacts.Count -1 do
  begin
    ContactData := FContacts.Objects[i];
    ContactsGrid.Cells[0, i + 1] := ContactData.Strings['id'];
    ContactsGrid.Cells[1, i + 1] := ContactData.Get('name', '');
  end;
end;

procedure TJSONServiceViewFrame.UpdateContactPhones(ContactData: TJSONObject);
var
  ResourcePath: String;
begin
  if ContactData = nil then
    Exit;
  PhonesLabel.Caption := '"' + ContactData.Get('name', '') + '" Phones';
  ResourcePath := Format('contacts/%d/phones', [ContactData.Integers['id']]);
  RESTClient.Get(ResourcePath, RES_CONTACTPHONES);
end;

procedure TJSONServiceViewFrame.UpdatePhonesView;
var
  i: Integer;
  PhoneData: TJSONObject;
begin
  PhonesGrid.RowCount := FContactPhones.Count + 1;
  for i := 0 to FContactPhones.Count -1 do
  begin
    PhoneData := FContactPhones.Objects[i];
    PhonesGrid.Cells[0, i + 1] := PhoneData.Strings['id'];
    PhonesGrid.Cells[1, i + 1] := PhoneData.Get('number', '');
  end;
end;

destructor TJSONServiceViewFrame.Destroy;
begin
  FContacts.Free;
  FContactPhones.Free;
  inherited Destroy;
end;


end.

