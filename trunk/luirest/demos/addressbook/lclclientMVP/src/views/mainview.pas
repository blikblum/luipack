unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, MainPresenter, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm, IFPObserver)
    AddContactButton: TButton;
    AddPhoneButton: TButton;
    ContactsGrid: TStringGrid;
    DeleteContactButton: TButton;
    DeletePhoneButton: TButton;
    EditContactButton: TButton;
    EditPhoneButton: TButton;
    Label1: TLabel;
    PhonesGrid: TStringGrid;
    PhonesLabel: TLabel;
    procedure AddContactButtonClick(Sender: TObject);
    procedure AddPhoneButtonClick(Sender: TObject);
    procedure ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure DeletePhoneButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure EditPhoneButtonClick(Sender: TObject);
    procedure PhonesGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FPresenter: TMainPresenter;
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure SetPresenter(Value: TMainPresenter);
    procedure UpdateContactListView;
    procedure UpdateContactPhoneListView;
    procedure UpdatePhonesLabel;
  public
  published
    property Presenter: TMainPresenter read FPresenter write SetPresenter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddContactButtonClick(Sender: TObject);
begin
  FPresenter.AddContact;
end;

procedure TMainForm.AddPhoneButtonClick(Sender: TObject);
begin
  FPresenter.AddPhone;
end;

procedure TMainForm.ContactsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  ContactData: TJSONObject;
begin
  if (aRow > 0) and (aRow <= FPresenter.Contacts.Data.Count) then
  begin
    ContactData := FPresenter.Contacts.Data.Objects[aRow - 1];
    FPresenter.SelectedContactData := ContactData;
  end;
  if FPresenter <> nil then
    UpdatePhonesLabel;
end;

procedure TMainForm.DeleteContactButtonClick(Sender: TObject);
begin
  FPresenter.DeleteContact;
end;

procedure TMainForm.DeletePhoneButtonClick(Sender: TObject);
begin
  FPresenter.DeletePhone;
end;

procedure TMainForm.EditContactButtonClick(Sender: TObject);
begin
  FPresenter.EditContact;
end;

procedure TMainForm.EditPhoneButtonClick(Sender: TObject);
begin
  FPresenter.EditPhone;
end;

procedure TMainForm.PhonesGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  PhoneData: TJSONObject;
begin
  if (aRow > 0) and (aRow <= FPresenter.ContactPhones.Data.Count) then
  begin
    PhoneData := FPresenter.ContactPhones.Data.Objects[aRow - 1];
    FPresenter.SelectedPhoneData := PhoneData;
  end;
end;

procedure TMainForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FPresenter.Contacts then
  begin
    case Operation of
      ooChange, ooDeleteItem, ooAddItem:
      begin
        UpdateContactListView;
        if (Operation = ooChange) and (Data <> nil) then
          UpdatePhonesLabel;
      end;
    end;
  end else if ASender = FPresenter.ContactPhones then
  begin
    case Operation of
      ooChange, ooDeleteItem, ooAddItem:
        UpdateContactPhoneListView;
    end;
  end;
end;

procedure TMainForm.SetPresenter(Value: TMainPresenter);
begin
  FPresenter := Value;
  FPresenter.Contacts.FPOAttachObserver(Self);
  FPresenter.ContactPhones.FPOAttachObserver(Self);
end;

procedure TMainForm.UpdateContactListView;
var
  i: Integer;
  ContactData: TJSONObject;
  ContactsData: TJSONArray;
  CategoryName: String;
begin
  ContactsData := FPresenter.Contacts.Data;
  ContactsGrid.RowCount := ContactsData.Count + 1;
  for i := 0 to ContactsData.Count - 1 do
  begin
    ContactData := ContactsData.Objects[i];
    ContactsGrid.Cells[0, i + 1] := ContactData.Strings['id'];
    ContactsGrid.Cells[1, i + 1] := ContactData.Get('name', '');
    CategoryName := FPresenter.GetContactCategory(ContactData);
    ContactsGrid.Cells[2, i + 1] := CategoryName;
  end;
end;

procedure TMainForm.UpdateContactPhoneListView;
var
  i: Integer;
  PhoneData: TJSONObject;
  PhonesData: TJSONArray;
begin
  PhonesData := FPresenter.ContactPhones.Data;
  PhonesGrid.RowCount := PhonesData.Count + 1;
  for i := 0 to PhonesData.Count - 1 do
  begin
    PhoneData := PhonesData.Objects[i];
    PhonesGrid.Cells[0, i + 1] := PhoneData.Strings['id'];
    PhonesGrid.Cells[1, i + 1] := PhoneData.Get('number', '');
  end;
end;

procedure TMainForm.UpdatePhonesLabel;
var
  ContactData: TJSONObject;
begin
  ContactData := FPresenter.SelectedContactData;
  if ContactData <> nil then
    PhonesLabel.Caption := Format('%s Phones', [ContactData.Get('name', '')])
  else
    PhonesLabel.Caption := 'Contact Phones';
end;

end.

