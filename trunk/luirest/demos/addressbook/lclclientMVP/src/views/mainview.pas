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
    procedure ContactsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
  private
    FPresenter: TMainPresenter;
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure SetPresenter(Value: TMainPresenter);
    procedure UpdateContactListView;
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
end;

procedure TMainForm.DeleteContactButtonClick(Sender: TObject);
begin
  FPresenter.DeleteContact;
end;

procedure TMainForm.EditContactButtonClick(Sender: TObject);
begin
  FPresenter.EditContact;
end;

procedure TMainForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FPresenter.Contacts then
  begin
    case Operation of
      ooChange, ooDeleteItem, ooAddItem:
        UpdateContactListView;
    end;
  end;
end;

procedure TMainForm.SetPresenter(Value: TMainPresenter);
begin
  FPresenter := Value;
  FPresenter.Contacts.FPOAttachObserver(Self);
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

end.

