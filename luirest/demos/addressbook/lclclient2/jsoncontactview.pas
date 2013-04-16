unit JSONContactView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, fpjson, LuiRESTClient, LuiDataClasses;

type

  { TJSONContactViewForm }

  TJSONContactViewForm = class(TForm)
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    CategoryComboBox: TComboBox;
    Label1: TLabel;
    NameEdit: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FCategoriesData: TJSONArray;
    FData: TJSONObject;
    procedure SetCategoriesData(AValue: TJSONArray);
    procedure SetData(AValue: TJSONObject);
  public
    property CategoriesData: TJSONArray read FCategoriesData write SetCategoriesData;
    property Data: TJSONObject read FData write SetData;
    class function EditData(AOwner: TCustomForm; Resources: TRESTResourceClient; ContactData: TJSONObject): Boolean;
  end;

var
  JSONContactViewForm: TJSONContactViewForm;

implementation

uses
  LuiJSONUtils;

{$R *.lfm}

{ TJSONContactViewForm }

procedure TJSONContactViewForm.SaveButtonClick(Sender: TObject);
begin
  FData.Strings['name'] := NameEdit.Text;
  if CategoryComboBox.ItemIndex = -1 then
    FData.Nulls['categoryid'] := True
  else
  begin
    FData.Integers['categoryid'] := FCategoriesData.Objects[CategoryComboBox.ItemIndex].Integers['id'];
  end;
end;

procedure TJSONContactViewForm.FormShow(Sender: TObject);
begin
  CategoryComboBox.ItemIndex := GetJSONIndexOf(FCategoriesData, ['id', FData.Get('categoryid', -1)]);
end;

procedure TJSONContactViewForm.SetData(AValue: TJSONObject);
begin
  FData := AValue;
  NameEdit.Text := AValue.Strings['name'];
end;

procedure TJSONContactViewForm.SetCategoriesData(AValue: TJSONArray);
var
  i: Integer;
begin
  FCategoriesData := AValue;
  CategoryComboBox.Items.BeginUpdate;
  for i := 0 to FCategoriesData.Count - 1 do
    CategoryComboBox.AddItem(FCategoriesData.Objects[i].Strings['name'], nil);
  CategoryComboBox.Items.EndUpdate;
end;

class function TJSONContactViewForm.EditData(AOwner: TCustomForm; Resources: TRESTResourceClient;
  ContactData: TJSONObject): Boolean;
var
  Categories: IJSONArrayResource;
begin
  with TJSONContactViewForm.Create(AOwner) do
   try
     Data := ContactData;
     Categories := Resources.GetJSONArray('category');
     Categories.Fetch;
     CategoriesData := Categories.Data;
     Result := ShowModal = mrOK;
   finally
     Destroy;
   end;
end;

end.

