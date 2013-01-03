unit JSONContactView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, fpjson;

type

  { TJSONContactViewForm }

  TJSONContactViewForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    NameEdit: TLabeledEdit;
    procedure BitBtn1Click(Sender: TObject);
  private
    FData: TJSONObject;
    procedure SetData(AValue: TJSONObject);
    { private declarations }
  public
    { public declarations }
    property Data: TJSONObject read FData write SetData;
    class function EditData(AOwner: TCustomForm; ContactData: TJSONObject): Boolean;
  end;

var
  JSONContactViewForm: TJSONContactViewForm;

implementation

{$R *.lfm}

{ TJSONContactViewForm }

procedure TJSONContactViewForm.BitBtn1Click(Sender: TObject);
begin
  FData.Strings['name'] := NameEdit.Text;
end;

procedure TJSONContactViewForm.SetData(AValue: TJSONObject);
begin
  FData := AValue;
  NameEdit.Text := AValue.Strings['name'];
end;

class function TJSONContactViewForm.EditData(AOwner: TCustomForm;
  ContactData: TJSONObject): Boolean;
begin
  with TJSONContactViewForm.Create(AOwner) do
   try
     Data := ContactData;
     Result := ShowModal = mrOK;
   finally
     Destroy;
   end;
end;

end.

