unit JSONPhoneView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, fpjson;

type

  { TJSONPhoneViewForm }

  TJSONPhoneViewForm = class(TForm)
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    NameEdit: TLabeledEdit;
    procedure SaveButtonClick(Sender: TObject);
  private
    FData: TJSONObject;
    procedure SetData(AValue: TJSONObject);
    { private declarations }
  public
    { public declarations }
    property Data: TJSONObject read FData write SetData;
    class function EditData(AOwner: TCustomForm; PhoneData: TJSONObject): Boolean;
  end;

var
  JSONPhoneViewForm: TJSONPhoneViewForm;

implementation

{$R *.lfm}

{ TJSONPhoneViewForm }

procedure TJSONPhoneViewForm.SaveButtonClick(Sender: TObject);
begin
  FData.Strings['number'] := NameEdit.Text;
end;

procedure TJSONPhoneViewForm.SetData(AValue: TJSONObject);
begin
  FData := AValue;
  NameEdit.Text := AValue.Get('number', '');
end;

class function TJSONPhoneViewForm.EditData(AOwner: TCustomForm;
  PhoneData: TJSONObject): Boolean;
begin
  with TJSONPhoneViewForm.Create(AOwner) do
   try
     Data := PhoneData;
     Result := ShowModal = mrOK;
   finally
     Destroy;
   end;
end;

end.

