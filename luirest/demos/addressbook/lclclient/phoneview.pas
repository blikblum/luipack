unit PhoneView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, fpjson;

type

  { TPhoneViewForm }

  TPhoneViewForm = class(TForm)
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
    class function EditData(AOwner: TCustomForm; PhoneData: TJSONObject): Boolean;
  end;

var
  PhoneViewForm: TPhoneViewForm;

implementation

{$R *.lfm}

{ TPhoneViewForm }

procedure TPhoneViewForm.BitBtn1Click(Sender: TObject);
begin
  FData.Strings['number'] := NameEdit.Text;
end;

procedure TPhoneViewForm.SetData(AValue: TJSONObject);
begin
  FData := AValue;
  NameEdit.Text := AValue.Get('number', '');
end;

class function TPhoneViewForm.EditData(AOwner: TCustomForm;
  PhoneData: TJSONObject): Boolean;
begin
  with TPhoneViewForm.Create(AOwner) do
   try
     Data := PhoneData;
     Result := ShowModal = mrOK;
   finally
     Destroy;
   end;
end;

end.

