unit JSONPhoneView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, fpjson;

type

  { TJSONPhoneViewForm }

  TJSONPhoneViewForm = class(TForm)
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
  JSONPhoneViewForm: TJSONPhoneViewForm;

implementation

{$R *.lfm}

{ TJSONPhoneViewForm }

procedure TJSONPhoneViewForm.BitBtn1Click(Sender: TObject);
begin
  FData.Strings['number'] := NameEdit.Text;
end;

procedure TJSONPhoneViewForm.SetData(AValue: TJSONObject);
begin
  if FData = AValue then Exit;
  FData := AValue;
  NameEdit.Text := AValue.Strings['number'];
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

