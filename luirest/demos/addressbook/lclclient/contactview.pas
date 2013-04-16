unit ContactView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, fpjson;

type

  { TContactViewForm }

  TContactViewForm = class(TForm)
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
  ContactViewForm: TContactViewForm;

implementation

{$R *.lfm}

{ TContactViewForm }

procedure TContactViewForm.BitBtn1Click(Sender: TObject);
begin
  FData.Strings['name'] := NameEdit.Text;
end;

procedure TContactViewForm.SetData(AValue: TJSONObject);
begin
  FData := AValue;
  NameEdit.Text := AValue.Get('name', '');
end;

class function TContactViewForm.EditData(AOwner: TCustomForm;
  ContactData: TJSONObject): Boolean;
begin
  with TContactViewForm.Create(AOwner) do
   try
     Data := ContactData;
     Result := ShowModal = mrOK;
   finally
     Destroy;
   end;
end;

end.

