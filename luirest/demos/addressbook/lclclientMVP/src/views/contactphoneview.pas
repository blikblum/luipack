unit ContactPhoneView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ContactPhoneModel;

type

  { TContactPhoneForm }

  TContactPhoneForm = class(TForm)
    CancelButton: TBitBtn;
    NumberEdit: TLabeledEdit;
    SaveButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FPhone: TContactPhone;
    procedure SetPhone(Value: TContactPhone);
    { private declarations }
  public
    { public declarations }
  published
    //a presenter is overkill here...
    property Phone: TContactPhone read FPhone write SetPhone;
  end;

var
  ContactPhoneForm: TContactPhoneForm;

implementation

uses
  strutils;

{$R *.lfm}

{ TContactPhoneForm }

procedure TContactPhoneForm.FormShow(Sender: TObject);
begin
  //...a mediator is also overkill
  NumberEdit.Text := FPhone.Data.Get('number', '');
end;

procedure TContactPhoneForm.SaveButtonClick(Sender: TObject);
begin
  FPhone.Data.Strings['number'] := NumberEdit.Text;
  FPhone.Save;
end;

procedure TContactPhoneForm.SetPhone(Value: TContactPhone);
begin
  FPhone := Value;
  Caption := IfThen(FPhone.Data.Find('id') = nil, 'Add', 'Edit') + ' Phone';
end;

end.

