unit AppConfigView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  CAMICUApp;

type

  { TAppConfigViewForm }

  TAppConfigViewForm = class(TForm)
    BaseURLEdit: TLabeledEdit;
    CloseButton: TBitBtn;
    SaveButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FConfig: TCAMICUAppConfig;
  published
    property Config: TCAMICUAppConfig read FConfig write FConfig;
  end;

implementation

{$R *.lfm}

{ TAppConfigViewForm }

procedure TAppConfigViewForm.FormShow(Sender: TObject);
begin
  BaseURLEdit.Text := Config.BaseURL;
end;

procedure TAppConfigViewForm.SaveButtonClick(Sender: TObject);
begin
  Config.BaseURL := BaseURLEdit.Text;
  Config.Save;
end;

end.

