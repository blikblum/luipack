unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, WizardControls, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    TitleLabel: TLabel;
    DescriptionLabel: TLabel;
    TopPanel: TPanel;
    WizardButtons: TWizardButtonPanel;
    WizardManager: TWizardManager;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WizardButtonsButtonClick(Sender: TObject; ButtonType: TWizardButton);
    procedure WizardManagerPageLoad(Sender: TObject;
      Page: TWizardPage);
    procedure WizardManagerPageShow(Sender: TObject;
      Page: TWizardPage);
  private
    FConfigData: TJSONObject;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  fWizardPageFour, fWizardPageFive, LuiRTTIUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //data that will be shared between pages
  FConfigData := TJSONObject.Create(['name', 'Luiz Américo']);
  //you can add the class dinamically
  WizardManager.PageByName('config2').ControlClass := TPageFourFrame;
  WizardManager.PageByName('finish').ControlClass := TPageFiveFrame;
  WizardManager.PageIndex := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FConfigData.Destroy;
end;

procedure TMainForm.WizardButtonsButtonClick(Sender: TObject; ButtonType: TWizardButton);
begin
  case ButtonType of
    wbCancel: Close;
    wbFinish: ShowMessage(FConfigData.AsJSON);
  end;
end;

procedure TMainForm.WizardManagerPageLoad(Sender: TObject; Page: TWizardPage);
begin
  SetObjectProperties(Page.Control, ['ConfigData', FConfigData]);
end;

procedure TMainForm.WizardManagerPageShow(Sender: TObject; Page: TWizardPage);
begin
  TitleLabel.Caption := Page.Caption;
  DescriptionLabel.Caption := Page.Description;
end;

end.

