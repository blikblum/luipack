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
    WizardButtonPanel1: TWizardButtonPanel;
    WizardController: TWizardController;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WizardControllerCreatePageControl(Sender: TWizardController;
      Page: TWizardPage);
    procedure WizardControllerPageStateChange(Sender: TWizardController;
      Page: TWizardPage);
    procedure WizardControllerShowPage(Sender: TWizardController;
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
  WizardController.Pages[3].ControlClass := TPageFourFrame;
  WizardController.Pages[4].ControlClass := TPageFiveFrame;
  WizardController.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FConfigData.Destroy;
end;

procedure TMainForm.WizardControllerCreatePageControl(
  Sender: TWizardController; Page: TWizardPage);
begin
  SetObjectProperties(Page.Control, ['ConfigData', FConfigData]);
end;

procedure TMainForm.WizardControllerPageStateChange(Sender: TWizardController;
  Page: TWizardPage);
begin
  WizardButtonPanel1.UpdateButtons(Page);
end;

procedure TMainForm.WizardControllerShowPage(Sender: TWizardController;
  Page: TWizardPage);
begin
  TitleLabel.Caption := Page.Title;
  DescriptionLabel.Caption := Page.Description;
  WizardButtonPanel1.UpdateButtons(Page);
end;

end.

