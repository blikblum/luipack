unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, WizardControls;

type

  { TMainForm }

  TMainForm = class(TForm)
    TitleLabel: TLabel;
    DescriptionLabel: TLabel;
    TopPanel: TPanel;
    WizardButtonPanel1: TWizardButtonPanel;
    WizardController: TWizardController;
    procedure FormCreate(Sender: TObject);
    procedure WizardControllerPageStateChange(Sender: TWizardController;
      Page: TWizardPage);
    procedure WizardControllerShowPage(Sender: TWizardController;
      Page: TWizardPage);
  private
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  fWizardPageFour, fWizardPageFive;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //you can add the class dinamically
  WizardController.Pages[3].ControlClass := TPageFourFrame;
  WizardController.Pages[4].ControlClass := TPageFiveFrame;
  WizardController.Start;
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

