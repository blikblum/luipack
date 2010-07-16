unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, WizardControls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonNext: TButton;
    BottomPanel: TPanel;
    ButtonPrevious: TButton;
    TitleLabel: TLabel;
    DescriptionLabel: TLabel;
    TopPanel: TPanel;
    WizardController: TWizardController;
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WizardControllerShowPage(Sender: TWizardController;
      Page: TWizardPage);
  private
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonNextClick(Sender: TObject);
begin
  WizardController.Next;
end;

procedure TMainForm.ButtonPreviousClick(Sender: TObject);
begin
  WizardController.Previous;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WizardController.First;
end;

procedure TMainForm.WizardControllerShowPage(Sender: TWizardController;
  Page: TWizardPage);
begin
  TitleLabel.Caption := Page.Title;
  DescriptionLabel.Caption := Page.Description;
end;

end.

