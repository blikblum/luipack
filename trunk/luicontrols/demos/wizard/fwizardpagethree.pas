unit fWizardPageThree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, WizardTypes;

type

  { TPageThreeFrame }

  TPageThreeFrame = class(TFrame, IWizardPage)
    DefaultRadioButton: TRadioButton;
    CustomRadioButton: TRadioButton;
    procedure DefaultRadioButtonChange(Sender: TObject);
  private
    { private declarations }
    FWizard: IWizardController;
  public
    { public declarations }
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
    procedure RegisterController(Controller: IWizardController);
  end; 

implementation

{$R *.lfm}

{ TPageThreeFrame }

procedure TPageThreeFrame.DefaultRadioButtonChange(Sender: TObject);
begin
  if FWizard <> nil then
    FWizard.PageStateChanged;
end;

procedure TPageThreeFrame.GetPageInfo(var PageInfo: TWizardPageInfo);
begin
  if DefaultRadioButton.Checked then
    PageInfo.NextOffset := 2
  else
    PageInfo.NextOffset := 1;
end;

procedure TPageThreeFrame.RegisterController(Controller: IWizardController);
begin
  FWizard := Controller;
end;

initialization
  RegisterClass(TPageThreeFrame);

end.

