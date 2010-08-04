unit fWizardPageTwo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, WizardTypes;

type

  { TPageTwoFrame }

  TPageTwoFrame = class(TFrame, IWizardPage)
    Label1: TLabel;
    Memo1: TMemo;
    DontAcceptRadioButton: TRadioButton;
    AcceptRadioButton: TRadioButton;
    procedure AcceptRadioButtonChange(Sender: TObject);
  private
    { private declarations }
    FWizard: IWizardController;
  public
    { public declarations }
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
  published
    property WizardController: IWizardController read FWizard write FWizard;
  end; 

implementation

{$R *.lfm}

{ TPageTwoFrame }

procedure TPageTwoFrame.AcceptRadioButtonChange(Sender: TObject);
begin
  if FWizard <> nil then
    FWizard.PageStateChanged;
end;

procedure TPageTwoFrame.GetPageInfo(var PageInfo: TWizardPageInfo);
begin
  if AcceptRadioButton.Checked then
    Include(PageInfo.EnabledButtons, wbNext)
  else
    Exclude(PageInfo.EnabledButtons, wbNext);
end;


initialization
  RegisterClass(TPageTwoFrame);

end.

