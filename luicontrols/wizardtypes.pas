unit WizardTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WizardPageIntfID = 'lui_wizardpage';
  WizardControllerIntfID = 'lui_wizardcontroller';

type
  TWizardButton = (wbBack, wbNext, wbFinish, wbCancel, wbHelp);

  TWizardButtons = set of TWizardButton;

  TWizardPageInfo = record
    Title: String;
    Description: String;
    VisibleButtons: TWizardButtons;
    EnabledButtons: TWizardButtons;
  end;

  {$INTERFACES CORBA}

  { IWizardController }

  IWizardController = interface
    [WizardControllerIntfID]
    function GetPageCount: Integer;
    procedure Previous;
    procedure Next;
    procedure UpdateButton(Button: TWizardButton; Visible, Enabled: Boolean);
  end;

  { IWizardPage }

  IWizardPage = interface
    [WizardPageIntfID]
    procedure GetPageInfo(out PageInfo: TWizardPageInfo);
    procedure RegisterController(Controller: IWizardController);
  end;

  { IWizardObserver }

  IWizardObserver = interface
    procedure EnabledButtonsChanged(Buttons: TWizardButtons);
    procedure PageChanged(PageIndex: Integer; Page: IWizardPage);
    procedure RegisterController(Controller: IWizardController);
  end;

const
  AllWizardButtons = [wbBack, wbNext, wbFinish, wbCancel, wbHelp];

implementation

end.

