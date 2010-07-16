unit WizardTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TWizardButton = (wbBack, wbNext, wbFinish, wbCancel, wbHelp);

  TWizardButtons = set of TWizardButton;

  TWizardPageInfo = record
    Title: String;
    LongTitle: String;
    Description: String;
  end;

  {$INTERFACES CORBA}

  { IWizardController }

  IWizardController = interface
    function GetPageCount: Integer;
    procedure Previous;
    procedure Next;
    procedure UpdateEnabledButtons(Buttons: TWizardButtons);
  end;

  { IWizardPage }

  IWizardPage = interface
    procedure GetPageInfo(out PageInfo: TWizardPageInfo);
    procedure GetButtons(out VisibleButtons, EnabledButtons: TWizardButtons);
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

