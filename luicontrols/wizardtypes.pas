unit WizardTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WizardPageIntfID = 'lui_wizardpage';
  WizardControllerIntfID = 'lui_wizardcontroller';

type
  TWizardButton = (wbPrevious, wbNext, wbFinish, wbCancel, wbHelp);

  TWizardAction = (waPrevious, waNext, waFinish, waCancel, waHelp, waCustom);

  TWizardDirection = (wdNext, wdPrevious);

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
    procedure MoveBy(Offset: Integer);
    procedure PageStateChanged;
  end;

  { IWizardPage }

  IWizardPage = interface
    [WizardPageIntfID]
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
    procedure RegisterController(Controller: IWizardController);
  end;

  { IWizardObserver }

  IWizardObserver = interface
    procedure EnabledButtonsChanged(Buttons: TWizardButtons);
    procedure PageChanged(PageIndex: Integer; Page: IWizardPage);
    procedure RegisterController(Controller: IWizardController);
  end;

const
  AllWizardButtons = [wbPrevious, wbNext, wbFinish, wbCancel, wbHelp];

implementation

end.

