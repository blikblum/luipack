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
    NextOffset: Cardinal;
    PreviousOffset: Cardinal;
  end;

  {$INTERFACES CORBA}

  { IWizardController }

  IWizardController = interface
    [WizardControllerIntfID]
    function GetPageCount: Integer;
    function MoveBy(Offset: Integer): Boolean;
    procedure PageStateChanged;
  end;

  { IWizardPage }

  IWizardPage = interface
    [WizardPageIntfID]
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
    //function HandleActionRequest(AnAction: TWizardAction; var Data: PtrInt): Boolean;
  end;

const
  AllWizardButtons = [wbPrevious, wbNext, wbFinish, wbCancel, wbHelp];

implementation

end.

