unit WizardControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WizardTypes, Controls;

type

  TWizardController = class;

  { TWizardPage }

  TWizardPage = class(TCollectionItem)
  private
    FControl: TControl;
    FControlClass: TControlClass;
    FControlClassName: String;
    FDescription: String;
    FTitle: String;
  protected
    function GetDisplayName: String; override;
  public
    property ControlClass: TControlClass read FControlClass write FControlClass;
  published
    property Title: String read FTitle write FTitle;
    property Description: String read FDescription write FDescription;
    property Control: TControl read FControl write FControl;
    property ControlClassName: String read FControlClassName write FControlClassName;
  end;

  { TWizardPages }

  TWizardPages = class(TCollection)
  private
    FOwner: TWizardController;
    function GetItem(Index: Integer): TWizardPage;
  protected
  public
    constructor Create(AOwner: TWizardController);
    property Items[Index: Integer]: TWizardPage read GetItem; default;
  end;

  TWizardShowPage = procedure(Sender: TWizardController; Page: TWizardPage) of object;

  { TWizardController }

  TWizardController = class(TComponent, IWizardController)
  private
    FOnShowPage: TWizardShowPage;
    FPageIndex: Integer;
    FPages: TWizardPages;
    procedure AddPage(const Title, Description: String; Control: TControl;
      ControlClass: TControlClass; const ControlClassName: String);
    procedure ShowPage(Index: Integer);
    procedure SetPages(const Value: TWizardPages);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //IWizardController
    function GetPageCount: Integer;
    procedure Previous;
    procedure Next;
    procedure UpdateEnabledButtons(Buttons: TWizardButtons);
    //
    procedure AddPage(const Title, Description: String; ControlClass: TControlClass);
    procedure AddPage(const Title, Description: String; Control: TControl);
    procedure AddPage(const Title, Description, ControlClassName: String);
    procedure First;
  published
    property Pages: TWizardPages read FPages write SetPages;
    //events
    property OnShowPage: TWizardShowPage read FOnShowPage write FOnShowPage;
  end;

implementation

uses
  LuiMiscUtils;

{ TWizardController }

procedure TWizardController.AddPage(const Title, Description: String;
  Control: TControl; ControlClass: TControlClass; const ControlClassName: String);
var
  NewPage: TWizardPage;
begin
  NewPage := TWizardPage(FPages.Add);
  NewPage.Title := Title;
  NewPage.Description := Description;
  NewPage.Control := Control;
  NewPage.ControlClass := ControlClass;
  NewPage.ControlClassName := ControlClassName;
end;

procedure TWizardController.ShowPage(Index: Integer);
var
  NextPage: TWizardPage;
  PageControl: TControl;
  PageControlClass: TControlClass;
  FoundClass: TPersistentClass;
  Parent: TWinControl;
  OldIndex: Integer;
begin
  if (Index < 0) or (Index >= FPages.Count) or (FPageIndex = Index) then
    Exit;
  OldIndex := FPageIndex;
  NextPage := FPages[Index];
  PageControl := NextPage.Control;
  if PageControl = nil then
  begin
    PageControlClass := NextPage.ControlClass;
    if PageControlClass = nil then
    begin
      FoundClass := FindClass(NextPage.ControlClassName);
      if FoundClass.InheritsFrom(PageControlClass) then
        PageControlClass := TControlClass(FoundClass);
    end;
    if PageControlClass <> nil then
    begin
      Parent := Owner as TWinControl;
      PageControl := PageControlClass.Create(Parent);
      CallMethod(PageControl, 'InitControl');
      PageControl.Align := alClient;
      PageControl.Parent := Parent;
      NextPage.Control := PageControl;
    end;
  end;
  if PageControl <> nil then
  begin
    if FOnShowPage <> nil then
      FOnShowPage(Self, NextPage);
    PageControl.Visible := True;
    FPageIndex := Index;
  end;
  if OldIndex <> -1 then
  begin
    PageControl := FPages[OldIndex].Control;
    if PageControl <> nil then
      PageControl.Visible := False;
  end;
end;

procedure TWizardController.SetPages(const Value: TWizardPages);
begin
  FPages.Assign(Value);
end;

constructor TWizardController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TWizardPages.Create(Self);
  FPageIndex := -1;
end;

destructor TWizardController.Destroy;
begin
  FPages.Destroy;
  inherited Destroy;
end;

function TWizardController.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TWizardController.Previous;
begin
  ShowPage(FPageIndex - 1);
end;

procedure TWizardController.Next;
begin
  ShowPage(FPageIndex + 1);
end;

procedure TWizardController.UpdateEnabledButtons(Buttons: TWizardButtons);
begin

end;

procedure TWizardController.AddPage(const Title, Description: String;
  ControlClass: TControlClass);
begin
  AddPage(Title, Description, nil, ControlClass, '');
end;

procedure TWizardController.AddPage(const Title, Description: String;
  Control: TControl);
begin
  AddPage(Title, Description, Control, nil, '');
end;

procedure TWizardController.AddPage(const Title, Description,
  ControlClassName: String);
begin
  AddPage(Title, Description, nil, nil, ControlClassName);
end;

procedure TWizardController.First;
begin
  ShowPage(0);
end;

{ TWizardPage }

function TWizardPage.GetDisplayName: String;
begin
  Result := FTitle;
end;

{ TWizardPages }

function TWizardPages.GetItem(Index: Integer): TWizardPage;
begin
  Result := TWizardPage(inherited GetItem(Index));
end;

constructor TWizardPages.Create(AOwner: TWizardController);
begin
  inherited Create(TWizardPage);
  FOwner := AOwner;
end;

end.

