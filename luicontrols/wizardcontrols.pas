unit WizardControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WizardTypes, Controls, ExtCtrls, Buttons;

type

  TWizardController = class;

  { TWizardPage }

  TWizardPage = class(TCollectionItem)
  private
    FControl: TControl;
    FControlClass: TControlClass;
    FControlClassName: String;
    FDescription: String;
    FEnabledButtons: TWizardButtons;
    FTitle: String;
    FVisibleButtons: TWizardButtons;
    procedure SetControl(Value: TControl);
  protected
    function GetDisplayName: String; override;
  public
    property ControlClass: TControlClass read FControlClass write FControlClass;
  published
    property Control: TControl read FControl write SetControl;
    property ControlClassName: String read FControlClassName write FControlClassName;
    property Description: String read FDescription write FDescription;
    property EnabledButtons: TWizardButtons read FEnabledButtons write FEnabledButtons;
    property Title: String read FTitle write FTitle;
    property VisibleButtons: TWizardButtons read FVisibleButtons write FVisibleButtons;
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
    procedure UpdateButton(Button: TWizardButton; Visible, Enabled: Boolean);
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

  { TWizardButtonPanel }

  TWizardButtonPanel = class(TCustomPanel)
  private
    FCancelButton: TBitBtn;
    FFinishButton: TBitBtn;
    FNextButton: TBitBtn;
    FPreviousButton: TBitBtn;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CancelButton: TBitBtn read FCancelButton;
    property FinishButton: TBitBtn read FFinishButton;
    property NextButton: TBitBtn read FNextButton;
    property PreviousButton: TBitBtn read FPreviousButton;
    //
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  LuiMiscUtils{, LuiRTTIUtils};

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
      //seems that currently fpc does not allow to set a corba interface through RTTI
      //SetObjectProperties(PageControl, ['WizardController', IWizardController(Self)]);
      PageControl.Align := alClient;
      PageControl.Parent := Parent;
      //setting control register the wizard in the page
      NextPage.Control := PageControl;
      //InitControl should be called after setting Control property
      CallMethod(PageControl, 'InitControl');
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

procedure TWizardController.UpdateButton(Button: TWizardButton; Visible,
  Enabled: Boolean);
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

procedure TWizardPage.SetControl(Value: TControl);
var
  WizardPage: IWizardPage;
  PageInfo: TWizardPageInfo;
begin
  if FControl = Value then
    Exit;
  FControl := Value;
  if (FControl<> nil) and FControl.GetInterface(WizardPageIntfID, WizardPage) then
  begin
    WizardPage.RegisterController(((Collection as TWizardPages).FOwner) as IWizardController);
    WizardPage.GetPageInfo(PageInfo);
    Title := PageInfo.Title;
    Description := PageInfo.Description;
    EnabledButtons := PageInfo.EnabledButtons;
    VisibleButtons := PageInfo.VisibleButtons;
  end;
end;

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

{ TWizardButtonPanel }

constructor TWizardButtonPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCancelButton := TBitBtn.Create(Self);
  FCancelButton.Parent := Self;
  FCancelButton.Caption := 'Cancel';
  FCancelButton.Anchors := [akRight, akTop];
  FCancelButton.AnchorParallel(akRight, 4, Self);
  FCancelButton.AnchorVerticalCenterTo(Self);
  FCancelButton.BorderSpacing.Around := 4;

  FFinishButton := TBitBtn.Create(Self);
  FFinishButton.Parent := Self;
  FFinishButton.Caption := 'Finish';
  FFinishButton.Anchors := [akRight, akTop];
  FFinishButton.AnchorToNeighbour(akRight, 4, FCancelButton);
  FFinishButton.AnchorVerticalCenterTo(Self);
  FFinishButton.BorderSpacing.Around := 4;


  FNextButton := TBitBtn.Create(Self);
  FNextButton.Parent := Self;
  FNextButton.Caption := 'Next';
  FNextButton.Anchors := [akRight, akTop];
  FNextButton.AnchorToNeighbour(akRight, 4, FFinishButton);
  FNextButton.AnchorVerticalCenterTo(Self);
  FNextButton.BorderSpacing.Around := 4;


  FPreviousButton := TBitBtn.Create(Self);
  FPreviousButton.Parent := Self;
  FPreviousButton.Caption := 'Previous';
  FPreviousButton.Anchors := [akRight, akTop];
  FPreviousButton.AnchorToNeighbour(akRight, 4, FNextButton);
  FPreviousButton.AnchorVerticalCenterTo(Self);
  FPreviousButton.BorderSpacing.Around := 4;

end;

destructor TWizardButtonPanel.Destroy;
begin
  FNextButton.Destroy;
  FPreviousButton.Destroy;
  FCancelButton.Destroy;
  FFinishButton.Destroy;
  inherited Destroy;
end;

end.

