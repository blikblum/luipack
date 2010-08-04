unit WizardControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WizardTypes, Controls, ExtCtrls, Buttons;

const
  WizardDefaultButtons = [wbPrevious, wbNext, wbCancel];

type

  TWizardController = class;

  TWizardButtonPanel = class;

  { TWizardPage }

  TWizardPage = class(TCollectionItem)
  private
    FControl: TControl;
    FControlClass: TControlClass;
    FControlClassName: String;
    FDescription: String;
    FEnabledButtons: TWizardButtons;
    FNextOffset: Cardinal;
    FPageIntf: IWizardPage;
    FPreviousOffset: Cardinal;
    FTitle: String;
    FVisibleButtons: TWizardButtons;
    procedure SetControl(Value: TControl);
    procedure UpdatePageInfo;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    property ControlClass: TControlClass read FControlClass write FControlClass;
  published
    property Control: TControl read FControl write SetControl;
    property ControlClassName: String read FControlClassName write FControlClassName;
    property Description: String read FDescription write FDescription;
    property EnabledButtons: TWizardButtons read FEnabledButtons write FEnabledButtons default WizardDefaultButtons;
    property NextOffset: Cardinal read FNextOffset write FNextOffset default 1;
    property PreviousOffset: Cardinal read FPreviousOffset write FPreviousOffset default 1;
    property Title: String read FTitle write FTitle;
    property VisibleButtons: TWizardButtons read FVisibleButtons write FVisibleButtons default WizardDefaultButtons;
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

  TWizardPageEvent = procedure(Sender: TWizardController; Page: TWizardPage) of object;

  { TWizardController }

  TWizardController = class(TComponent, IWizardController)
  private
    FOnCreatePageControl: TWizardPageEvent;
    FOnPageStateChange: TWizardPageEvent;
    FOnShowPage: TWizardPageEvent;
    FPageIndex: Integer;
    FPages: TWizardPages;
    procedure AddPage(const Title, Description: String; Control: TControl;
      ControlClass: TControlClass; const ControlClassName: String);
    function ShowPage(Index: Integer): Boolean;
    procedure SetPages(const Value: TWizardPages);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //IWizardController
    function GetPageCount: Integer;
    function MoveBy(Offset: Integer): Boolean;
    procedure PageStateChanged;
    //
    procedure AddPage(const Title, Description: String; ControlClass: TControlClass);
    procedure AddPage(const Title, Description: String; Control: TControl);
    procedure AddPage(const Title, Description, ControlClassName: String);
    procedure DoAction(Action: TWizardAction);
    procedure Start;
  published
    property Pages: TWizardPages read FPages write SetPages;
    //events
    property OnCreatePageControl: TWizardPageEvent read FOnCreatePageControl write FOnCreatePageControl;
    property OnPageStateChange: TWizardPageEvent read FOnPageStateChange write FOnPageStateChange;
    property OnShowPage: TWizardPageEvent read FOnShowPage write FOnShowPage;
  end;

  { TWizardPanelBitBtn }

  TWizardPanelBitBtn = class(TCustomBitBtn)
  private
    FButtonType: TWizardButton;
  public
    constructor Create(AOwner: TComponent); override;
    property ButtonType: TWizardButton read FButtonType;
  published
    property Caption stored True;
    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property Enabled;
    property Font;
    property Glyph;
    property Name stored True;
    property ShowHint;
    property OnClick;
  end;

  { TWizardButtonPanel }

  TWizardButtonPanel = class(TCustomPanel)
  private
    FBevel: TBevel;
    FCancelButton: TWizardPanelBitBtn;
    FController: TWizardController;
    FFinishButton: TWizardPanelBitBtn;
    FNextButton: TWizardPanelBitBtn;
    FPreviousButton: TWizardPanelBitBtn;
    FShowBevel: Boolean;
    procedure ButtonClick(Sender: TObject);
    function CreateButton(ButtonType: TWizardButton): TWizardPanelBitBtn;
    procedure SetController(const Value: TWizardController);
    procedure SetShowBevel(const Value: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateButtons(Page: TWizardPage);
  protected
    procedure DoOnResize; override;
  published
    property Controller: TWizardController read FController write SetController;
    property CancelButton: TWizardPanelBitBtn read FCancelButton;
    property FinishButton: TWizardPanelBitBtn read FFinishButton;
    property NextButton: TWizardPanelBitBtn read FNextButton;
    property PreviousButton: TWizardPanelBitBtn read FPreviousButton;
    property ShowBevel: Boolean read FShowBevel write SetShowBevel default false;
    //
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter default bvNone;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
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
  LuiMiscUtils, LuiRTTIUtils;

const
  WizardButtonNames: array[TWizardButton] of String = (
    'Previous',
    'Next',
    'Finish',
    'Cancel',
    'Help'
  );

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

function TWizardController.ShowPage(Index: Integer): Boolean;
var
  NextPage: TWizardPage;
  PageControl: TControl;
  PageControlClass: TControlClass;
  FoundClass: TPersistentClass;
  Parent: TWinControl;
  OldIndex: Integer;
begin
  Result := False;
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
      PageControl.Align := alClient;
      PageControl.Parent := Parent;
      //setting control register the wizard in the page
      NextPage.Control := PageControl;
      if Assigned(FOnCreatePageControl) then
        FOnCreatePageControl(Self, NextPage);
      //InitControl should be called after setting Control property
      CallMethod(PageControl, 'InitControl');
    end;
  end;
  if PageControl <> nil then
  begin
    if FOnShowPage <> nil then
      FOnShowPage(Self, NextPage);
    CallMethod(PageControl, 'UpdateControl');
    PageControl.Visible := True;
    FPageIndex := Index;
  end;
  if OldIndex <> -1 then
  begin
    PageControl := FPages[OldIndex].Control;
    if PageControl <> nil then
      PageControl.Visible := False;
  end;
  Result := True;
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

function TWizardController.MoveBy(Offset: Integer): Boolean;
begin
  //ShowPage takes care of index bounds
  Result := ShowPage(FPageIndex + Offset);
end;

procedure TWizardController.PageStateChanged;
var
  ActivePage: TWizardPage;
begin
  if FPageIndex <> -1 then
  begin
    ActivePage := FPages[FPageIndex];
    ActivePage.UpdatePageInfo;
    if Assigned(FOnPageStateChange) then
      FOnPageStateChange(Self, ActivePage);
  end;
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

procedure TWizardController.DoAction(Action: TWizardAction);
var
  ActivePage: TWizardPage;
  Offset: Integer;
begin
  if FPageIndex <> -1 then
  begin
    ActivePage := FPages[FPageIndex];
    case Action of
      waNext:
        begin
          Offset := ActivePage.NextOffset;
          if MoveBy(Offset) then
          begin
            //set the PreviousOffset of the new activepage
            ActivePage := FPages[FPageIndex];
            ActivePage.PreviousOffset := Offset;
          end;
        end;
      waPrevious: MoveBy(-ActivePage.PreviousOffset);
    end;
  end;
end;

procedure TWizardController.Start;
begin
  ShowPage(0);
end;

{ TWizardPage }

procedure TWizardPage.SetControl(Value: TControl);
begin
  if FControl = Value then
    Exit;
  FControl := Value;
  if (Value <> nil) and FControl.GetInterface(WizardPageIntfID, FPageIntf) then
  begin
    SetObjectProperties(Value, ['WizardController', ((Collection as TWizardPages).FOwner) as IWizardController]);
    UpdatePageInfo;
  end;
end;

procedure TWizardPage.UpdatePageInfo;
var
  PageInfo: TWizardPageInfo;
begin
  if FPageIntf = nil then
    Exit;
  PageInfo.Title := Title;
  PageInfo.Description := Description;
  PageInfo.EnabledButtons := EnabledButtons;
  PageInfo.VisibleButtons := VisibleButtons;
  PageInfo.NextOffset := NextOffset;
  PageInfo.PreviousOffset := PreviousOffset;

  FPageIntf.GetPageInfo(PageInfo);

  Title := PageInfo.Title;
  Description := PageInfo.Description;
  EnabledButtons := PageInfo.EnabledButtons;
  VisibleButtons := PageInfo.VisibleButtons;
  PreviousOffset := PageInfo.PreviousOffset;
  NextOffset := PageInfo.NextOffset;
end;

function TWizardPage.GetDisplayName: String;
begin
  Result := FTitle;
end;

constructor TWizardPage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVisibleButtons := WizardDefaultButtons;
  FEnabledButtons := WizardDefaultButtons;
  FNextOffset := 1;
  FPreviousOffset := 1;
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

procedure TWizardButtonPanel.ButtonClick(Sender: TObject);
begin
  if FController = nil then
    Exit;
  with Sender as TWizardPanelBitBtn do
  begin
    case ButtonType of
      wbNext: FController.DoAction(waNext);
      wbPrevious: FController.DoAction(waPrevious);
    end;
  end;
end;

function TWizardButtonPanel.CreateButton(ButtonType: TWizardButton): TWizardPanelBitBtn;
begin
  Result := TWizardPanelBitBtn.Create(Self);
  Result.Parent := Self;
  Result.Anchors := [akRight, akTop];
  Result.FButtonType := ButtonType;
  Result.Caption := WizardButtonNames[ButtonType];
  Result.Name := WizardButtonNames[ButtonType] + 'Button';
  Result.OnClick := @ButtonClick;
end;

procedure TWizardButtonPanel.SetController(const Value: TWizardController);
begin
  if Value = FController then
    Exit;
  FController := Value;
end;

procedure TWizardButtonPanel.SetShowBevel(const Value: Boolean);
begin
  if FShowBevel = Value then exit;
  FShowBevel := Value;
  if Value then
  begin
    if FBevel = nil then
      FBevel := TBevel.Create(Self);
    FBevel.Parent := Self;
    FBevel.Height := 2;
    FBevel.Shape := bsTopLine;
    FBevel.Align := alTop;
    FBevel.Visible := True;
  end
  else
  begin
    if FBevel <> nil then
      FBevel.Visible := False;
  end;
end;

constructor TWizardButtonPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
  //create the buttons
  FCancelButton := CreateButton(wbCancel);
  FCancelButton.AnchorParallel(akRight, 4, Self);
  FCancelButton.AnchorVerticalCenterTo(Self);

  FFinishButton := CreateButton(wbFinish);
  FFinishButton.AnchorVerticalCenterTo(Self);


  FNextButton := CreateButton(wbNext);
  FNextButton.AnchorVerticalCenterTo(Self);

  FPreviousButton := CreateButton(wbPrevious);
  FPreviousButton.AnchorVerticalCenterTo(Self);
end;

destructor TWizardButtonPanel.Destroy;
begin
  FNextButton.Destroy;
  FPreviousButton.Destroy;
  FCancelButton.Destroy;
  FFinishButton.Destroy;
  inherited Destroy;
end;

procedure TWizardButtonPanel.UpdateButtons(Page: TWizardPage);
var
  VisibleButtons, EnabledButtons: TWizardButtons;
begin
  VisibleButtons := Page.VisibleButtons;
  EnabledButtons := Page.EnabledButtons;

  FCancelButton.Visible := wbCancel in VisibleButtons;
  FFinishButton.Visible := wbFinish in VisibleButtons;
  FNextButton.Visible := wbNext in VisibleButtons;
  FPreviousButton.Visible := wbPrevious in VisibleButtons;

  FCancelButton.Enabled := wbCancel in EnabledButtons;
  FFinishButton.Enabled := wbFinish in EnabledButtons;
  FNextButton.Enabled := wbNext in EnabledButtons;
  FPreviousButton.Enabled := wbPrevious in EnabledButtons;
end;

procedure TWizardButtonPanel.DoOnResize;
begin
  inherited DoOnResize;
  FFinishButton.Left := FCancelButton.Left - FFinishButton.Width - 8;
  FNextButton.Left := FFinishButton.Left - FNextButton.Width - 8;
  FPreviousButton.Left := FNextButton.Left - FPreviousButton.Width - 2;
end;

{ TWizardPanelBitBtn }

constructor TWizardPanelBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
end;

end.

