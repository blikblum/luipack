unit VirtualPages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Controls, VarRecUtils;

type

  TVirtualPage = class;

  TLoadControlEvent = procedure(Sender: TObject; Page: TVirtualPage) of object;

  { TControlDisplayOptions }

  TControlDisplayOptions = class(TPersistent)
  private
    FBorderSpacing: TControlBorderSpacing;
    FParent: TWinControl;
    FHeight: Integer;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FAlign: TAlign;
    procedure SetBorderSpacing(AValue: TControlBorderSpacing);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure SetControlBounds(Control: TControl);
  published
    property Align: TAlign read FAlign write FAlign default alClient;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property Height: Integer read FHeight write FHeight default -1;
    property Left: Integer read FLeft write FLeft default -1;
    property Parent: TWinControl read FParent write FParent;
    property Top: Integer read FTop write FTop default -1;
    property Width: Integer read FWidth write FWidth default -1;
  end;

  { TVirtualPage }

  TVirtualPage = class(TCollectionItem)
  private
    FCaption: String;
    FControl: TControl;
    FControlClass: TControlClass;
    FControlClassName: String;
    FProperties: TConstArray;
    procedure SetControl(Value: TControl);
  protected
    function GetDisplayName: String; override;
  public
    destructor Destroy; override;
    procedure SetProperties(Elements: array of const);
    property ControlClass: TControlClass read FControlClass write FControlClass;
    property Properties: TConstArray read FProperties;
  published
    property Caption: String read FCaption write FCaption;
    property Control: TControl read FControl write SetControl;
    property ControlClassName: String read FControlClassName write FControlClassName;
  end;

  { TVirtualPages }

  TVirtualPages = class(TCollection)
  private
    FDisplayOptions: TControlDisplayOptions;
    FOnLoadControl: TLoadControlEvent;
    FPageIndex: Integer;
    function GetItems(Index: Integer): TVirtualPage;
    procedure SetDisplayOptions(AValue: TControlDisplayOptions);
    procedure SetPageIndex(NewPageIndex: Integer);
  protected
    procedure DoLoadControl(Page: TVirtualPage); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Caption: String; Control: TControl);
    procedure Add(const Caption: String; ControlClass: TControlClass; const Properties: array of const);
    procedure Add(const Caption, ControlClassName: String; const Properties: array of const);
    property Items[Index: Integer]: TVirtualPage read GetItems; default;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  published
    property DisplayOptions: TControlDisplayOptions read FDisplayOptions write SetDisplayOptions;
    property OnLoadControl: TLoadControlEvent read FOnLoadControl write FOnLoadControl;
  end;

  { TVirtualPageManager }

  TVirtualPageManager = class(TComponent)
  private
    FPages: TVirtualPages;
    procedure SetPages(AValue: TVirtualPages);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Pages: TVirtualPages read FPages write SetPages;
  end;

implementation

uses
  RtlConsts, LuiRTTIUtils, LuiMiscUtils, Math;

type

  { TManagedPages }

  TManagedPages = class(TVirtualPages)
  private
    FManager: TVirtualPageManager;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AManager: TVirtualPageManager);
  end;

{ TManagedPages }

function TManagedPages.GetOwner: TPersistent;
begin
  Result := FManager;
end;

constructor TManagedPages.Create(AManager: TVirtualPageManager);
begin
  inherited Create;
  FManager := AManager;
end;

{ TVirtualPageManager }

procedure TVirtualPageManager.SetPages(AValue: TVirtualPages);
begin
  FPages.Assign(AValue);
end;

constructor TVirtualPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TManagedPages.Create(Self);
end;

destructor TVirtualPageManager.Destroy;
begin
  FPages.Destroy;
  inherited Destroy;
end;

{ TControlDisplayOptions }

procedure TControlDisplayOptions.SetBorderSpacing(AValue: TControlBorderSpacing);
begin
  FBorderSpacing.Assign(AValue);
end;

constructor TControlDisplayOptions.Create;
begin
  FBorderSpacing := TControlBorderSpacing.Create(nil);
  FAlign := alClient;
  FLeft := -1;
  FTop := -1;
  FWidth := -1;
  FHeight := -1;
end;

procedure TControlDisplayOptions.Assign(Source: TPersistent);
begin
  if Source is TControlDisplayOptions then
  begin
    FBorderSpacing.Assign(TControlDisplayOptions(Source).BorderSpacing);
    FAlign := TControlDisplayOptions(Source).FAlign;
    FHeight := TControlDisplayOptions(Source).FHeight;
    FLeft := TControlDisplayOptions(Source).FLeft;
    FTop := TControlDisplayOptions(Source).FTop;
    FWidth := TControlDisplayOptions(Source).FWidth;
  end
  else
    inherited Assign(Source);
end;

procedure TControlDisplayOptions.SetControlBounds(Control: TControl);
var
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
begin
  NewLeft := IfThen(FLeft >= 0, FLeft, Control.Left);
  NewTop := IfThen(FTop >= 0, FTop, Control.Top);
  NewHeight := IfThen(FHeight >= 0, FHeight, Control.Height);
  NewWidth := IfThen(FWidth >= 0, FWidth, Control.Width);
  Control.SetBounds(NewLeft, NewTop, NewWidth, NewHeight);
end;

{ TVirtualPage }

procedure TVirtualPage.SetControl(Value: TControl);
begin
  FControl := Value;
end;

function TVirtualPage.GetDisplayName: String;
begin
  Result := FCaption;
end;

destructor TVirtualPage.Destroy;
begin
  FinalizeConstArray(FProperties);
  inherited Destroy;
end;

procedure TVirtualPage.SetProperties(Elements: array of const);
begin
  FinalizeConstArray(FProperties);
  FProperties := CreateConstArray(Elements);
end;

{ TVirtualPages }

function TVirtualPages.GetItems(Index: Integer): TVirtualPage;
begin
  Result := TVirtualPage(GetItem(Index));
end;

procedure TVirtualPages.SetDisplayOptions(AValue: TControlDisplayOptions);
begin
  FDisplayOptions.Assign(AValue);
end;

procedure TVirtualPages.SetPageIndex(NewPageIndex: Integer);
var
  NewPage: TVirtualPage;
  PageControl: TControl;
  PageControlClass: TControlClass;
  FoundClass: TPersistentClass;
begin
  if FPageIndex = NewPageIndex then Exit;
  if NewPageIndex >= Count then
    raise Exception.CreateFmt(SListIndexError, [NewPageIndex]);
  if (NewPageIndex > -1) then
  begin
    NewPage := Items[NewPageIndex];
    PageControl := NewPage.Control;
    if (PageControl = nil) then
    begin
      PageControlClass := NewPage.ControlClass;
      if PageControlClass = nil then
      begin
        FoundClass := FindClass(NewPage.ControlClassName);
        if FoundClass.InheritsFrom(TControl) then
          PageControlClass := TControlClass(FoundClass);
      end;
      if (PageControlClass <> nil) and (FDisplayOptions.Parent <> nil) then
      begin
        //todo: use Fowner as Owner?
        PageControl := PageControlClass.Create(FDisplayOptions.Parent);
        PageControl.Name := 'VirtualPage' + IntToStr(NewPageIndex) + PageControlClass.ClassName;
        PageControl.Visible := False;
        //todo: see how avoid unecessary resizes
        FDisplayOptions.SetControlBounds(PageControl);
        PageControl.BorderSpacing := FDisplayOptions.BorderSpacing;
        PageControl.Align := FDisplayOptions.Align;
        PageControl.Parent := FDisplayOptions.Parent;
        NewPage.FControl := PageControl;
        SetObjectProperties(PageControl, NewPage.Properties);
        DoLoadControl(NewPage);
        CallMethod(PageControl, 'PageLoad');
      end;
    end;
    if PageControl <> nil then
    begin
      PageControl.Visible := True;
      CallMethod(PageControl, 'PageShow');
    end;
  end;
  if (FPageIndex <> -1) then
  begin
   PageControl := Items[FPageIndex].Control;
   if PageControl <> nil then
   begin
     CallMethod(PageControl, 'PageHide');
     PageControl.Visible := False;
   end;
  end;
  FPageIndex := NewPageIndex;
end;

procedure TVirtualPages.DoLoadControl(Page: TVirtualPage);
begin
  if Assigned(FOnLoadControl) then
    FOnLoadControl(Self, Page);
end;

constructor TVirtualPages.Create;
begin
  inherited Create(TVirtualPage);
  FDisplayOptions := TControlDisplayOptions.Create;
  FPageIndex := -1;
end;

destructor TVirtualPages.Destroy;
begin
  FDisplayOptions.Destroy;
  inherited Destroy;
end;

procedure TVirtualPages.Add(const Caption: String; Control: TControl);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.Control := Control;
end;

procedure TVirtualPages.Add(const Caption: String; ControlClass: TControlClass;
  const Properties: array of const);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.ControlClass := ControlClass;
  Page.SetProperties(Properties);
end;

procedure TVirtualPages.Add(const Caption, ControlClassName: String;
  const Properties: array of const);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.ControlClassName := ControlClassName;
  Page.SetProperties(Properties);
end;

end.

