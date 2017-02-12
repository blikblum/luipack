unit DropDownManager;

{$mode objfpc}{$H+}

interface

uses
  Controls, LMessages, Forms, Classes, SysUtils;

type

  { TCustomDropDownManager }

  //todo:
  // make use of window shadow

  TDropDownOption = (ddoUsePopupForm, ddoSetFocus);

  TDropDownOptions = set of TDropDownOption;

  TDropDownCreateControl = procedure(Sender: TObject; Control: TControl) of object;

  TDropDownState = (ddsUndefined, ddsVisible, ddsHidden);

  TCustomDropDownManager = class(TComponent)
  private
    FControl: TWinControl;
    FControlClass: TWinControlClass;
    FMasterControl: TControl;
    FOnCreateControl: TDropDownCreateControl;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOptions: TDropDownOptions;
    FPopupForm: TForm;
    //todo: replace FVisible by TDropDownState?
    FVisible: Variant;
    FDropDownState: TDropDownState;
    FInitialized: Boolean;
    function ControlGrabsFocus(AControl: TControl): Boolean;
    procedure ControlNeeded;
    procedure FocusChangeHandler(Sender: TObject; LastControl: TControl);
    procedure FormFirstShow(Sender: TObject);
    procedure FormVisibleChanged(Sender: TObject; Form: TCustomForm);
    function GetVisible: Boolean;
    procedure InitializePopupForm;
    procedure RemoveHandlers;
    procedure SetControl(Value: TWinControl);
    procedure SetState(DoEvents: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure UserInputHandler(Sender: TObject; Msg: Cardinal);
  protected
    procedure DoHide; virtual;
    procedure DoInitialize(Data: PtrInt); virtual;
    procedure DoShow; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Control: TWinControl read FControl write SetControl;
    property MasterControl: TControl read FMasterControl write FMasterControl;
    property Options: TDropDownOptions read FOptions write FOptions default [];
    property OnCreateControl: TDropDownCreateControl read FOnCreateControl write FOnCreateControl;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ControlClass: TWinControlClass read FControlClass write FControlClass;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TDropDownManager = class (TCustomDropDownManager)
  published
    property Control;
    property MasterControl;
    property Options;
    property OnCreateControl;
    property OnHide;
    property OnShow;
  end;

implementation

uses
  LCLProc, variants;

{ TCustomDropDownManager }

function TCustomDropDownManager.ControlGrabsFocus(AControl: TControl): Boolean;
begin
  Result := (AControl <> nil) and (AControl <> FControl) and (AControl <> FMasterControl) and
    not FControl.IsParentOf(AControl) and ((ddoUsePopupForm in FOptions) or (GetParentForm(FControl) = GetParentForm(AControl)));
end;

procedure TCustomDropDownManager.ControlNeeded;
begin
  if FControl = nil then
  begin
    if FControlClass <> nil then
    begin
      FControl := FControlClass.Create(Self);
      if ddoUsePopupForm in FOptions then
        InitializePopupForm
      else
      begin
        FControl.Visible := False;
        if FMasterControl <> nil then
        begin
          FControl.Parent := FMasterControl.Parent;
          FControl.AnchorParallel(akLeft, 0, FMasterControl);
          FControl.AnchorToNeighbour(akTop, 2, FMasterControl);
        end;
      end;
      if Assigned(FOnCreateControl) then
        FOnCreateControl(Self, FControl);
    end;
    if FControl = nil then
      raise Exception.Create('TDropDownWindow: Control not defined');
  end;
end;

procedure TCustomDropDownManager.DoInitialize(Data: PtrInt);
begin
  FInitialized := True;
  if not VarIsEmpty(FVisible) then
    SetVisible(FVisible)
  else
  begin
    if FControl <> nil then
      SetState(False);
  end;
end;

procedure TCustomDropDownManager.FocusChangeHandler(Sender: TObject; LastControl: TControl);
begin
  if ControlGrabsFocus(LastControl) then
    Visible := False;
end;

function TCustomDropDownManager.GetVisible: Boolean;
begin
  if not FInitialized and not VarIsEmpty(FVisible) then
    Result := FVisible
  else
  begin
    if FControl <> nil then
      Result := FControl.Visible
    else
      Result := False;
  end;
end;

procedure TCustomDropDownManager.InitializePopupForm;
begin
  if FPopupForm = nil then
  begin
    FPopupForm := TForm.Create(nil);
    FPopupForm.BorderStyle := bsNone;
    FPopupForm.PopupMode := pmAuto;
  end;
  FPopupForm.SetBounds(FPopupForm.Left, FPopupForm.Top, FControl.Width, FControl.Height);
  FControl.Parent := FPopupForm;
end;

procedure TCustomDropDownManager.RemoveHandlers;
begin
  Application.RemoveOnUserInputHandler(@UserInputHandler);
  Screen.RemoveHandlerActiveControlChanged(@FocusChangeHandler);
end;

procedure TCustomDropDownManager.SetControl(Value: TWinControl);
begin
  if FControl = Value then Exit;
  FControl := Value;
  if (Value <> nil) and (ddoUsePopupForm in FOptions) then
    InitializePopupForm;
end;

procedure TCustomDropDownManager.SetState(DoEvents: Boolean);
var
  IsControlVisible: Boolean;
begin
  if (ddoUsePopupForm in FOptions) and (FPopupForm <> nil) then
    IsControlVisible := FPopupForm.Visible
  else
    IsControlVisible := FControl.Visible;
  if IsControlVisible then
  begin
    if FDropDownState <> ddsVisible then
    begin
      FDropDownState := ddsVisible;
      if (ddoSetFocus in FOptions) and FControl.CanFocus then
        FControl.SetFocus;
      if DoEvents then
        DoShow;
      Application.AddOnUserInputHandler(@UserInputHandler);
      Screen.AddHandlerActiveControlChanged(@FocusChangeHandler);
    end;
  end
  else
  begin
    if FDropDownState <> ddsHidden then
    begin
      FDropDownState := ddsHidden;
      RemoveHandlers;
      if DoEvents then
        DoHide;
    end;
  end;
end;

procedure TCustomDropDownManager.SetVisible(const Value: Boolean);
var
  P: TPoint;
begin
  if not FInitialized then
  begin
    FVisible := Value;
    Exit;
  end;
  if (FControl = nil) and not Value then
    Exit;
  ControlNeeded;
  if (ddoUsePopupForm in FOptions) then
  begin
    if FPopupForm.Visible = Value then
      Exit;
    if Value then
    begin
      if FMasterControl <> nil then
      begin
        P := Point(0, FMasterControl.Height);
        P := FMasterControl.ClientToScreen(P);
        FPopupForm.SetBounds(P.x, p.y + 2, FPopupForm.Width, FPopupForm.Height);
      end;
      FPopupForm.Show;
    end
    else
      FPopupForm.Hide;
  end
  else
    FControl.Visible := Value;
  SetState(True);
end;

procedure TCustomDropDownManager.UserInputHandler(Sender: TObject; Msg: Cardinal);
begin
  case Msg of
    LM_LBUTTONDOWN, LM_LBUTTONDBLCLK, LM_RBUTTONDOWN, LM_RBUTTONDBLCLK,
    LM_MBUTTONDOWN, LM_MBUTTONDBLCLK, LM_XBUTTONDOWN, LM_XBUTTONDBLCLK:
    begin
      if ControlGrabsFocus(Application.MouseControl) then
        Visible := False;
    end;
  end;
end;

procedure TCustomDropDownManager.FormFirstShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoInitialize, 0);
end;

procedure TCustomDropDownManager.FormVisibleChanged(Sender: TObject;
  Form: TCustomForm);
var
  ControlOwner: TControl;
  AOwner: TComponent;
begin
  AOwner := Owner;
  ControlOwner := nil;
  while (AOwner <> nil) do
  begin
    if AOwner is TControl then
    begin
      ControlOwner := TControl(AOwner);
      Break;
    end
    else
      AOwner := AOwner.Owner;
  end;
  if Form.IsParentOf(ControlOwner) then
  begin
    Form.AddHandlerFirstShow(@FormFirstShow);
    Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChanged);
  end;
end;

procedure TCustomDropDownManager.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TCustomDropDownManager.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TCustomDropDownManager.Loaded;
var
  Form: TCustomForm;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and (Owner is TControl) then
  begin
    Form := GetParentForm(TControl(Owner));
    //todo: review approach when Form is not acessible at this time
    if Form <> nil then
      Form.AddHandlerFirstShow(@FormFirstShow)
    else
      Screen.AddHandlerFormVisibleChanged(@FormVisibleChanged);
  end;
end;

procedure TCustomDropDownManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FControl then
      FControl := nil
    else if AComponent = FMasterControl then
      FMasterControl := nil;
  end;
end;

destructor TCustomDropDownManager.Destroy;
begin
  RemoveHandlers;
  FPopupForm.Free;
  inherited Destroy;
end;

procedure TCustomDropDownManager.Assign(Source: TPersistent);
begin
  if Source is TCustomDropDownManager then
  begin
    FControl := TCustomDropDownManager(Source).FControl;
    FControlClass := TCustomDropDownManager(Source).FControlClass;
    FMasterControl := TCustomDropDownManager(Source).FMasterControl;
    FOptions := TCustomDropDownManager(Source).FOptions;
    FOnCreateControl := TCustomDropDownManager(Source).FOnCreateControl;
    FOnHide := TCustomDropDownManager(Source).FOnHide;
    FOnShow := TCustomDropDownManager(Source).FOnShow;
  end
  else
    inherited Assign(Source);
end;

end.

