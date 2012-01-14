unit DropDownManager;

{$mode objfpc}{$H+}

interface

uses
  Controls, LMessages, Forms, Classes, SysUtils;

type

  { TDropDownManager }

  //todo:
  // make use of window shadow

  TDropDownOption = (ddoUsePopupForm);

  TDropDownOptions = set of TDropDownOption;

  TDropDownCreateControl = procedure(Sender: TObject; Control: TControl) of object;

  TDropDownManager = class(TComponent)
  private
    FControl: TWinControl;
    FControlClass: TWinControlClass;
    FMasterControl: TControl;
    FOnCreateControl: TDropDownCreateControl;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOptions: TDropDownOptions;
    FPopupForm: TForm;
    function ControlGrabsFocus(AControl: TControl): Boolean;
    procedure ControlNeeded;
    procedure FocusChangeHandler(Sender: TObject; LastControl: TControl);
    function GetDroppedDown: Boolean;
    procedure RemoveHandlers;
    procedure SetState(DoEvents: Boolean);
    procedure SetDroppedDown(const Value: Boolean);
    procedure UserInputHandler(Sender: TObject; Msg: Cardinal);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure UpdateState;
    property ControlClass: TWinControlClass read FControlClass write FControlClass;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
  published
    property Control: TWinControl read FControl write FControl;
    property MasterControl: TControl read FMasterControl write FMasterControl;
    property Options: TDropDownOptions read FOptions write FOptions;
    property OnCreateControl: TDropDownCreateControl read FOnCreateControl write FOnCreateControl;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

{ TDropDownManager }

function TDropDownManager.ControlGrabsFocus(AControl: TControl): Boolean;
begin
  Result := (AControl <> FControl) and (AControl <> FMasterControl) and
    not FControl.IsParentOf(AControl) and ((ddoUsePopupForm in FOptions) or (GetParentForm(FControl) = GetParentForm(AControl)));
end;

procedure TDropDownManager.ControlNeeded;
begin
  if FControl = nil then
  begin
    if FControlClass <> nil then
    begin
      FControl := FControlClass.Create(Self);
      if ddoUsePopupForm in FOptions then
      begin
        if FPopupForm = nil then
        begin
          FPopupForm := TForm.Create(nil);
          FPopupForm.BorderStyle := bsNone;
          FPopupForm.PopupMode := pmAuto;
        end;
        FPopupForm.SetBounds(FPopupForm.Left, FPopupForm.Top, FControl.Width, FControl.Height);
        FControl.Parent := FPopupForm;
      end
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

procedure TDropDownManager.FocusChangeHandler(Sender: TObject; LastControl: TControl);
begin
  if ControlGrabsFocus(Application.MouseControl) then
    DroppedDown := False;
end;

function TDropDownManager.GetDroppedDown: Boolean;
begin
  if FControl <> nil then
    Result := FControl.Visible
  else
    Result := False;
end;

procedure TDropDownManager.RemoveHandlers;
begin
  Application.RemoveOnUserInputHandler(@UserInputHandler);
  Screen.RemoveHandlerActiveControlChanged(@FocusChangeHandler);
end;

procedure TDropDownManager.SetState(DoEvents: Boolean);
var
  ControlIsVisible: Boolean;
begin
  if (ddoUsePopupForm in FOptions) and (FPopupForm <> nil) then
    ControlIsVisible := FPopupForm.Visible
  else
    ControlIsVisible := FControl.Visible;
  if ControlIsVisible then
  begin
    if FControl.CanFocus then
      FControl.SetFocus;
    if Assigned(FOnShow) and DoEvents then
      FOnShow(Self);
    Application.AddOnUserInputHandler(@UserInputHandler);
    Screen.AddHandlerActiveControlChanged(@FocusChangeHandler);
  end
  else
  begin
    RemoveHandlers;
    if Assigned(FOnHide) and DoEvents then
      FOnHide(Self);
  end;
end;

procedure TDropDownManager.SetDroppedDown(const Value: Boolean);
var
  P: TPoint;
begin
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
  begin
    if FControl.Visible = Value then
      Exit;
    FControl.Visible := Value;
  end;
  SetState(True);
end;

procedure TDropDownManager.UserInputHandler(Sender: TObject; Msg: Cardinal);
begin
  case Msg of
    LM_LBUTTONDOWN, LM_LBUTTONDBLCLK, LM_RBUTTONDOWN, LM_RBUTTONDBLCLK,
    LM_MBUTTONDOWN, LM_MBUTTONDBLCLK, LM_XBUTTONDOWN, LM_XBUTTONDBLCLK:
    begin
      if ControlGrabsFocus(Application.MouseControl) then
        DroppedDown := False;
    end;
  end;
end;

procedure TDropDownManager.Notification(AComponent: TComponent;
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

destructor TDropDownManager.Destroy;
begin
  RemoveHandlers;
  FPopupForm.Free;
  inherited Destroy;
end;

procedure TDropDownManager.UpdateState;
begin
  if FControl <> nil then
    SetState(False);
end;

end.

