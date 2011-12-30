unit DropDownManager;

{$mode objfpc}{$H+}

interface

uses
  Controls, LMessages, Forms, Classes, SysUtils;

type

  { TDropDownManager }

  //todo:
  // specific code for TForm/TFrame (make use of window shadow)

  TDropDownCreateControl = procedure(Sender: TObject; Control: TControl) of object;

  TDropDownManager = class(TComponent)
  private
    FControl: TWinControl;
    FControlClass: TWinControlClass;
    FMasterControl: TControl;
    FOnCreateControl: TDropDownCreateControl;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
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
    property OnCreateControl: TDropDownCreateControl read FOnCreateControl write FOnCreateControl;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

{ TDropDownManager }

function TDropDownManager.ControlGrabsFocus(AControl: TControl): Boolean;
begin
  Result := (AControl <> FControl) and (AControl <> FMasterControl) and
    not FControl.IsParentOf(AControl) and (GetParentForm(FControl) = GetParentForm(AControl));
end;

procedure TDropDownManager.ControlNeeded;
begin
  if FControl = nil then
  begin
    if FControlClass <> nil then
    begin
      FControl := FControlClass.Create(Self);
      if FMasterControl <> nil then
      begin
        FControl.Parent := FMasterControl.Parent;
        FControl.AnchorParallel(akLeft, 0, FMasterControl);
        FControl.AnchorToNeighbour(akTop, 2, FMasterControl);
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
  if ControlGrabsFocus(Screen.ActiveControl) then
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
begin
  if FControl.Visible then
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
begin
  ControlNeeded;
  if FControl.Visible = Value then
    Exit;
  FControl.Visible := Value;
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
      FMasterControl := nil;;
  end;
end;

destructor TDropDownManager.Destroy;
begin
  RemoveHandlers;
  inherited Destroy;
end;

procedure TDropDownManager.UpdateState;
begin
  ControlNeeded;
  SetState(False);
end;

end.

