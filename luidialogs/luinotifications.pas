unit LuiNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, StdCtrls, ExtCtrls, Buttons,
  LMessages, Forms;

type
  TNotificationType = (ntMessage, ntInformation, ntWarning, ntError, ntSuccess);

  TNotificationTypes = set of TNotificationType;

  TNotificationPosition = (npChildTop, npChildBottom, npSiblingAbove, npSiblingBelow);

procedure ShowNotification(AnchorControl: TWinControl; const Msg: String;
  NotificationType: TNotificationType; Position: TNotificationPosition;
  TimeOut: Integer = 0);

procedure HideNotification(ParentControl: TWinControl; NotificationTypes: TNotificationTypes);

implementation

uses
  LCLIntf;

const
  NotificationControlIntfId = 'lui_notificationcontrol';

type

  {$INTERFACES CORBA}

  { INotificationControl }

  INotificationControl = interface
    [NotificationControlIntfId]
    function GetNotificationType: TNotificationType;
    procedure SetNotificationType(Value: TNotificationType);
    property NotificationType: TNotificationType read GetNotificationType write SetNotificationType;
  end;

  { TDefaultNotificationControl }

  TDefaultNotificationControl = class(TCustomControl, INotificationControl)
  private
    FLabel: TLabel;
    FTimer: TTimer;
    FCloseButton: TSpeedButton;
    FNotificationType: TNotificationType;
    procedure HideMessage(Sender: TObject);
    procedure DelayedDestroy(Data: PtrInt);
    procedure SetMessage(const Value: String);
    procedure SetTimeout(const Value: Integer);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure VisibleChanged; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    function GetNotificationType: TNotificationType;
    procedure SetNotificationType(Value: TNotificationType);
    property Message: String write SetMessage;
    property NotificationType: TNotificationType read GetNotificationType write SetNotificationType;
    property Timeout: Integer write SetTimeout;
  end;
{ TDefaultNotificationControl }

procedure TDefaultNotificationControl.HideMessage(Sender: TObject);
begin
  Visible := False;
end;

procedure TDefaultNotificationControl.DelayedDestroy(Data: PtrInt);
begin
  Destroy;
end;

procedure TDefaultNotificationControl.SetMessage(const Value: String);
begin
  FLabel.Caption := Value;
end;

procedure TDefaultNotificationControl.SetTimeout(const Value: Integer);
begin
  if FTimer  = nil then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := @HideMessage;
  end;
  FTimer.Interval := Value;
end;

procedure TDefaultNotificationControl.CreateWnd;
begin
  inherited CreateWnd;
  Canvas.Brush.Color := clYellow;
end;

procedure TDefaultNotificationControl.Paint;
begin
  Canvas.Rectangle(0, 0, Width, Height);
end;

procedure TDefaultNotificationControl.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible then
  begin
    if (FTimer <> nil) and (FTimer.Interval > 0) then
      FTimer.Enabled := True;
  end
  else
  begin
    //auto destroy on hide
    Application.QueueAsyncCall(@DelayedDestroy, 0);
  end;
end;

procedure TDefaultNotificationControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // Do nothing. Just to avoid flicker.
end;

constructor TDefaultNotificationControl.Create(AOwner: TComponent);
var
  CloseBitmap: TCustomBitmap;
begin
  inherited Create(AOwner);
  BorderSpacing.Around := 2;

  FCloseButton := TSpeedButton.Create(Self);
  FCloseButton.Parent := Self;
  FCloseButton.OnClick := @HideMessage;
  FCloseButton.Flat := True;
  CloseBitmap := GetButtonIcon(idButtonCancel);
  try
    FCloseButton.Glyph.Assign(CloseBitmap);
  finally
    CloseBitmap.Free;
  end;
  FCloseButton.Width := FCloseButton.Glyph.Width + 2;
  FCloseButton.Height := FCloseButton.Glyph.Height + 2;
  FCloseButton.Top := 2;
  FCloseButton.Anchors := [akRight, akTop];
  FCloseButton.AnchorParallel(akRight, 2, Self);
  FCloseButton.Visible := True;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.WordWrap := True;
  FLabel.Layout := tlCenter;
  FLabel.Left := 4;
  FLabel.AnchorVerticalCenterTo(Self);
  FLabel.Visible := True;

  Height := FCloseButton.Height + 4;
end;

function TDefaultNotificationControl.GetNotificationType: TNotificationType;
begin
  Result := FNotificationType;
end;

procedure TDefaultNotificationControl.SetNotificationType(
  Value: TNotificationType);
begin
  FNotificationType := Value;
end;

procedure ShowNotification(AnchorControl: TWinControl; const Msg: String;
  NotificationType: TNotificationType; Position: TNotificationPosition;
  TimeOut: Integer = 0);
var
  Control: TDefaultNotificationControl;
  ParentControl: TWinControl;
begin
  if AnchorControl = nil then
    raise Exception.Create('ShowNotification: AnchorControl not defined');
  if Position in [npSiblingAbove, npSiblingBelow] then
  begin
    ParentControl := AnchorControl.Parent
    if ParentControl = nil then
      raise Exception.Create('ShowNotification: ParentControl not found');
  end
  else
    ParentControl := AnchorControl;
  Control := TDefaultNotificationControl.Create(ParentControl);
  Control.Parent := ParentControl;
  case Position of
    npChildTop: Control.Align := alTop;
    npChildBottom: Control.Align := alBottom;
    npSiblingAbove:
      begin
        Control.Anchors := [akRight, akBottom, akLeft];
        Control.AnchorToNeighbour(akBottom, 2, AnchorControl);
        Control.AnchorSide[akLeft].Side:=asrLeft;
        Control.AnchorSide[akLeft].Control:=ParentControl;
        Control.AnchorSide[akRight].Side:=asrRight;
        Control.AnchorSide[akRight].Control:=ParentControl;
      end;
    npSiblingBelow:
      begin
        Control.Anchors := [akRight, akTop, akLeft];
        Control.AnchorToNeighbour(akTop, 2, AnchorControl);
        Control.AnchorSide[akLeft].Side:=asrLeft;
        Control.AnchorSide[akLeft].Control:=ParentControl;
        Control.AnchorSide[akRight].Side:=asrRight;
        Control.AnchorSide[akRight].Control:=ParentControl;
      end;
  end;
  Control.Message := Msg;
  Control.Timeout := TimeOut;
  Control.Visible := True;
end;

procedure HideNotification(ParentControl: TWinControl;
  NotificationTypes: TNotificationTypes);
begin
  //todo
end;


end.

