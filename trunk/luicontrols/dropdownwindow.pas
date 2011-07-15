unit DropDownWindow;

{$mode objfpc}{$H+}

interface

uses
  Controls, LMessages, Forms, Classes, SysUtils;

type

  { TDropDownWindow }

  //todo: add create control on demand, OnCreateControl
  // specific code for TForm/TFrame (make use of window shadow)
  TDropDownWindow = class(TComponent)
  private
    FControl: TWinControl;
    FMasterControl: TControl;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function ControlGrabsFocus(AControl: TControl): Boolean;
    procedure FocusChangeHandler(Sender: TObject; LastControl: TControl);
    function GetVisible: Boolean;
    procedure RemoveHandlers;
    procedure SetState(DoEvents: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure UserInputHandler(Sender: TObject; Msg: Cardinal);
  public
    destructor Destroy; override;
    procedure UpdateState;
    property Visible: Boolean read GetVisible write SetVisible;
  published
    property Control: TWinControl read FControl write FControl;
    property MasterControl: TControl read FMasterControl write FMasterControl;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

{ TDropDownWindow }

function TDropDownWindow.ControlGrabsFocus(AControl: TControl): Boolean;
begin
  Result := (AControl <> FControl) and (AControl <> FMasterControl) and
    not FControl.IsParentOf(AControl) and (GetParentForm(FControl) = GetParentForm(AControl));
end;

procedure TDropDownWindow.FocusChangeHandler(Sender: TObject; LastControl: TControl);
begin
  if ControlGrabsFocus(Screen.ActiveControl) then
    Visible := False;
end;

function TDropDownWindow.GetVisible: Boolean;
begin
  if FControl <> nil then
    Result := FControl.Visible
  else
    Result := False;
end;

procedure TDropDownWindow.RemoveHandlers;
begin
  Application.RemoveOnUserInputHandler(@UserInputHandler);
  Screen.RemoveHandlerActiveControlChanged(@FocusChangeHandler);
end;

procedure TDropDownWindow.SetState(DoEvents: Boolean);
begin
  if FControl.Visible then
  begin
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

procedure TDropDownWindow.SetVisible(const Value: Boolean);
begin
  if FControl = nil then
    raise Exception.Create('TDropDownWindow.Visible: Control not set');
  if FControl.Visible = Value then
    Exit;
  FControl.Visible := Value;
  SetState(True);
end;

procedure TDropDownWindow.UserInputHandler(Sender: TObject; Msg: Cardinal);
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

destructor TDropDownWindow.Destroy;
begin
  RemoveHandlers;
  inherited Destroy;
end;

procedure TDropDownWindow.UpdateState;
begin
  if FControl <> nil then
    SetState(False);
end;

end.

