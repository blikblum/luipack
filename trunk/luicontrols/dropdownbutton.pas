unit DropDownButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DropDownManager, DropDownBaseButtons, Controls, LCLType, Buttons, Forms,
  Graphics;

type

  TDropDownButtonOption = (dboShowIndicator);

  TDropDownButtonOptions = set of TDropDownButtonOption;

  { TDropDownButton }
  //todo add TDropDownBaseButton or TCustomDropDownButton to merge functions of TMenuButton
  TDropDownButton = class(TCustomDropDownButton)
  private
    FManager: TDropDownManager;
    procedure DropDownHide(Sender: TObject);
    procedure FormVisibleChange(Sender: TObject; Form: TCustomForm);
    function GetDropDownControl: TWinControl;
    function GetDroppedDown: Boolean;
    procedure SetDropDownControl(const Value: TWinControl);
    procedure SetDroppedDown(const Value: Boolean);
  protected
    procedure DoShowDropDown; override;
    procedure DoHideDropDown; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Manager: TDropDownManager read FManager;
  published
    property DropDownControl: TWinControl read GetDropDownControl write SetDropDownControl;
    property Options;
    //
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property Layout;
    property Margin;
    property NumGlyphs;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowCaption;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

{ TDropDownButton }

procedure TDropDownButton.DropDownHide(Sender: TObject);
begin
  DropDownClosed;
end;

procedure TDropDownButton.FormVisibleChange(Sender: TObject; Form: TCustomForm);
begin
  if Form.Visible then
  begin
    FManager.UpdateState;
    Down := FManager.DroppedDown;
  end;
  Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChange);
end;

function TDropDownButton.GetDropDownControl: TWinControl;
begin
  Result := FManager.Control;
end;

function TDropDownButton.GetDroppedDown: Boolean;
begin
  Result := FManager.DroppedDown;
end;

procedure TDropDownButton.SetDropDownControl(const Value: TWinControl);
begin
  FManager.Control := Value;
end;

procedure TDropDownButton.SetDroppedDown(const Value: Boolean);
begin
  Down := Value;
  FManager.DroppedDown := Value;
end;

procedure TDropDownButton.DoShowDropDown;
begin
  FManager.DroppedDown := True;
end;

procedure TDropDownButton.DoHideDropDown;
begin
  FManager.DroppedDown := False;
end;

procedure TDropDownButton.Loaded;
begin
  inherited Loaded;
  Screen.AddHandlerFormVisibleChanged(@FormVisibleChange);
end;

constructor TDropDownButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FManager := TDropDownManager.Create(Self);
  FManager.MasterControl := Self;
  FManager.OnHide := @DropDownHide;
end;

destructor TDropDownButton.Destroy;
begin
  Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChange);
  inherited Destroy;
end;

end.

