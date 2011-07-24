unit DropDownButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DropDownManager, Controls, LCLType, Buttons, Forms;

type

  { TDropDownButton }

  TDropDownButton = class(TCustomSpeedButton)
  private
    FManager: TDropDownManager;
    procedure DropDownHide(Sender: TObject);
    procedure FormVisibleChange(Sender: TObject; Form: TCustomForm);
    function GetDropDownControl: TWinControl;
    function GetDroppedDown: Boolean;
    procedure SetDropDownControl(const Value: TWinControl);
    procedure SetDroppedDown(const Value: Boolean);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
  published
    property DropDownControl: TWinControl read GetDropDownControl write SetDropDownControl;
    //
    property Action;
    property Align;
    property Anchors;
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
  Down := False;
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
  //necessary to the button toggle
  AllowAllUp := True;
  GroupIndex := 1;
end;

destructor TDropDownButton.Destroy;
begin
  Screen.RemoveHandlerFormVisibleChanged(@FormVisibleChange);
  inherited Destroy;
end;

procedure TDropDownButton.Click;
begin
  inherited Click;
  FManager.DroppedDown := Down;
end;

end.

