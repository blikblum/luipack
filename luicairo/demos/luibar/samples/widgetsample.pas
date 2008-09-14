unit WidgetSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, Graphics, LuiCairoControls, CairoClasses,
  Controls, Dialogs, Menus;

type

  { TWidgetLuiBarSample }

  TWidgetLuiBarSample = class(TPage)
  private
    FSimpleWidget: TLuiWidget;
    FLabelWidget: TLuiLabel;
    FMenuButtonWidget: TLuiMenuButton;
    FPopup: TPopupMenu;
    procedure SimpleWidgetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure SimpleWidgetDraw(Sender: TLuiWidget; Context: TCairoContext);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CairoUtils, CairoLCL, Cairo;
  
{ TWidgetLuiBarSample }

procedure TWidgetLuiBarSample.SimpleWidgetMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ShowMessage('Clicked in Custom Widget');
end;

procedure TWidgetLuiBarSample.SimpleWidgetDraw(Sender: TLuiWidget;
  Context: TCairoContext);
var
  R: TDoubleRect;
  Extents: cairo_text_extents_t;
  Str: String;
begin
  //draw a rounded rectangle
  if wsMouseInWidget in Sender.States then
    Context.Color := ColorToCairoColor(clBlue)
  else
    Context.Color := ColorToCairoColor(clRed);
  Context.LineWidth := 1;
  CalculateSharpRect(0, 0, Sender.Width, Sender.Height, 1, R);
  RoundedRectangle(Context, R, 4);
  Context.Stroke;

  //draw some text
  Str := 'Custom Widget - Click Me';
  Context.TextExtents(Str, @Extents);
  Context.MoveTo((Sender.Width - Extents.width) / 2,
    ((Sender.Height - Extents.height) / 2) + Extents.height);
  Context.ShowText(Str);
end;

constructor TWidgetLuiBarSample.Create(AOwner: TComponent);
var
  NewMenuItem: TMenuItem;
begin
  inherited Create(AOwner);
  FSimpleWidget := TLuiWidget.Create(Self);
  with FSimpleWidget do
  begin
    SetBounds(200, 15, 130, 25);
    WidgetOptions := [lwoHotTrack];
    OnDraw := @SimpleWidgetDraw;
    OnMouseDown := @SimpleWidgetMouseDown;
  end;
  FLabelWidget := TLuiLabel.Create(Self);
  with FLabelWidget do
  begin
    SetBounds(200, 40, 125, 25);
    Caption := 'Label Widget';
  end;
  FPopup := TPopupMenu.Create(Self);
  with FPopup do
  begin
    NewMenuItem := TMenuItem.Create(FPopup);
    NewMenuItem.Caption := 'MenuItem1';
    Items.Add(NewMenuItem);
    Items.AddSeparator;
    NewMenuItem := TMenuItem.Create(FPopup);
    NewMenuItem.Caption := 'MenuItemY';
    Items.Add(NewMenuItem);
  end;
  FMenuButtonWidget := TLuiMenuButton.Create(Self);
  with FMenuButtonWidget do
  begin
    SetBounds(340, 15, 120, 25);
    Caption := 'Menu Button Widget';
    PopupMenu := FPopup;
    WidgetOptions := [lwoHotTrack];
  end;
  //Color := clWhite;
  with TLuiBar.Create(Self) do
  begin
    Parent := Self;
    SetBounds(10, 10, 500, 80);
    Options := [lboOutLineClientArea];
    CellWidth := 50;
    CellHeight := 50;
    OuterOffset := 15;
    OutLineWidth := 1;
    Spacing := 4;
    InitialSpace := 10;
    with Colors do
    begin
      Normal := RGBToColor(180, 206, 180);
      Hover := Normal;
      Selected := RGBToColor(51, 102, 51);
      ClientArea := Selected;
      SelectedText := clWhite;
      OutLine := clRed;
      Text := clWhite;
    end;
    Cells.Add('Item1');
    Cells.Add('Item2');
    Cells.Add('Item3');
    
    Widgets.Add(FSimpleWidget);
    Widgets.Add(FLabelWidget);
    Widgets.Add(FMenuButtonWidget);

    Visible := True;
  end;
end;

end.

