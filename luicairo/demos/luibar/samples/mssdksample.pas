unit MSSDKSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  { TMSSDKLuiBarSample }

  TMSSDKLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure Drawing(Sender: TCustomLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
    procedure DrawCell(Sender: TCustomLuiBar; Cell: TLuiBarCell);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TMSSDKLuiBarSample }

procedure TMSSDKLuiBarSample.DrawCell(Sender: TCustomLuiBar; Cell: TLuiBarCell);
begin
  with Sender, Context do
  begin
    if SelectedIndex = Cell.Index then
    begin
      MoveTo(Cell.Width - 0.5, 1);
      LineTo(Cell.Width - 0.5, Cell.Height + 1);
      Source := Patterns.Text; //Black
      LineWidth := 1;
      Stroke;
    end
    else
      if Cell.Index <> SelectedIndex - 1 then
      begin
        MoveTo(Cell.Width - 0.5, 2);
        LineTo(Cell.Width - 0.5, Cell.Height - 2);
        LineWidth := 1;
        Color := CairoColor(0.5,0.5,0.5,1);
        Stroke;
      end;
  end;
end;

procedure TMSSDKLuiBarSample.Drawing(Sender: TCustomLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  //draw only selected cells
  Allowed := (DrawType <> dtCell) or (Sender.SelectedIndex = Cell.Index);
end;

constructor TMSSDKLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 28);
    Options := [lboEmulateTab, lboVariableCellWidth];
    OnDrawCell := @DrawCell;
    OnDrawing := @Drawing;
    OutLineWidth := 1;
    CellHeight := 22;
    OuterOffset := 2;
    InitialSpace := 4;
    with Colors do
    begin
      Normal := RGBToColor(247,243,233);
      Background := Normal;
      Hover := Normal;
      Selected := RGBToColor(212, 208, 200);
      ClientArea := Selected;
      SelectedText := clBlack;
      Text := clBlack;
      OutLine := clWhite;
    end;
    Cells.Add('Web');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add('Imagens');
    EndUpdate;
    Visible := True;
  end;
end;

end.

