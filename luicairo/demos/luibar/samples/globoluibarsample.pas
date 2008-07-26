unit GloboLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  { TGloboLuiBarSample }

  TGloboLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure DrawCell(Sender: TLuiBar; Cell: TLuiBarCell);
    procedure Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation


procedure TGloboLuiBarSample.DrawCell(Sender: TLuiBar; Cell: TLuiBarCell);
var
  CellColor: TCairoColor;
begin
  case Cell.Index mod 4 of
    0: CellColor := RGBToCairoColor(231, 2, 0);
    1: CellColor := RGBToCairoColor(151, 201, 0);
    2: CellColor := RGBToCairoColor(255, 153, 0);
    3: CellColor := RGBToCairoColor(0, 102, 255);
  end;
  if (Cell.Index = Sender.SelectedIndex) or (Cell.Index = Sender.HoverIndex) then
    Sender.Context.Rectangle(0, -10, Cell.Width, Cell.Height + 10)
  else
    Sender.Context.Rectangle(0, 0, Cell.Width, Cell.Height);
  Sender.Context.Color := CellColor;
  Sender.Context.Fill;
end;

procedure TGloboLuiBarSample.Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  Allowed := DrawType <> dtCell;
end;

constructor TGloboLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    Parent := Self;
    SetBounds(10, 10, 600, 45);
    Options := [lboHotTrack];
    OnDrawCell := @DrawCell;
    OnDrawing := @Drawing;
    OutLineWidth := 0;
    CellHeight := 35;
    OuterOffset := 10;
    InitialSpace := 0;
    Spacing := 1;
    TextAlign := taLeft;
    with Colors do
    begin
      Background := clWhite;
      Text := clWhite;
    end;
    Cells.Add(UTF8Encode('notícias'));
    Cells.Add('esportes');
    Cells.Add('entretenimento');
    Cells.Add(UTF8Encode('vídeos'));
    Visible := True;
  end;
end;

end.

