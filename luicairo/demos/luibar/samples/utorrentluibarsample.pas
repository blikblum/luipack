unit UTorrentLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type

  { TUTorrentLuiBarSample }

  TUTorrentLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure GetCellPattern(Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure AfterDraw(Sender: TLuiBar);
    procedure Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CairoUtils;

{ TUTorrentLuiBarSample }


procedure TUTorrentLuiBarSample.GetCellPattern(Sender: TLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  Exit;
  if (PatternType = ptText) and (Sender.HoverIndex = Cell.Index) then
    Pattern := Sender.Patterns.BackGround;
end;

procedure TUTorrentLuiBarSample.AfterDraw(Sender: TLuiBar);
var
  R: TDoubleRect;
begin
  with Sender do
  begin
    CalculateSharpRect(0, 0, Width, Height, 2, R);
    Context.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    Context.Source := Patterns.Hover;
    Context.Stroke;
  end;
end;

procedure TUTorrentLuiBarSample.Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  //draw only hover or selected cells
  Allowed := (DrawType <> dtCell) or
    ((Cell.Index = Sender.HoverIndex) or (Cell.Index = Sender.SelectedIndex));
end;

constructor TUTorrentLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    Parent := Self;
    SetBounds(10, 10, 400, 200);
    Options := [lboHotTrack, lboVariableCellWidth, lboEmulateTab];
    CellAlign := caInvert;
    CellHeight := 35;
    OutLineWidth := 1;
    OuterOffset := 1;
    InitialSpace := 1;
    OnGetCellPattern := @GetCellPattern;
    OnDrawing := @Drawing;
    OnAfterDraw := @AfterDraw;
    with Colors do
    begin
      Selected := RGBToColor(234, 245, 230);
      Background := RGBToColor(207, 236, 200);
      Text := RGBToColor(68, 68, 68);
      SelectedText := Text;
      Hover := RGBToColor(195, 216, 191);
      OutLine := Hover;
      ClientArea := clWhite;
    end;
    Cells.Add('Home');
    Cells.Add('Download');
    Cells.Add('Skins');
    Cells.Add('FAQ');

    Visible := True;
  end;
end;

initialization


end.

