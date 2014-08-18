unit SqliteLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type


  { TSqliteLuiBarSample }

  TSqliteLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure GetCellPattern(Sender: TCustomLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure DrawBackground(Sender: TCustomLuiBar);
    procedure Drawing(Sender: TCustomLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CairoUtils;

{ TSqliteLuiBarSample }


procedure TSqliteLuiBarSample.GetCellPattern(Sender: TCustomLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  if (PatternType = ptText) and (Sender.HoverIndex = Cell.Index) then
    Pattern := Sender.Patterns.BackGround;
end;

procedure TSqliteLuiBarSample.DrawBackground(Sender: TCustomLuiBar);
begin
  with Sender do
  begin
    RoundedRectangle(Context, 0, 0, Width, Height, 8);
    Context.Source := Patterns.BackGround;
    Context.Fill;
  end;
end;

procedure TSqliteLuiBarSample.Drawing(Sender: TCustomLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  //don't draw background
  Allowed := (DrawType <> dtBackground);
  //draw only hover cells
  if Allowed then
    Allowed := (DrawType <> dtCell) or (Cell.Index = Sender.HoverIndex);
end;

constructor TSqliteLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 28);
    Options := [lboHotTrack, lboVariableCellWidth, lboTransitorySelect];
    CellAlign := caCenter;
    CellHeight := 28;
    OutLineWidth := 0;
    OnGetCellPattern := @GetCellPattern;
    OnDrawing := @Drawing;
    OnDrawBackground := @DrawBackground;
    with Colors do
    begin
      Background := RGBToColor(128, 167, 150);
      Text := clWhite;
      Hover := clWhite;
    end;
    Cells.Add('About');
    Cells.Add('Sitemap');
    Cells.Add('Documentation');
    Cells.Add('Download');
    EndUpdate;
    Visible := True;
  end;
end;

initialization


end.

