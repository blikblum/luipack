unit LgMobileLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type

  { TLgMobileLuiBarSample }

  TLgMobileLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    FImages: TImageList;
    procedure GetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
    procedure GetCellPattern(Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure DrawCellPath(Sender: TLuiBar; Cell: TLuiBarCell);
    procedure Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

  
const
  HoverOutlinePattern = 0;
  SelectedOutlinePattern = 1;

{ TLgMobileLuiBarSample }

procedure TLgMobileLuiBarSample.GetImageInfo(Sender: TLuiBar;
  Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
begin
  ImageInfo.Index := Cell.Index;
  //ImageInfo.Effect := ;
end;

procedure TLgMobileLuiBarSample.GetCellPattern(Sender: TLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  if not (PatternType = ptOutLine) then
    Exit;
  if Cell.Index = Sender.SelectedIndex then
    Pattern := Sender.Patterns[SelectedOutlinePattern]
  else
    if Cell.Index = Sender.HoverIndex then
      Pattern := Sender.Patterns[HoverOutlinePattern]
end;

procedure TLgMobileLuiBarSample.DrawCellPath(Sender: TLuiBar; Cell: TLuiBarCell);
begin
  with Sender.Context do
  begin
    NewPath;
    Arc(50, 50, 40, 0, 2 * Pi);
  end;
end;

procedure TLgMobileLuiBarSample.Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  Allowed := DrawType <> dtCellPath;
end;

constructor TLgMobileLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  FImages := TImageList.Create(AOwner);
  with FImages do
  begin
    Width := 64;
    Height := 64;
    AddLazarusResource('book');
    AddLazarusResource('books');
    AddLazarusResource('host');
    AddLazarusResource('order');
  end;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 120, 500);
    Patterns[HoverOutlinePattern] := TCairoSolidPattern.Create(RGBToCairoColor(215, 208, 135));
    Patterns[SelectedOutlinePattern] := TCairoSolidPattern.Create(RGBToCairoColor(141, 89, 89));
    Options := [lboCenterImage, lboHotTrack];
    Position := poRight;
    OnGetCellPattern := @GetCellPattern;
    OnDrawCellPath := @DrawCellPath;
    OnDrawing := @Drawing;
    Images := FImages;
    OnGetImageInfo := @GetImageInfo;
    CellWidth := 100;
    CellHeight := 100;
    OuterOffset := 10;
    with Colors do
    begin
      OutLine := RGBToColor(143, 163, 213);
      Normal := RGBToColor(207, 221, 255);
      Selected := RGBToColor(255, 245, 221);
      Hover := clWhite;
      Background := RGBToColor(162, 180, 219);
    end;
    OutLineWidth := 3;
    Cells.Add('');
    Cells.Add('');
    Cells.Add('');
    Cells.Add('');
    EndUpdate;
    Visible := True;
  end;
end;

initialization
  {$I bigimages.lrs}

end.

