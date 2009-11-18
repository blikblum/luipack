unit GnomeDoLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type

  { TGnomeDoLuiBarSample }

  TGnomeDoLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    FImages: TImageList;
    procedure CreatePattern(Sender: TLuiBar; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
    procedure GetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
    procedure GetCellPattern(Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure DrawBackground(Sender: TLuiBar);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CairoUtils;

const
  SelectedOutlinePatternId = 0;

{ TGnomeDoLuiBarSample }

procedure TGnomeDoLuiBarSample.CreatePattern(Sender: TLuiBar;
  PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal:
      Pattern := TCairoSolidPattern.Create(1, 1, 1, 0.2);
    ptSelected, ptHover:
      Pattern := TCairoSolidPattern.Create(1, 1, 1, 0.4);
    ptOutLine:
      Pattern := TCairoSolidPattern.Create(1, 1, 1, 0.3);
  end;
end;

procedure TGnomeDoLuiBarSample.GetImageInfo(Sender: TLuiBar;
  Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
begin
  ImageInfo.Index := Cell.Index;
  //ImageInfo.Effect := ;
end;

procedure TGnomeDoLuiBarSample.GetCellPattern(Sender: TLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  if (PatternType = ptOutLine) and
    ((Cell.Index = Sender.SelectedIndex) or (Cell.Index = Sender.HoverIndex)) then
    Pattern := Sender.Patterns[SelectedOutlinePatternId];
end;

procedure TGnomeDoLuiBarSample.DrawBackground(Sender: TLuiBar);
var
  R: TDoubleRect;
  RoundInfo: TRoundedRectInfo;
begin
  with Sender do
  begin
    RoundedRectangle(Context, 0, 0, Width, Height, 10);
    Context.Color := RGBToCairoColor(71, 61, 122);
    Context.Fill;

    CalculateSharpRect(1, 1, Width - 2, Height div 2, 1, R);
    FillChar(RoundInfo, SizeOf(RoundInfo), 0);
    RoundInfo.TopLeftRadius := 10;
    RoundInfo.TopRightRadius := 10;
    RoundInfo.BottomRadius := -Height / 6;
    RoundedRectangle(Context, R, RoundInfo);
    Context.SetSourceRgba(1, 1, 1, 0.2);
    Context.Fill;
  end;
end;

constructor TGnomeDoLuiBarSample.Create(AOwner: TComponent);
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
    SetBounds(10, 10, 500, 120);
    //Selected outline pattern
    Patterns[SelectedOutlinePatternId] := TCairoSolidPattern.Create(1, 1, 1, 0.6);
    Options := [lboCenterImage, lboHotTrack, lboHoverAsSelected];
    ImagePosition := ipTop;
    OnDrawBackground := @DrawBackground;
    OnCreatePattern := @CreatePattern;
    OnGetCellPattern := @GetCellPattern;
    CellRoundRadius := 10;
    CellAlign := caCenter;
    Images := FImages;
    OnGetImageInfo := @GetImageInfo;
    CellWidth := 100;
    CellHeight := 100;
    OuterOffset := (Height - CellHeight) div 2;
    Spacing := 5;
    with Colors do
    begin
      Text := clWhite;
    end;
    OutLineWidth := 1;
    Cells.Add('Test1');
    Cells.Add('Test2');
    Cells.Add('Test3');
    Cells.Add('Test4');
    EndUpdate;
    Visible := True;
  end;
end;

initialization
  {$I bigimages.lrs}

end.

