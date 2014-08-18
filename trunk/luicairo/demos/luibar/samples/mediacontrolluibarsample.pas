unit MediaControlLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type

  { TMediaControlLuiBarSample }

  TMediaControlLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    FImages: TImageList;
    FPlaying: Boolean;
    FAudioOn: Boolean;
    FButtonPressed: Boolean;
    procedure GetCellPattern(Sender: TCustomLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure GetImageInfo(Sender: TCustomLuiBar; Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
    procedure CreatePattern(Sender: TCustomLuiBar; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
    procedure DrawCellPath(Sender: TCustomLuiBar; Cell: TLuiBarCell);
    procedure CellSelect(Sender: TCustomLuiBar);
    procedure MouseDownHandle(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure MouseUpHandle(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CairoUtils;

const
  ButtonPressedPatternId = 0;
  

{ MediaControlLuiBarSample }

procedure TMediaControlLuiBarSample.GetCellPattern(Sender: TCustomLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  //if audio is off, highlight
  if (Cell.Index = 4) and (PatternType = ptNormal) and not FAudioOn then
    Pattern := Sender.Patterns.Selected;
  //Homebrew button pressed emulation
  if (PatternType = ptHover) and FButtonPressed then
    Pattern := Sender.Patterns[ButtonPressedPatternId];
end;

procedure TMediaControlLuiBarSample.GetImageInfo(Sender: TCustomLuiBar;
  Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
begin
  case Cell.Index of
  0: //Stop
    begin
      ImageInfo.Index := 0;
    end;
  1: //Backward
    begin
      ImageInfo.Index := 1;
    end;
  2: //Play/Pause
    begin
      if FPlaying then
        ImageInfo.Index := 2
      else
        ImageInfo.Index := 3;
    end;
  3: //Forward
    begin
      ImageInfo.Index := 4;
    end;
  4: //Audio
    begin
      if FAudioOn then
        ImageInfo.Index := 5
      else
        ImageInfo.Index := 6;
    end;
  end;
end;

procedure TMediaControlLuiBarSample.CreatePattern(Sender: TCustomLuiBar;
  PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal:
      begin
        Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
        with TCairoLinearGradient(Pattern) do
        begin
          AddColorStop(0, RGBToCairoColor(235, 239, 243));
          AddColorStop(0.5, RGBToCairoColor(202, 207, 213));
          AddColorStop(1, RGBToCairoColor(235, 239, 243));
        end;
      end;
    ptSelected:
      begin
        Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
        with TCairoLinearGradient(Pattern) do
        begin
          AddColorStop(0, RGBToCairoColor(186, 204, 147));
          AddColorStop(0.5, RGBToCairoColor(180, 198, 132));
          AddColorStop(1, RGBToCairoColor(186, 204, 147));
        end;
      end;
  end;
end;

procedure TMediaControlLuiBarSample.DrawCellPath(Sender: TCustomLuiBar;
  Cell: TLuiBarCell);
var
  R: TDoubleRect;
  RoundInfo: TRoundedRectInfo;
begin
  Sender.Context.NewPath;
  CalculateSharpRect(4, 8, Cell.Width - 8, Cell.Height - 16, 1, R);
  case Cell.Index of
  0: //Stop
    begin
      RoundedRectangle(Sender.Context, R, 5);
    end;
  1, 3: //Backward/Forward
    begin
      FillChar(RoundInfo, SizeOf(RoundInfo), 0);
      if Cell.Index = 1 then
      begin
        RoundInfo.RightRadius := -10;
        RoundInfo.TopLeftRadius := 5;
        RoundInfo.BottomLeftRadius := 5;
      end
      else
      begin
        RoundInfo.LeftRadius := -10;
        RoundInfo.TopRightRadius := 5;
        RoundInfo.BottomRightRadius := 5;
      end;
      RoundedRectangle(Sender.Context, R, RoundInfo);
    end;
  2: //Play/Pause
    begin
      Sender.Context.Arc(Cell.Width div 2, Cell.Height div 2,
        Cell.Height div 2, 0, 2 * Pi);
    end;
  4: //Audio On/Off
    begin
      RoundedRectangle(Sender.Context, R, 5);
    end;
  end;
end;

procedure TMediaControlLuiBarSample.CellSelect(Sender: TCustomLuiBar);
begin
  case Sender.SelectedIndex of
    2: FPlaying := not FPlaying;
    4: FAudioOn := not FAudioOn;
  end;
end;

procedure TMediaControlLuiBarSample.MouseDownHandle(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FButtonPressed := True;
    FBar.Redraw;
  end;
end;

procedure TMediaControlLuiBarSample.MouseUpHandle(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FButtonPressed := False;
    FBar.Redraw;
  end;
end;

constructor TMediaControlLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAudioOn := True;
  Color := clWhite;
  FImages := TImageList.Create(AOwner);
  with FImages do
  begin
    Width := 22;
    Height := 22;
    AddLazarusResource('media-playback-stop');
    AddLazarusResource('media-skip-backward');
    AddLazarusResource('media-playback-pause');
    AddLazarusResource('media-playback-start');
    AddLazarusResource('media-skip-forward');
    AddLazarusResource('audio-volume-high');
    AddLazarusResource('audio-volume-muted');
  end;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 500, 120);
    Patterns[ButtonPressedPatternId] := TCairoSolidPattern.Create(RGBToCairoColor(147, 163, 107));
    Options := [lboCenterImage, lboHotTrack, lboTransitorySelect, lboHoverAsSelected];
    OnCreatePattern := @CreatePattern;
    OnDrawCellPath := @DrawCellPath;
    OnGetImageInfo := @GetImageInfo;
    OnGetCellPattern := @GetCellPattern;
    OnSelect := @CellSelect;
    OnMouseDown := @MouseDownHandle;
    OnMouseUp := @MouseUpHandle;
    Images := FImages;
    CellWidth := 50;
    CellHeight := 50;
    OuterOffset := (Height - CellHeight) div 2;
    with Colors do
    begin
      OutLine := clGray;
    end;
    OutLineWidth := 1;
    Cells.Add('');
    Cells.Add('');
    Cells.Add('');
    Cells.Add('');
    Cells.Add('');
    EndUpdate;
    Visible := True;
  end;
end;

initialization
  {$I mediaimages.lrs}

end.

