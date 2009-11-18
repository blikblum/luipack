unit YouTubeSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  { TYouTubeLuiBarSample }

  TYouTubeLuiBarSample = class(TPage)
  private
    procedure CreatePattern(Sender: TLuiBar; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
    procedure GetCellPattern(Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TYouTubeLuiBarSample }

procedure TYouTubeLuiBarSample.CreatePattern(Sender: TLuiBar;
  PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(251, 252, 253));
        AddColorStop(0.5, RGBToCairoColor(210, 221, 244));
        AddColorStop(1, RGBToCairoColor(187, 199, 230));
      end;
    end;
    ptSelected:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(253, 253, 253));
        AddColorStop(0.5, RGBToCairoColor(229, 229, 229));
        AddColorStop(1, RGBToCairoColor(214, 214, 214));
      end;
    end;
    ptClientArea:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.Height -
        (Sender.CellHeight + Sender.OuterOffset));
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(213, 213, 213));
        AddColorStop(0.5, RGBToCairoColor(221, 221, 221));
        AddColorStop(1, RGBToCairoColor(231, 231, 231));
      end;
    end;
    ptHover:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(251, 251, 253));
        AddColorStop(0.5, RGBToCairoColor(243, 245, 248));
        AddColorStop(1, RGBToCairoColor(235, 238, 243));
      end;
    end;
  end;
end;

procedure TYouTubeLuiBarSample.GetCellPattern(Sender: TLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  if (PatternType = ptOutLine) and (Sender.SelectedIndex <> Cell.Index) then
    Pattern := Sender.Patterns.Indexed[0];
end;

constructor TYouTubeLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  with TLuiBar.Create(Self) do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 600, 100);
    Options := [lboEmulateTab, lboOmitBaseLine];
    OnCreatePattern := @CreatePattern;
    OnGetCellPattern := @GetCellPattern;
    OutLineWidth := 1;
    CellHeight := 30;
    CellWidth := 100;
    OuterOffset := 20;
    CellRoundRadius := 4;
    Spacing := 6;
    CellAlign := caCenter;
    with Colors do
    begin
      SelectedText := RGBToColor(51, 51, 51);
      Text := RGBToColor(0, 51, 153);
      OutLine := RGBToColor(214, 214, 214);
      Background := clWhite;
    end;
    Patterns.Indexed[0] := TCairoSolidPattern.Create(RGBToCairoColor(195, 208, 236));
    Cells.Add('Home');
    Cells.Add('Videos');
    Cells.Add('Channels');
    Cells.Add('Community');
    InitialSpace := 10;
    EndUpdate;
    Visible := True;
  end;
end;

end.

