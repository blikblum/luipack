unit YahooSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  { TYahooLuiBarSample }

  TYahooLuiBarSample = class(TPage)
  private
    procedure CreatePattern(Sender: TCustomLuiBar; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern);
    procedure GetCellPattern(Sender: TCustomLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

const
  HoverTextPatternId = 0;

{ TYahooLuiBarSample }

procedure TYahooLuiBarSample.CreatePattern(Sender: TCustomLuiBar; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal, ptHover:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, CairoColor(1, 1, 1, 1));
        AddColorStop(1, RGBToCairoColor(218, 229, 230));
      end;
    end;
    ptSelected:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(223, 228, 232));
        AddColorStopRgba(1, 1, 1, 1, 1);
      end;
    end;
  end;
end;

procedure TYahooLuiBarSample.GetCellPattern(Sender: TCustomLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern
  );
begin
  if (Sender.HoverIndex = Cell.Index) and (PatternType = ptText) then
    Pattern := Sender.Patterns.Indexed[HoverTextPatternId];
end;

constructor TYahooLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  with TLuiBar.Create(Self) do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 100);
    Options := [lboEmulateTab, lboOutLineClientArea];
    OnCreatePattern := @CreatePattern;
    OutLineWidth := 1;
    with Colors do
    begin
      ClientArea := clWhite;
      SelectedText := RGBToColor(204, 102, 51);
      Text := RGBToColor(24, 57, 124);
      OutLine := RGBToColor(119, 138, 152);
      Background := clWhite;
    end;
    Cells.Add('Small');
    Cells.Add('LongLongLongLong');
    Cells.Add('x');
    EndUpdate;
    Visible := True;
  end;
  
  with TLuiBar.Create(Self) do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 200, 400, 30);
    Options := [lboEmulateTab, lboOmitBaseLine, lboVariableCellWidth, lboHotTrack];
    OnGetCellPattern := @GetCellPattern;
    OutLineWidth := 1;
    CellRoundRadius := 4;
    CellHeight := 24;
    with Colors do
    begin
      Normal := RGBToColor(246, 246, 246);
      Selected := RGBToColor(61, 119, 203);
      ClientArea := Selected;
      Hover := RGBToColor(149, 179, 222);
      SelectedText := clWhite;
      Text := RGBToColor(0, 51, 153);
      OutLine := RGBToColor(193, 193, 193);
      Background := clWhite;
    end;
    Patterns[HoverTextPatternId] := TCairoSolidPattern.Create(1, 1, 1);
    Cells.Add('Small');
    Cells.Add('LongLongLongLong');
    Cells.Add('x');
    EndUpdate;
    Visible := True;
  end;
end;

end.

