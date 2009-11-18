unit UbuntuSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  TUbuntuLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure CreatePattern(Sender: TLuiBar; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TUbuntuLuiBarSample }

procedure TUbuntuLuiBarSample.CreatePattern(Sender: TLuiBar;
  PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(244, 238, 222));
        AddColorStop(0.5, RGBToCairoColor(230, 226, 210));
        AddColorStop(1, RGBToCairoColor(220, 215, 200));
      end;
    end;
    ptSelected:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(242, 239, 232));
        AddColorStop(0.4, RGBToCairoColor(251, 251, 249));
        AddColorStopRgb(1, 1, 1, 1);
      end;
    end;
    ptHover:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(255, 251, 237));
        AddColorStop(0.5, RGBToCairoColor(246, 242, 229));
        AddColorStop(1, RGBToCairoColor(231, 228, 215));
      end;
    end;
  end;
end;

constructor TUbuntuLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 80);
    Options := [lboEmulateTab, lboVariableCellWidth, lboHotTrack];
    OnCreatePattern := @CreatePattern;
    OutLineWidth := 1;
    Spacing := 1;
    CellHeight := 30;
    OuterOffset := 20;
    CellRoundRadius := 4;
    CellAlign := caInvert;
    with Colors do
    begin
      ClientArea := clWhite;
      SelectedText := RGBToColor(68, 68, 68);
      Text := SelectedText;
      OutLine := RGBToColor(176, 176, 158);
      Background := RGBToColor(219, 186, 117);
    end;
    Cells.Add('Small');
    Cells.Add('LongLongLongLong');
    Cells.Add('x');
    InitialSpace := 10;
    EndUpdate;
    Visible := True;
  end;
end;

end.

