unit ConversaAfiadaSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics;

type

  { TConversaAfiadaLuiBarSample }

  TConversaAfiadaLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure CreatePattern(Sender: TLuiBar; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TConversaAfiadaLuiBarSample }

procedure TConversaAfiadaLuiBarSample.CreatePattern(Sender: TLuiBar;
  PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  case PatternType of
    ptNormal:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(251, 251, 253));
        AddColorStop(0.5, RGBToCairoColor(243, 245, 248));
        AddColorStop(0, RGBToCairoColor(235, 238, 243));
      end;
    end;
    ptSelected:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(251, 251, 253));
        AddColorStop(0.5, RGBToCairoColor(243, 245, 248));
        AddColorStop(0, RGBToCairoColor(235, 238, 243));
      end;
    end;
    ptHover:
    begin
      Pattern := TCairoLinearGradient.Create(0, 0, 0, Sender.CellHeight);
      with TCairoLinearGradient(Pattern) do
      begin
        AddColorStop(0, RGBToCairoColor(251, 251, 253));
        AddColorStop(0.5, RGBToCairoColor(243, 245, 248));
        AddColorStop(0, RGBToCairoColor(235, 238, 243));
      end;
    end;
  end;
end;

constructor TConversaAfiadaLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    Parent := Self;
    SetBounds(10, 10, 400, 80);
    Options := [lboVariableCellWidth];
    OnCreatePattern := @CreatePattern;
    OutLineWidth := 1;
    CellHeight := 24;
    OuterOffset := 20;
    CellRoundRadius := 4;
    CellAlign := caInvert;
    with Colors do
    begin
      SelectedText := RGBToColor(166, 28, 5);
      Text := RGBToColor(20, 42, 77);
      OutLine := RGBToColor(189, 193, 204);
      Background := clWhite;
    end;
    Cells.Add('Home');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add(UTF8Encode('Vídeos'));
    Cells.Add('RSS');
    InitialSpace := 10;
    Visible := True;
  end;
end;

end.

