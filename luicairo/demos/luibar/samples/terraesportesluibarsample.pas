unit TerraEsportesLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, CairoClasses, Graphics, Controls, LResources;

type

  { TTerraEsportesLuiBarSample }

  TTerraEsportesLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    procedure GetCellPattern(Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
      var Pattern: TCairoPattern);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTerraEsportesLuiBarSample }

procedure TTerraEsportesLuiBarSample.GetCellPattern(Sender: TLuiBar;
  Cell: TLuiBarCell; PatternType: TLuiBarPatternType; var Pattern: TCairoPattern);
begin
  if (PatternType = ptOutLine) and (Sender.SelectedIndex <> Cell.Index) and
    (Sender.HoverIndex <> Cell.Index) then
      Pattern := Sender.Patterns.Indexed[0];
  if (PatternType = ptText) and (Sender.HoverIndex = Cell.Index) then
    Pattern := Sender.Patterns.SelectedText;
end;

constructor TTerraEsportesLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 100);
    Options := [lboEmulateTab, lboHoverAsSelected, lboHotTrack];
    CellHeight := 30;
    CellRoundRadius := 6;
    Spacing := 5;
    OuterOffset := 40;
    CellAlign := caCenter;
    OutLineWidth := 1;
    InitialSpace := 10;
    OnGetCellPattern := @GetCellPattern;
    with Colors do
    begin
      Background := RGBToColor(27, 98, 165);
      Text := clWhite;
      Normal := RGBToColor(14, 54, 100);
      Selected := RGBToColor(220, 230, 234);
      SelectedText := RGBToColor(22, 117, 0);
      ClientArea := clWhite;
      OutLine := RGBToColor(165, 190, 199);
    end;
    Patterns.Indexed[0] := TCairoSolidPattern.Create(RGBToCairoColor(41, 120, 194));
    Cells.Add(UTF8Encode('1ª Fase'));
    Cells.Add(UTF8Encode('2ª Fase'));
    Cells.Add(UTF8Encode('3ª Fase'));
    Cells.Add('Fase Final');
    EndUpdate;
    Visible := True;
  end;
end;

initialization
  {$I xpstyleimages.lrs}

end.

