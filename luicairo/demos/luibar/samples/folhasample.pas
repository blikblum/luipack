unit FolhaSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, Graphics;

type

  { TCustomLuiBarSample }

  TFolhaLuiBarSample = class(TPage)
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TFolhaLuiBarSample }

constructor TFolhaLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  with TLuiBar.Create(Self) do
  begin
    Parent := Self;
    SetBounds(10, 10, 400, 40);
    Options := [lboEmulateTab];
    Spacing := 4;
    with Colors do
    begin
      Normal := RGBToColor(180, 206, 180);
      Hover := Normal;
      Selected := RGBToColor(51, 102, 51);
      ClientArea := Selected;
      SelectedText := clWhite;
      Text := clWhite;
    end;
    Cells.Add('Item1');
    Cells.Add('Item2');
    Cells.Add('Item3');
    Visible := True;
  end;
end;

end.

