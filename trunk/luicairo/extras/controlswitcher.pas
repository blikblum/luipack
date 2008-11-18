unit ControlSwitcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiBar, Controls;

type

  { TControlSwitcher }

  TControlSwitcher = class (TLuiBar)
  protected
    function DoSelecting(OldCell, NewCell: Integer): Boolean; override;
  public
    procedure AttachControl(const Title: String; Control: TControl);
  end;

implementation

{ TControlSwitcher }

function TControlSwitcher.DoSelecting(OldCell, NewCell: Integer): Boolean;
begin
  Result := inherited DoSelecting(OldCell, NewCell);
  if Result then
  begin
   if (NewCell <> -1) then
      TControl(Cells[NewCell].UserData).Visible := True;
   if (OldCell <> -1) then
      TControl(Cells[OldCell].UserData).Visible := False;
  end;
end;

procedure TControlSwitcher.AttachControl(const Title: String; Control: TControl);
var
  NewCell: TLuiBarCell;
begin
  NewCell := Cells.Add(Title);
  NewCell.UserData := Control;
end;

end.

