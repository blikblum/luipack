unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    Label1: TLabel;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure CairoPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    CoordX, CoordY: array[0..100] of Double;
    Count: Integer;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  i, j: Integer;
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0, 0, 0);
    LineWidth := 0.5;

    for i := 0 to Count - 1 do
      for j := 0 to Count - 1 do
      begin
        MoveTo(CoordX[i], CoordY[i]);
        LineTo(CoordX[j], CoordY[j]);
      end;

    Count := 0;
    Stroke;
  end;
end;

procedure TFormMain.CairoPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
      begin
        CoordX[Count] := X;
        CoordY[Count] := Y;
        Inc(Count)
      end;
    mbRight:
      CairoPaintBox.Redraw;
  end;
end;

initialization
  {$I fmain.lrs}

end.

