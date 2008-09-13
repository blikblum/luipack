unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    TimerWaiting: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure TimerWaitingTimer(Sender: TObject);
  private
    { private declarations }
    FCount: Integer;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  Cairo14;

const
  trs: array [0..7, 0..7] of Double = (
      ( 0.0, 0.15, 0.30, 0.5, 0.65, 0.80, 0.9, 1.0 ),
      ( 1.0, 0.0,  0.15, 0.30, 0.5, 0.65, 0.8, 0.9 ),
      ( 0.9, 1.0,  0.0,  0.15, 0.3, 0.5, 0.65, 0.8 ),
      ( 0.8, 0.9,  1.0,  0.0,  0.15, 0.3, 0.5, 0.65),
      ( 0.65, 0.8, 0.9,  1.0,  0.0,  0.15, 0.3, 0.5),
      ( 0.5, 0.65, 0.8, 0.9, 1.0,  0.0,  0.15, 0.3 ),
      ( 0.3, 0.5, 0.65, 0.8, 0.9, 1.0,  0.0,  0.15 ),
      ( 0.15, 0.3, 0.5, 0.65, 0.8, 0.9, 1.0,  0.0)
  );


{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  i: Integer;
begin
  with CairoPaintBox.Context do
  begin
    Translate(CairoPaintBox.Width / 2, CairoPaintBox.Height / 2);
    LineWidth := 3;
    LineCap := CAIRO_LINE_CAP_ROUND;
    for i := 0 to 7 do
    begin
      SetSourceRgba(0, 0, 0, trs[FCount mod 8][i]);
      MoveTo(0.0, -10.0);
      LineTo(0.0, -40.0);
      Rotate(PI / 4);
      Stroke;
    end;
  end;
end;

procedure TFormMain.TimerWaitingTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
  Inc(FCount);
end;

initialization
  {$I fmain.lrs}

end.

