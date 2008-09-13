unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    Timer: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
    FScale: Double;
    FAngle: Double;
    FDelta: Double;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

const
  Points: array [0..10, 0..1] of integer = (
    ( 0, 85 ),
    ( 75, 75 ),
    ( 100, 10 ),
    ( 125, 75 ),
    ( 200, 85 ),
    ( 150, 125 ),
    ( 160, 190 ),
    ( 100, 150 ),
    ( 40, 190 ),
    ( 50, 125 ),
    ( 0, 85 )
  );


{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  w, h, i: Integer;
begin
  w := CairoPaintBox.Width;
  h := CairoPaintBox.Height;
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0, 0.44, 0.7);
    LineWidth := 1;

    Translate(w / 2, h / 2);
    Rotate(FAngle);
    Scale(FScale, FScale);

    for  i := 0 to 9 do
      LineTo(Points[i][0], Points[i][1]);

    ClosePath;
    Fill;

    if (FScale < 0.01) or (FScale > 0.99) then
      FDelta := -FDelta;

    FScale := FScale + FDelta;
    FAngle := FAngle + 0.01;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FAngle := 0;
  FScale := 1;
  FDelta := 0.01;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

