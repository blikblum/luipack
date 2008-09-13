unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    procedure CairoPaintBoxDraw(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

const
  Points: array[0..10, 0..1] of integer = (
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
  i: Integer;
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0, 0, 0);
    LineWidth := 1;

    for i := 0 to 10 do
       LineTo(Points[i][0], Points[i][1]);


    ClosePath;
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    MoveTo(240, 40);
    LineTo(240, 160);
    LineTo(350, 160);
    ClosePath;

    SetSourceRgb(0, 0, 0);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    MoveTo(380, 40);
    LineTo(380, 160);
    LineTo(450, 160);
    CurveTo(440, 155, 380, 145, 380, 40);

    SetSourceRgb(0, 0, 0);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

