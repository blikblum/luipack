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

uses
  Cairo14;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgba(0, 0, 0, 1);
    LineWidth := 10;


    LineCap := CAIRO_LINE_CAP_BUTT;
    MoveTo(30, 50);
    LineTo(150, 50);
    Stroke;

    LineCap := CAIRO_LINE_CAP_ROUND;
    MoveTo(30, 90);
    LineTo(150, 90);
    Stroke;

    LineCap := CAIRO_LINE_CAP_SQUARE;
    MoveTo(30, 130);
    LineTo(150, 130);
    Stroke;

    LineWidth := 1.5;

    MoveTo(30, 40);
    LineTo(30, 140);
    Stroke;

    MoveTo(150, 40);
    LineTo(150, 140);
    Stroke;

    MoveTo(155, 40);
    LineTo(155, 140);
    Stroke;
  end;
end;

initialization
  {$I fmain.lrs}

end.

