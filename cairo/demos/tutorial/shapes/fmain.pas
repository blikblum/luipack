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

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0, 0, 0);
    LineWidth := 1;

    Rectangle(20, 20, 120, 80);
    Rectangle(180, 20, 80, 80);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    SetSourceRgb(0, 0, 0);
    Arc(330, 60, 40, 0, 2 * PI);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    SetSourceRgb(0, 0, 0);
    Arc(90, 160, 40, PI/4, PI);
    ClosePath;
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    SetSourceRgb(0, 0, 0);
    Translate(220, 180);
    Scale(1, 0.7);
    Arc(0, 0, 50, 0, 2*PI);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

