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
const
  Dashed1: array[0..1] of Double = (4.0, 1.0);
  Dashed2: array[0..2] of Double = (4.0, 1.0, 4.0);
  Dashed3: array[0..0] of Double = (1.0);
  Len1 = 2;
  Len2 = 3;

begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgba(0, 0, 0, 1);
    LineWidth := 1.5;

    SetDash(Dashed1, Len1, 0);
    MoveTo(40, 30);
    LineTo(200, 30);
    Stroke;

    SetDash(Dashed2, Len2, 1);
    MoveTo(40, 50);
    LineTo(200, 50);
    Stroke;

    SetDash(Dashed3, 1, 0);
    MoveTo(40, 70);
    LineTo(200, 70);
    Stroke;
  end;
end;

initialization
  {$I fmain.lrs}

end.

