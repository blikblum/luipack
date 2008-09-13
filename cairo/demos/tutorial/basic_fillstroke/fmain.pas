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
  Math;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  w, h: Integer;
begin
  w := CairoPaintBox.Width;
  h := CairoPaintBox.Height;
  with CairoPaintBox.Context do
  begin
    LineWidth := 9;
    SetSourceRgb(0.69, 0.19, 0);
    Arc(w / 2, h / 2, IfThen(w < h, w, h) / 2 - 10, 0, 2 * PI);
    StrokePreserve;
    SetSourceRgb(0.3, 0.4, 0.6);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

