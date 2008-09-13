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
    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(20, 20, 80, 50);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;

    Translate(150, 100);
    Rotate(PI / 2);

    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(20, 20, 80, 50);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

