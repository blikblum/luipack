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
    SetSourceRgb(0.5, 0.5, 1);
    Rectangle(20, 20, 100, 100);
    Fill;

    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(150, 20, 100, 100);
    Fill;

    SetSourceRgb(0, 0.3, 0);
    Rectangle(20, 140, 100, 100);
    Fill;

    SetSourceRgb(1, 0, 0.5);
    Rectangle(150, 140, 100, 100);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

