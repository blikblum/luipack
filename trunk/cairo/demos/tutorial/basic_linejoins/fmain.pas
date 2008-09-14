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
  Cairo;
  
{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0.1, 0, 0);
    LineWidth := 14;
    
    Rectangle(30, 30, 100, 100);
    LineJoin :=  CAIRO_LINE_JOIN_MITER;
    Stroke;

    Rectangle(160, 30, 100, 100);
    LineJoin := CAIRO_LINE_JOIN_BEVEL;
    Stroke;

    Rectangle(100, 160, 100, 100);
    LineJoin := CAIRO_LINE_JOIN_ROUND;
    Stroke;
  end;
end;

initialization
  {$I fmain.lrs}

end.

