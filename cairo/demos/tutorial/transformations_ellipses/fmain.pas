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
var
  i, w, h: Integer;
begin
  w := CairoPaintBox.Width;
  h := CairoPaintBox.Height;
  with CairoPaintBox.Context do
  begin
    LineWidth := 0.5;
    Translate(w / 2, h / 2);
    Arc(0, 0, 120, 0, 2 * PI);
    Stroke;

    Save;
    for  i := 0 to 35 do
    begin
      Rotate(i * PI / 36);
      Scale(0.3, 1);
      Arc(0, 0, 120, 0, 2 * PI);
      Restore;
      Stroke;
      Save;
    end;
  end;
end;

initialization
  {$I fmain.lrs}

end.

