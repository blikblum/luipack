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
  CairoClasses;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  Pat1, Pat2, Pat3: TCairoLinearGradient;
  i, j, Count: Integer;
begin
  with CairoPaintBox.Context do
  begin
    Pat1 := TCairoLinearGradient.Create(0.0, 0.0,  350.0, 350.0);

    Count := 1;
    for j := 1 to 10 do
    begin
      if (Count Mod 2) = 1 then
        Pat1.AddColorStopRgb(j / 10, 0, 0, 0)
      else
        Pat1.AddColorStopRgb(j / 10, 1, 0, 0);
      Inc(Count);
    end;

    Rectangle(20, 20, 300, 100);
    Source := Pat1;
    Fill;

    Pat2 := TCairoLinearGradient.Create(0.0, 0.0,  350.0, 0.0);

    Count := 1;
    for i := 1 to 36 do
    begin
      if (Count Mod 2) = 1 then
        Pat2.AddColorStopRgb(0.025 + i * 0.025, 0, 0, 0)
      else
        Pat2.AddColorStopRgb(0.025 + i * 0.025, 0, 0, 1);
      Inc(Count);
    end;

    Rectangle(20, 140, 300, 100);
    Source := Pat2;
    Fill;

    Pat3 := TCairoLinearGradient.Create(20.0, 260.0, 20.0, 360.0);

    Pat3.AddColorStopRgb(0.1, 0, 0, 0);
    Pat3.AddColorStopRgb(0.5, 1, 1, 0);
    Pat3.AddColorStopRgb(0.9, 0, 0, 0);

    Rectangle(20, 260, 300, 100);
    Source := Pat3;
    Fill;

    Pat1.Destroy;
    Pat2.Destroy;
    Pat3.Destroy;
  end;
end;

initialization
  {$I fmain.lrs}

end.

