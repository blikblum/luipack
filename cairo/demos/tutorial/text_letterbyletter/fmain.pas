unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    Timer: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
    FCount: Integer;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  Cairo;

const
  TheText: array[0..6] of Char = ('Z', 'e', 't', 'C', 'o', 'd', 'e');

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  Extents: cairo_text_extents_t;
  i: Integer;
  x: Double;
begin
  with CairoPaintBox.Context do
  begin
    //The courier font is returning wrong extents at win32
    {$ifndef LCLWin32}
    SelectFontFace('Courier',
        CAIRO_FONT_SLANT_NORMAL,
        CAIRO_FONT_WEIGHT_BOLD);
    {$endif}
    FontSize := 35;
    SetSourceRgb(0.2, 0.2, 0.2);

    x := 0;

    for i := 0 to FCount - 1 do
    begin
      TextExtents(TheText[i], @Extents);
      x := x + Extents.width + 2;
      MoveTo(x + 30, 50);
      ShowText(TheText[i]);
    end;

    Inc(FCount);

    if (FCount = 8) then
    begin
      Timer.Enabled := False;
      FCount := 0;
    end;
  end;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

