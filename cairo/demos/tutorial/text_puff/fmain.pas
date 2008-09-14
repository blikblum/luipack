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
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
    FAlpha: Double;
    FSize: Double;
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
var
  Extents: cairo_text_extents_t;
  x, y: Double;
begin
  x := CairoPaintBox.Width / 2;
  y := CairoPaintBox.Height / 2;
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(0.5, 0, 0);
    Paint;

    //The extents are wrong when using Courier font under win32
    {$ifndef LCLWin32}
    SelectFontFace('Courier',
        CAIRO_FONT_SLANT_NORMAL,
        CAIRO_FONT_WEIGHT_BOLD);
    {$endif}
    FSize := FSize + 0.8;

    if (FSize > 20) then
      FAlpha := FAlpha - 0.01;

    FontSize := FSize;

    SetSourceRgb(1, 1, 1);

    TextExtents('ZetCode', @extents);
    MoveTo(x - extents.width/2, y);
    TextPath('ZetCode');
    Clip;
    Stroke;
    PaintWithAlpha(FAlpha);

    if (FAlpha <= 0) then
      Timer.Enabled := False;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FAlpha := 1.0;
  FSize := 1;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

