unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL;

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
    SetSourceRgb(0.1, 0.1, 0.1);

    SelectFontFace('Purisa',
        CAIRO_FONT_SLANT_NORMAL,
        CAIRO_FONT_WEIGHT_BOLD);

    FontSize := 13;

    MoveTo(20, 30);
    ShowText('Most relationships seem so transitory');
    MoveTo(20, 60);
    ShowText('They''re all good but not the permanent one');

    MoveTo(20, 120);
    ShowText('Who doesn''t long for someone to hold');

    MoveTo(20, 150);
    ShowText('Who knows how to love you without being told');
    MoveTo(20, 180);
    ShowText('Somebody tell me why I''m on my own');
    MoveTo(20, 210);
    ShowText('If there''s a soulmate for everyone');
  end;
end;

initialization
  {$I fmain.lrs}

end.

