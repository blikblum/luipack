unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL, CairoClasses;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Surface1: TCairoImageSurface;
    Surface2: TCairoImageSurface;
    Surface3: TCairoImageSurface;
    Surface4: TCairoImageSurface;
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
  Pattern1, Pattern2, Pattern3, Pattern4: TCairoPattern;
begin
  with CairoPaintBox.Context do
  begin
    Pattern1 := TCairoSurfacePattern.Create(Surface1);
    Pattern2 := TCairoSurfacePattern.Create(Surface2);
    Pattern3 := TCairoSurfacePattern.Create(Surface3);
    Pattern4 := TCairoSurfacePattern.Create(Surface4);

    Source := Pattern1;
    Source.Extend := CAIRO_EXTEND_REPEAT;
    Rectangle(20, 20, 100, 100);
    Fill;

    Source := Pattern2;
    Source.Extend := CAIRO_EXTEND_REPEAT;
    Rectangle(150, 20, 100, 100);
    Fill;

    Source := Pattern3;
    Source.Extend := CAIRO_EXTEND_REPEAT;
    Rectangle(20, 140, 100, 100);
    Fill;

    Source := Pattern4;
    Source.Extend := CAIRO_EXTEND_REPEAT;
    Rectangle(150, 140, 100, 100);
    Fill;

    Pattern1.Destroy;
    Pattern2.Destroy;
    Pattern3.Destroy;
    Pattern4.Destroy;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Surface1 := TCairoImageSurface.Create('..'+PathDelim+'images'+PathDelim+'blueweb.png');
  Surface2 := TCairoImageSurface.Create('..'+PathDelim+'images'+PathDelim+'maple.png');
  Surface3 := TCairoImageSurface.Create('..'+PathDelim+'images'+PathDelim+'crack.png');
  Surface4 := TCairoImageSurface.Create('..'+PathDelim+'images'+PathDelim+'chocolate.png');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Surface1.Destroy;
  Surface2.Destroy;
  Surface3.Destroy;
  Surface4.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

