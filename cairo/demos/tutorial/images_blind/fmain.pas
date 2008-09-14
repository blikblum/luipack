unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL, CairoClasses, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    Timer: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
    FImage: TCairoImageSurface;
    w, h: Integer;
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
  Surface: TCairoSurface;
  ImageWidth, ImageHeight: Integer;
  ImageContext: TCairoContext;
begin
  ImageWidth := FImage.Width;
  ImageHeight := FImage.Height;
  w := ImageWidth;

  Surface := TCairoImageSurface.Create(CAIRO_FORMAT_ARGB32, ImageWidth, ImageHeight);
  ImageContext := TCairoContext.Create(Surface);

  ImageContext.Rectangle(0, 0, w, h);
  ImageContext.Fill;

  Inc(h);
  if h = ImageHeight then
    Timer.Enabled := False;

  with CairoPaintBox.Context do
  begin
    SetSourceSurface(FImage, 10, 10);
    MaskSurface(Surface, 10, 10);
  end;
  Surface.Destroy;
  ImageContext.Destroy;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ImagePath: String;
begin
  ImagePath := '..' + PathDelim + 'images' + PathDelim + 'plaveckycastle.png';
  FImage := TCairoImageSurface.Create(ImagePath);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FImage.Destroy;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

