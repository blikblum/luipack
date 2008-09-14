unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, CairoLCL, CairoClasses;

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
    FCount: Integer;
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
  ImageContext: TCairoContext;
  ImageWidth, ImageHeight: Integer;
  i, j: Integer;
begin
  ImageWidth := FImage.Width;
  ImageHeight := FImage.Height;
  Surface := TCairoImageSurface.Create(CAIRO_FORMAT_ARGB32, ImageWidth, ImageHeight);

  ImageContext := TCairoContext.Create(Surface);

  i := 0;
  while i <= ImageHeight do
  begin
    for j := 0 to FCount - 1 do
    begin
      ImageContext.MoveTo(0, i + j);
      ImageContext.LineTo(ImageWidth, i + j);
    end;
    Inc(i, 7);
  end;

  Inc(FCount);
  if FCount = 8 then
    Timer.Enabled := False;

  ImageContext.Stroke;


  CairoPaintBox.Context.SetSourceSurface(FImage, 10, 10);
  CairoPaintBox.Context.MaskSurface(Surface, 10, 10);

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

