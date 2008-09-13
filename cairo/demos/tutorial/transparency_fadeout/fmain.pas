unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL, CairoClasses,
  ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    TimerFadeOut: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerFadeOutTimer(Sender: TObject);
  private
    { private declarations }
    FImage: TCairoImageSurface;
    FAlpha: Double;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

const
  Delta: Double = 0.01;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);

begin
  with CairoPaintBox.Context do
  begin
    SetSourceSurface(FImage, 10, 10);
    PaintWithAlpha(FAlpha);

    FAlpha := FAlpha - Delta;

    if FAlpha <= 0 then
      TimerFadeOut.Enabled := False;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FImage := TCairoImageSurface.Create('..'+PathDelim+'images'+PathDelim+'turnacastle.png');
  FAlpha := 1;
  TimerFadeOut.Enabled := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FImage.Destroy;
end;

procedure TFormMain.TimerFadeOutTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

