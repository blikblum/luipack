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
    TimerClip: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerClipTimer(Sender: TObject);
  private
    { private declarations }
    FImage: TCairoImageSurface;
    FPosX: Integer;
    FPosY: Integer;
    FDelta: array [0..1] of Integer;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  w, h: Integer;
const
  Radius = 40;
begin
  w := CairoPaintBox.Width;
  h := CairoPaintBox.Height;
  with CairoPaintBox.Context do
  begin
    if (FPosX < Radius) then
      FDelta[0] := Trunc(Random) Mod 4 + 5
    else if (FPosX > (w - Radius)) then
      FDelta[0] := -(Trunc(Random) Mod 4 + 5);

    if (FPosY < Radius) then
      FDelta[1] := Trunc(Random) Mod 4 + 5
    else if (FPosY > (h - Radius)) then
      FDelta[1] := -(Trunc(Random) Mod 4 + 5);

    Inc(FPosX, FDelta[0]);
    Inc(FPosY, FDelta[1]);

    SetSourceSurface(FImage, 1, 1);
    Arc(FPosX, FPosY, Radius, 0, 2 * PI);
    Clip;
    Paint;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FImage := TCairoImageSurface.Create('..'+ PathDelim+'images'+PathDelim+'turnacastle.png');
  FPosX := 128;
  FPosY := 128;
  FDelta[0] := 3;
  FDelta[1] := 3;
  Randomize;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FImage.Destroy;
end;

procedure TFormMain.TimerClipTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

