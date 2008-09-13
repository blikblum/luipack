unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    TimerClip: TTimer;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerClipTimer(Sender: TObject);
  private
    { private declarations }
    FBigX: Integer;
    FBigY: Integer;
    FDelta: Integer;
    FCounter: Integer;
    FRotate: Double;
    FXDirection: Boolean;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  LCLIntf;
  
{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  w, h: Integer;
  ARect, BigRect, Intersect: TRect;
begin
  w := CairoPaintBox.Width;
  h := CairoPaintBox.Height;
  with CairoPaintBox.Context do
  begin
    Inc(FCounter);

    if FBigX > w then
    begin
      FXDirection := False;
      FDelta := -FDelta;
      FBigX := w;
    end;

    if FBigX < 1 then
    begin
      FBigX := 1;
      FDelta := -FDelta;
    end;

    if FBigY > h then
    begin
      FXDirection := True;
      FDelta := -FDelta;
      FBigY := h;
    end;

    if FBigY < 1 then
    begin
      FDelta := -FDelta;
      FBigY := 1;
    end;

    if FXDirection  then
      Inc(FBigX, FDelta)
    else
      Inc(FBigY, FDelta);

    Translate(w / 2, h / 2);

    Rectangle(-FBigX/2, -FBigY/2, FBigX-2, FBigY-2);
    SetSourceRgb(0, 0, 0);
    LineWidth := 1;
    Stroke;

    Rotate(FRotate);
    FRotate := FRotate + 0.01;

    Rectangle(-50, -25, 100, 50);
    Stroke;

    BigRect.Left := -FBigX div 2;
    BigRect.Top := -FBigY div 2;
    BigRect.Right := BigRect.Left + FBigX - 2;
    BigRect.Bottom := BigRect.Top + FBigY -2;

    ARect := Rect(-50, -25, 50, 25);

    IntersectRect(Intersect, ARect, BigRect);
    with Intersect do
      Rectangle(Left, Top, Right - Left, Bottom - Top);
    Fill;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FBigX := 20;
  FBigY := 200;
  FDelta := 1;
  FXDirection := True;
end;

procedure TFormMain.TimerClipTimer(Sender: TObject);
begin
  CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

