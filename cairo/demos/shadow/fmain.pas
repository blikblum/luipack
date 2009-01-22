unit fMain;

//Pascal port of http://people.freedesktop.org/~joonas/shadow/shadow.c

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL, CairoClasses, Cairo;

type

  { TMainForm }

  TMainForm = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FLogoSurface: TCairoImageSurface;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

procedure PaintSurfaceWithDropShadow (Context: TCairoContext; SourceSurface: TCairoImageSurface;
  ShadowOffset: Integer; ShadowAlpha, TintAlpha: Double);
var
  ShadowSurface: TCairoSurface;
  Temp: TCairoContext;
  ShadowMask: TCairoSurfacePattern;
begin
  // A temporary surface the size of the source surface.
  ShadowSurface := TCairoSurface.Create (SourceSurface, CAIRO_CONTENT_COLOR_ALPHA,
    SourceSurface.Width, SourceSurface.Height);

  // Draw the shadow to the shadow surface
  Temp := TCairoContext.Create(ShadowSurface);
  if (TintAlpha < 1.0) then
  begin
    // Draw the shadow image with the desired transparency.
    Temp.SetSourceSurface(SourceSurface, 0,0);
    Temp.PaintWithAlpha(ShadowAlpha);
    // Darken the shadow by tinting it with black. The
    // TintAlpha determines how much black to place on top
    // of the shadow image.
    Temp.SetOperator(CAIRO_OPERATOR_ATOP);
    Temp.SetSourceRgba(0,0,0,TintAlpha);
    Temp.Paint;
  end
  else
  begin
    // Identical to the above when TintAlpha = 1.0, but
    // ostensibly faster.
    ShadowMask := TCairoSurfacePattern.Create(SourceSurface);
    Temp.SetSourceRgba (0,0,0, ShadowAlpha);
    Temp.Mask(ShadowMask);
    ShadowMask.Destroy;
  end;
  Temp.Destroy;

  // Paint the shadow surface to Context
  Context.Save;
  Context.Translate(ShadowOffset, ShadowOffset);
  Context.SetOperator(CAIRO_OPERATOR_OVER);
  Context.SetSourceSurface(ShadowSurface, 0, 0);
  Context.Paint;
  Context.Restore;

  ShadowSurface.Destroy;

  // Paint the image itself to Context.
  Context.SetSourceSurface (SourceSurface, 0,0);
  Context.Paint;
end;


procedure PaintBackgroundGrid(Context: TCairoContext);
const
  w = 15;
  h = 15;
var
  Grid: TCairoSurface;
  Tmp: TCairoContext;
  SourcePattern: TCairoSurfacePattern;
begin
  Grid := TCairoSurface.Create(Context.Target, CAIRO_CONTENT_COLOR_ALPHA, 2*w, 2*h);

  // Draw a 2 x 2 grey grid with boxes of size w x h
  Tmp := TCairoContext.Create(Grid);
  Tmp.SetSourceRgb(0.4,0.4,0.4);
  Tmp.Paint;
  Tmp.Rectangle(0, 0, w, h);
  Tmp.Rectangle (w, h, w, h);
  Tmp.SetSourceRgb (0.8,0.8,0.8);
  Tmp.Fill;
  Tmp.Destroy;

  // Fill Context with the grid repeating everywhere
  SourcePattern := TCairoSurfacePattern.Create(Grid);
  SourcePattern.Extend := CAIRO_EXTEND_REPEAT;
  Context.Source := SourcePattern;
  Context.Paint;
  SourcePattern.Destroy;

  Grid.Destroy;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FLogoSurface := TCairoImageSurface.Create('freedesktop.png');
end;

procedure TMainForm.CairoPaintBoxDraw(Sender: TObject);
const
  SHADOW_OFFSET = 10;
  X_STEPS = 5;
  Y_STEPS = 5;
var
  x, y, w, h: Integer;
  TintAlpha, ShadowAlpha: Double;
begin
  PaintBackgroundGrid(CairoPaintBox.Context);
  w := FLogoSurface.Width + SHADOW_OFFSET;
  h := FLogoSurface.Height + SHADOW_OFFSET;
  for y := 0 to Y_STEPS - 1 do
  begin
    TintAlpha := (y + 1.0) / Y_STEPS;
    for x := 0 to X_STEPS - 1 do
    begin
      ShadowAlpha := (x+1.0) / X_STEPS;
      CairoPaintBox.Context.Save;
      CairoPaintBox.Context.Translate(x * w, y * h);
      CairoPaintBox.Context.Rectangle(0, 0, w, h);
      CairoPaintBox.Context.Clip;

      PaintSurfaceWithDropShadow(CairoPaintBox.Context, FLogoSurface,
        SHADOW_OFFSET, ShadowAlpha, TintAlpha);

      CairoPaintBox.Context.Restore;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLogoSurface.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

