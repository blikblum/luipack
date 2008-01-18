unit fmain;

{$mode objfpc}{$H+}

{Based in JanaGtk Clock widget}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL, CairoClasses, Math, cairo14;

type

  { TForm1 }

  TForm1 = class(TForm)
    CairoPaintBox1: TCairoPaintBox;
    procedure CairoPaintBox1Draw(Sender: TObject);
  private
    { private declarations }
    FDrawShadow: Boolean;
    procedure DrawAnalogueClock;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.CairoPaintBox1Draw(Sender: TObject);
begin

  {
  with CairoPaintBox1.Context do
  begin
    SetSourceRGB(1,1,1);
    Paint;
  end;
  }
  DrawAnalogueClock;
end;

procedure TForm1.DrawAnalogueClock;
var
  Pattern: TCairoRadialGradient;
  awidth, aheight, size, thickness, i, shadow_radius: Integer;
  base_color, bg_color, fg_color: array [0..2] of Double;
  TempColor: TColor;
begin
  with CairoPaintBox1.Context do
  begin
    (* Draw a Tango-style analogue clock face *)
    
    {
    base_color[0] = ((double)widget->style->bg[GTK_STATE_SELECTED].red)/
            (double)G_MAXUINT16;
    base_color[1] = ((double)widget->style->bg[GTK_STATE_SELECTED].green)/
            (double)G_MAXUINT16;
    base_color[2] = ((double)widget->style->bg[GTK_STATE_SELECTED].blue)/
            (double)G_MAXUINT16;
    }
    TempColor := ColorToRGB(clHighlight);
    base_color[0] := Red(TempColor)/255;
    base_color[1] := Green(TempColor)/255;
    base_color[2] := Blue(TempColor)/255;
    {
    bg_color[0] = ((double)widget->style->base[GTK_STATE_NORMAL].red)/
            (double)G_MAXUINT16;
    bg_color[1] = ((double)widget->style->base[GTK_STATE_NORMAL].green)/
            (double)G_MAXUINT16;
    bg_color[2] = ((double)widget->style->base[GTK_STATE_NORMAL].blue)/
            (double)G_MAXUINT16;
    }
    TempColor := ColorToRGB(clWindow);
    bg_color[0] := Red(TempColor)/255;
    bg_color[1] := Green(TempColor)/255;
    bg_color[2] := Blue(TempColor)/255;
    {
    fg_color[0] = ((double)widget->style->text[GTK_STATE_NORMAL].red)/
            (double)G_MAXUINT16;
    fg_color[1] = ((double)widget->style->text[GTK_STATE_NORMAL].green)/
            (double)G_MAXUINT16;
    fg_color[2] = ((double)widget->style->text[GTK_STATE_NORMAL].blue)/
            (double)G_MAXUINT16;
    }
    TempColor := ColorToRGB(clWindowText);
    fg_color[0] := Red(TempColor)/255;
    fg_color[1] := Green(TempColor)/255;
    fg_color[2] := Blue(TempColor)/255;

    awidth := CairoPaintBox1.Width;
    aheight := CairoPaintBox1.Height;
    if FDrawShadow then
      // aheight -= aheight/20;
      aheight := aheight - (aheight div 20);
    size := MIN (awidth, aheight);

    //SetOperator (CAIRO_OPERATOR_CLEAR);
    //Paint;
    //SetOperator (CAIRO_OPERATOR_SOURCE);

    (* Draw shadow *)
    if FDrawShadow then
    begin
      shadow_radius := MIN (awidth, aheight) - size;
      Translate(awidth/2, (aheight/2) + (size/2));
      Scale (size / (shadow_radius*2), 1.0);
      NewPath;
      Arc (0, 0, shadow_radius, 0, 2 * PI);
      ClosePath;
      Pattern := TCairoRadialGradient.Create(0, 0,
              0, 0, 0, shadow_radius);
      Pattern.AddColorStopRgba (0, 0, 0, 0, 0.5);
      Pattern.AddColorStopRgba (1, 0, 0, 0, 0);
      Source := Pattern;
      Fill;
      Pattern.Destroy;
      IdentityMatrix;
    end;

    (* Draw clock face *)
    thickness := size div 20;
    NewPath;
    Arc (awidth/2, aheight/2,
            size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    pattern := TCairoRadialGradient.Create (awidth/2, aheight/3,
            0, awidth/2, aheight/2,
            size/2 - thickness/2);
    Pattern.AddColorStopRgb (0, bg_color[0]*2,
            bg_color[1]*2, bg_color[2]*2);
    Pattern.AddColorStopRgb (0.3, bg_color[0],
            bg_color[1], bg_color[2]);
    Pattern.AddColorStopRgb (1, bg_color[0]/1.15,
            bg_color[1]/1.15, bg_color[2]/1.15);
    Source := Pattern;
    Fill;
    Pattern.Destroy;


    (* Draw tick marks *)
    SetSourceRGB (fg_color[0], fg_color[1], fg_color[2]);
    for i := 0 to 3 do
    begin
      NewPath;
      Arc ((awidth/2) + ((size/2 - thickness/2 - size/6) *
                      cos (i * PI/2)),
              (aheight/2) + ((size/2 - thickness/2 - size/6) *
                      sin (i * PI/2)),
              size/40, 0, 2 * PI);
      ClosePath;
      Fill;
    end;

    (* Draw centre point *)
    NewPath;
    Arc(awidth/2, aheight/2, size/35, 0, 2 * PI);
    ClosePath;
    LineWidth := size/60;
    Stroke;


    (* Draw internal clock-frame shadow *)
    thickness := size div 20;
    NewPath;
    Arc(awidth/2, aheight/2,
            size/2 - thickness, 0, 2 * PI);
    ClosePath;
    pattern := TCairoRadialGradient.Create ((awidth/2) - (size/4),
            (aheight/2) - (size/4),
            0, awidth/2, aheight/2,
            size/2 - thickness/2);
    Pattern.AddColorStopRgb (0, bg_color[0]/2,
            bg_color[1]/2, bg_color[2]/2);
    Pattern.AddColorStopRgb (0.5, bg_color[0]/2,
            bg_color[1]/2, bg_color[2]/2);
    Pattern.AddColorStopRgb ( 1, bg_color[0]*2,
            bg_color[1]*2, bg_color[2]*2);
    Source := pattern;
    LineWidth := thickness;
    Stroke;
    Pattern.Destroy;

    (* Draw internal clock-frame *)
    NewPath;
    Arc (awidth/2, aheight/2,
            size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    pattern := TCairoRadialGradient.Create ((awidth/2) - (size/3),
            (aheight/2) - (size/3), 0, awidth/3, aheight/3, size/2);
    Pattern.AddColorStopRgb ( 0, base_color[0]*1.2,
            base_color[1]*1.2, base_color[2]*1.2);
    Pattern.AddColorStopRgb ( 0.7, base_color[0],
            base_color[1], base_color[2]);
    Pattern.AddColorStopRgb ( 1, base_color[0]/1.2,
            base_color[1]/1.2, base_color[2]/1.2);
    Source := pattern;
    Stroke;
    Pattern.Destroy;

    (* Dark outline frame *)

    thickness := size div 60;
    NewPath;
    Arc (awidth/2, aheight/2,
            size/2 - thickness/2, 0, 2 * PI);
    ClosePath;
    SetSourceRGB (base_color[0]/2,
            base_color[1]/2, base_color[2]/2);
    LineWidth := thickness;
    Stroke ;

    (* Draw less dark inner outline frame *)
    thickness := size div 40;
    NewPath;
    Arc ( awidth/2, aheight/2, size/2 -
            (size/20)/2 - thickness, 0, 2 * PI);
    ClosePath ;
    SetSourceRGB ( base_color[0]/1.5,
            base_color[1]/1.5, base_color[2]/1.5);
    LineWidth := thickness;
    Stroke;
  end;
end;

initialization
  {$I fmain.lrs}

end.

