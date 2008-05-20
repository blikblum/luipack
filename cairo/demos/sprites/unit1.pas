unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL, CairoClasses, cairo14, AvgLvlTree,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CairoControl1: TCairoPaintBox;
    Timer1: TTimer;
    procedure CairoControl1Draw(Sender: TObject);
    procedure CairoControl1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure CreateStretchedSurface(BaseSurface: TCairoSurface; AWidth, AHeight: Integer);
  public
    { public declarations }
    FBackgroundSurface: TCairoImageSurface;
    FSpriteSurface: TCairoImageSurface;
    FStretchedSurface: TCairoSurface;
  end; 

var
  Form1: TForm1; 

implementation

const
  UseBuffer = True;

{ TForm1 }

procedure TForm1.CairoControl1Draw(Sender: TObject);
var
  t: Double;
  x: Int64;
  y: Integer;
  CenterX: Integer;
  CenterY: Integer;
  Matrix: TCairoMatrix;
begin
  with CairoControl1, Context do
  begin
    //Paint the background
    if UseBuffer then
    begin
      //Create a stretched surface to be used as a buffer
      if FStretchedSurface = nil then
        CreateStretchedSurface(Target, Width, Height);

      //Set the buffer as the source
      SetSourceSurface(FStretchedSurface, 0, 0);

      Paint;
    end
    else
    begin
      Save;
      //Init a matrix with the scale factors we need
      Matrix.InitScale(Width / FBackgroundSurface.Width,
        Height / FBackgroundSurface.Height);

      //Is necessary to set the matrix to context before setting the source
      SetMatrix(Matrix);

      //Set the source surface from where get the data
      SetSourceSurface(FBackgroundSurface, 0, 0);

      Paint;
      //This will undo the matrix transformation
      Restore;
    end;

    
    // Paint the sprite
    CenterX := Width div 2;
    CenterY := Height div 2;
    t := Now * 86400;
    x := CenterX + round(cos(t)*CenterX*2/3) - (FSpriteSurface.Width div 2);
    y := CenterY + round(sin(t*0.7)*CenterY*2/3) - (FSpriteSurface.Height div 2);
    
    SetSourceSurface(FSpriteSurface, x, y);
    Paint;
  end;
end;

procedure TForm1.CairoControl1Resize(Sender: TObject);
begin
  FStretchedSurface.Free;
  FStretchedSurface := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBackgroundSurface := TCairoImageSurface.Create('splash_logo.png');
  FSpriteSurface := TCairoImageSurface.Create('ide_icon48x48.png');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBackgroundSurface.Destroy;
  FSpriteSurface.Destroy;
  FStretchedSurface.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CairoControl1.Redraw;
end;

procedure TForm1.CreateStretchedSurface(BaseSurface: TCairoSurface; AWidth, AHeight: Integer);
var
  Context: TCairoContext;
  Matrix: TCairoMatrix;
begin
  //create a surface similar to BaseSurface. This speeds paint operation between surfaces
  FStretchedSurface := TCairoSurface.Create(BaseSurface, CAIRO_CONTENT_COLOR, AWidth, AHeight);

  //create a context for the newly created surface
  Context := TCairoContext.Create(FStretchedSurface);
  
  //Init a matrix with the scale factors we need
  Matrix.InitScale(AWidth / FBackgroundSurface.Width,
    AHeight / FBackgroundSurface.Height);

  //Is necessary to set the matrix to context before setting the source
  Context.SetMatrix(Matrix);
  
  //Set the source surface from where get the data
  Context.SetSourceSurface(FBackgroundSurface, 0, 0);
  
  //Paint from Source (FBackgroundSurface) to Target (FStretchedSurface)
  //  using the Current Transformation Matrix (Matrix)
  Context.Paint;
  Context.Destroy;
end;

initialization
  {$I unit1.lrs}

end.

