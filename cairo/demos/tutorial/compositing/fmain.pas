unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL;

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
  Cairo, CairoClasses;

const
  oper: array[0..5] of cairo_operator_t = (
    CAIRO_OPERATOR_DEST_OVER,
    CAIRO_OPERATOR_DEST_IN,
    CAIRO_OPERATOR_OUT,
    CAIRO_OPERATOR_ADD,
    CAIRO_OPERATOR_ATOP,
    CAIRO_OPERATOR_DEST_ATOP
  );
  
  
procedure DrawComposited(Context: TCairoContext; x, w, h: Integer;
  op: cairo_operator_t);
var
  FirstContext, SecondContext: TCairoContext;
  FirstSurface, SecondSurface: TCairoSurface;
begin
  FirstSurface := TCairoSurface.Create(Context.Target,
    CAIRO_CONTENT_COLOR_ALPHA, w, h);

  SecondSurface := TCairoSurface.Create(Context.Target,
    CAIRO_CONTENT_COLOR_ALPHA, w, h);

  FirstContext := TCairoContext.Create(FirstSurface);
  FirstContext.SetSourceRgb(0, 0, 0.4);
  FirstContext.Rectangle(x, 20, 50, 50);
  FirstContext.Fill;

  SecondContext := TCairoContext.Create(SecondSurface);
  SecondContext.SetSourceRgb(0.5, 0.5, 0);
  SecondContext.Rectangle(x + 10, 40, 50, 50);
  SecondContext.Fill;

  FirstContext.SetOperator(op);
  FirstContext.SetSourceSurface(SecondSurface, 0, 0);
  FirstContext.Paint;

  Context.SetSourceSurface(FirstSurface, 0, 0);
  Context.Paint;

  FirstSurface.Destroy;
  SecondSurface.Destroy;

  FirstContext.Destroy;
  SecondContext.Destroy;
end;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  i, w, h, x: Integer;
begin
  with CairoPaintBox do
  begin
    w := Width;
    h := Height;
    x := 20;
    for i:= 0 to 5 do
    begin
      DrawComposited(Context, x, w, h, Oper[i]);
      Inc(x, 80);
    end;
  end;
end;

initialization
  {$I fmain.lrs}

end.

