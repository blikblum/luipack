unit fmain;

{
  Based in cairomm example:
  http://gtkmm.org/docs/gtkmm-2.4/docs/tutorial/html/sec-drawing-clock-example.html
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CairoControl1: TCairoControl;
    Timer1: TTimer;
    procedure CairoControl1Draw(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  cairo14;

const
  m_radius = 0.42;
  m_lineWidth = 0.05;

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CairoControl1.Redraw;
end;

procedure TForm1.CairoControl1Draw(Sender: TObject);
var
  i : Integer;
  Hour, Second, Minute, MSecond: Word;
  inset, HourAngle, MinuteAngle, SecondAngle: Double;
begin
  with CairoControl1, Context do
  begin
    // scale to unit square and translate (0, 0) to be (0.5, 0.5), i.e.
    // the center of the window
    Scale(Width, Height);
    Translate(0.5, 0.5);
    LineWidth := m_lineWidth;
    Save;
    SetSourceRGBA(0.337, 0.612, 0.117, 0.9);   // green
    Paint;
    Restore;
    Arc(0, 0, m_radius, 0, 2 * PI);
    Save;
    SetSourceRGBA(1.0, 1.0, 1.0, 0.8);
    FillPreserve;
    Restore;
    StrokePreserve;
    Clip;

    //clock ticks
    for i := 0 to 11 do
    begin
      inset := 0.05;
      Save;
      LineCap := CAIRO_LINE_CAP_ROUND;

      if (i div 3 <> 0) then
      begin
        inset := inset*0.8;
        LineWidth := 0.03;
      end;

      MoveTo( (m_radius - inset) * cos (i * PI / 6),
              (m_radius - inset) * sin (i * PI / 6));
      LineTo (m_radius * cos (i * PI / 6),
              m_radius * sin (i * PI / 6));
      Stroke;
      Restore; (* stack-pen-size *)
    end;

    // store the current time
    DecodeTime(Time, Hour, Minute, Second, MSecond);

    // compute the angles of the indicators of our clock
    MinuteAngle := Minute * PI / 30;
    HourAngle := hour * PI / 6;
    SecondAngle := Second * PI / 30;

    Save;
    LineCap := CAIRO_LINE_CAP_ROUND;

    // draw the seconds hand
    Save;
    LineWidth := m_lineWidth / 3;
    SetSourceRGBA(0.7, 0.7, 0.7, 0.8); // gray
    MoveTo(0, 0);
    LineTo(sin(SecondAngle) * (m_radius * 0.9),
            -cos(SecondAngle) * (m_radius * 0.9));
    Stroke;
    Restore;

    // draw the minutes hand
    SetSourceRGBA(0.117, 0.337, 0.612, 0.9);   // blue
    MoveTo(0, 0);
    LineTo(sin(MinuteAngle + SecondAngle / 60) * (m_radius * 0.8),
            -cos(MinuteAngle + SecondAngle / 60) * (m_radius * 0.8));
    Stroke;

    // draw the hours hand
    SetSourceRGBA(0.337, 0.612, 0.117, 0.9);   // green
    MoveTo(0, 0);
    LineTo(sin(HourAngle + MinuteAngle / 12.0) * (m_radius * 0.5),
            -cos(HourAngle + MinuteAngle / 12.0) * (m_radius * 0.5));
    Stroke;
    Restore;

    // draw a little dot in the middle
    Arc(0, 0, m_lineWidth / 3.0, 0, 2 * PI);
    Fill;
  end;
end;

initialization
  {$I fmain.lrs}

end.

