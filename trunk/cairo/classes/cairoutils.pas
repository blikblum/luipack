unit CairoUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, math;

function DegreeToRadian(const Angle: Double): Double;

function RadianToDegree(const Angle: Double): Double;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double; Radius: Double);

implementation

function DegreeToRadian(const Angle: Double): Double;
begin
  Result := Angle * (PI / 180);
end;

function RadianToDegree(const Angle: Double): Double;
begin
  Result := Angle * (180 / PI);
end;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double; Radius: Double);
begin
  with Context do
  begin
    MoveTo(X, Y + Radius);
    CurveTo(X, Y + Radius,
      X, Y,
      X + Radius, Y);
    LineTo(X + Width - Radius, Y);
    CurveTo(X + Width - Radius,
      Y, X + Width, Y,
      X + Width, Y + Radius);
    LineTo(X + Width, Y + Height - Radius);
    CurveTo(X + Width, Y + Height - Radius,
      X + Width, Y + Height,
      Width + X - Radius, Y + Height);
    LineTo(X + Radius, Y + Height);
    CurveTo(X + Radius, Y + Height,
      X, Y + Height,
      X, Y + Height - Radius);
    ClosePath;
  end;
end;


end.

