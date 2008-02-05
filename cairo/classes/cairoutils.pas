unit CairoUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, math;

function DegreeToRadian(const Angle: Double): Double;

function RadianToDegree(const Angle: Double): Double;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  TopRadius: Double; BottomRadius: Double = -1);

implementation

function DegreeToRadian(const Angle: Double): Double;
begin
  Result := Angle * (PI / 180);
end;

function RadianToDegree(const Angle: Double): Double;
begin
  Result := Angle * (180 / PI);
end;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  TopRadius: Double; BottomRadius: Double = -1);
begin
  if BottomRadius < 0 then
    BottomRadius := TopRadius;
  with Context do
  begin
    MoveTo(X, Y + TopRadius);
    CurveTo(X, Y + TopRadius,
      X, Y,
      X + TopRadius, Y);
    LineTo(X + Width - TopRadius, Y);
    CurveTo(X + Width - TopRadius,
      Y, X + Width, Y,
      X + Width, Y + TopRadius);
    LineTo(X + Width, Y + Height - BottomRadius);
    CurveTo(X + Width, Y + Height - BottomRadius,
      X + Width, Y + Height,
      Width + X - BottomRadius, Y + Height);
    LineTo(X + BottomRadius, Y + Height);
    CurveTo(X + BottomRadius, Y + Height,
      X, Y + Height,
      X, Y + Height - BottomRadius);
    ClosePath;
  end;
end;


end.

