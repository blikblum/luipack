unit CairoUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses;

type

  TDoubleRect = record
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
  end;

function DegreeToRadian(const Angle: Double): Double;

procedure CalculateSharpBounds(ALeft, ATop, ARight, ABottom, LineWidth: Integer; out Bounds: TDoubleRect);

procedure CalculateSharpRect(X, Y, Width, Height, LineWidth: Integer; out Rect: TDoubleRect);

function GetSharpLineOffset(LineWidth: Integer): Double;

function RadianToDegree(const Angle: Double): Double;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  TopRadius: Double; BottomRadius: Double = -1);



implementation

function DegreeToRadian(const Angle: Double): Double;
begin
  Result := Angle * (PI / 180);
end;

function GetSharpLineOffset(LineWidth: Integer): Double;
begin
  if Odd(LineWidth) then
    Result := (LineWidth div 2) + 0.5
  else
    Result := LineWidth / 2;
end;

procedure CalculateSharpBounds(ALeft, ATop, ARight, ABottom,
  LineWidth: Integer; out Bounds: TDoubleRect);
var
  Offset: Double;
begin
  Offset := GetSharpLineOffset(LineWidth);
  with Bounds do
  begin
    Left := ALeft + Offset;
    Top := ATop + Offset;
    Right := ARight - Offset;
    Bottom := ABottom - Offset;
  end;
end;

procedure CalculateSharpRect(X, Y, Width, Height, LineWidth: Integer; out
  Rect: TDoubleRect);
var
  Offset: Double;
begin
  Offset := GetSharpLineOffset(LineWidth);
  with Rect do
  begin
    Left := X + Offset;
    Top := Y + Offset;
    Right := Width - (2 * Offset);
    Bottom := Height - (2 * Offset);
  end;
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

