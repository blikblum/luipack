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
  
  TRoundedRectInfo = record
    TopLeftRadius: Double;
    TopRightRadius: Double;
    BottomLeftRadius: Double;
    BottomRightRadius: Double;
    TopRadius: Double;
    LeftRadius: Double;
    RightRadius: Double;
    BottomRadius: Double;
  end;

function DegreeToRadian(const Angle: Double): Double;

procedure CalculateSharpBounds(ALeft, ATop, ARight, ABottom, LineWidth: Integer; out Bounds: TDoubleRect);

procedure CalculateSharpRect(X, Y, Width, Height, LineWidth: Integer; out Rect: TDoubleRect);

function GetSharpLineOffset(LineWidth: Integer): Double;

function RadianToDegree(const Angle: Double): Double;

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  Radius: Double);

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  const RectInfo: TRoundedRectInfo);

procedure RoundedRectangle(Context: TCairoContext; const R: TDoubleRect;
  Radius: Double);

procedure RoundedRectangle(Context: TCairoContext; const R: TDoubleRect;
  const RectInfo: TRoundedRectInfo);


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
  Radius: Double);
begin
  with Context do
  begin
    MoveTo(X, Y + Radius);
    CurveTo(X, Y + Radius,
      X, Y,
      X + Radius, Y);
    LineTo(X + Width - Radius, Y);
    CurveTo(X + Width - Radius, Y,
      X + Width, Y,
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

//Alternative implementation. Keep here for the record.
{
procedure RoundedRectangle2(Context: TCairoContext; X, Y, Width, Height: Double; RadiusX: Double = 5; RadiusY: Double = 5);
const
  ARC_TO_BEZIER = 0.55228475;
var
  c1, c2: Double;
begin
  with Context do
  begin
    //Todo: add newpath??
    //from mono moonlight aka mono silverlight
    //test limits (without using multiplications)
    //http://graphics.stanford.edu/courses/cs248-98-fall/Final/q1.html

    if RadiusX > Width - RadiusX then
        RadiusX := Width / 2;
    if RadiusY > Height - RadiusY then
        RadiusY := Height / 2;

    //approximate (quite close) the arc using a bezier curve
    c1 := ARC_TO_BEZIER * RadiusX;
    c2 := ARC_TO_BEZIER * RadiusY;

    NewPath;
    MoveTo(X + RadiusX, Y);

    RelLineTo(Width - 2 * RadiusX, 0.0);
    RelCurveTo(c1, 0.0, RadiusX, c2, RadiusX, RadiusY);
    RelLineTo(0, Height - 2 * RadiusY);
    RelCurveTo(0.0, c2, c1 - RadiusX, RadiusY, -RadiusX, RadiusY);
    RelLineTo(-Width + 2 * RadiusX, 0);
    RelCurveTo( -c1, 0, -RadiusX, -c2, -RadiusX, -RadiusY);
    RelLineTo(0, -Height + 2 * RadiusY);
    RelCurveTo(0.0, -c2, RadiusX - c1, -RadiusY, RadiusX, -RadiusY);
    ClosePath;
  end;
end;
}

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double;
  const RectInfo: TRoundedRectInfo);
begin
  with Context, RectInfo do
  begin
    MoveTo(X, Y + TopLeftRadius);

    CurveTo(X, Y + TopLeftRadius,
      X, Y,
      X + TopLeftRadius, Y);
      
    CurveTo(X + TopLeftRadius, Y,
       X + (Width / 2), Y - TopRadius,
       X + Width - TopRightRadius, Y);

    CurveTo(X + Width - TopRightRadius, Y,
      X + Width, Y,
      X + Width, Y + TopRightRadius);

    CurveTo(X + Width, Y + TopRightRadius,
      X + Width + RightRadius, Y + (Height / 2),
      X + Width, Y + Height - BottomRightRadius);

    CurveTo(X + Width, Y + Height - BottomRightRadius,
      X + Width, Y + Height,
      Width + X - BottomRightRadius, Y + Height);

    CurveTo(Width + X - BottomRightRadius, Y + Height,
      X + (Width / 2), Y + Height + BottomRadius,
      X + BottomLeftRadius, Y + Height);

    CurveTo(X + BottomLeftRadius, Y + Height,
      X, Y + Height,
      X, Y + Height - BottomLeftRadius);

    CurveTo(X, Y + Height - BottomLeftRadius,
      X - LeftRadius, Y + (Height / 2),
      X, Y + TopLeftRadius);
  end;
end;

procedure RoundedRectangle(Context: TCairoContext; const R: TDoubleRect;
  Radius: Double);
begin
  RoundedRectangle(Context, R.Left, R.Top, R.Right, R.Bottom, Radius);
end;

procedure RoundedRectangle(Context: TCairoContext; const R: TDoubleRect;
  const RectInfo: TRoundedRectInfo);
begin
  RoundedRectangle(Context, R.Left, R.Top, R.Right, R.Bottom, RectInfo);
end;


end.

