
  
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
begin
  Result:=Windows.ExtTextOutW(DC, X, Y, Options, Rect,Str, Count, Dx);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
begin
  Result:=Windows.TextOutW(DC,X,Y,Str,Count);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
begin
  Result:=Windows.GetTextExtentPointW(DC, Str, Count, Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
begin
  Result:=Windows.GetTextExtentPointW(DC, Str, Count, Size);
end;

function GetTextExtentExPoint(DC: HDC; p2: PChar; p3, p4: Integer; p5,
  p6: PInteger; var p7: TSize): BOOL;
begin
  Result:=Windows.GetTextExtentExPoint(DC, p2, p3, p4, p5, p6,  p7);
end;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; p5,
  p6: PInteger; var p7: TSize): BOOL;
begin
  Result:=Windows.GetTextExtentExPointW(DC, p2, p3, p4, p5, p6,  p7);
end;

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;
begin
  Result:=Windows.InvertRect(hDC, lprc);
end;


