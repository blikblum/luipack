                                            function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
var
 TempStr: String;
begin
  TempStr:=WideCharToString(Str);
  Result:= ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr), Length(TempStr), Dx);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
var
 TempStr: String;
begin
  TempStr:=WideCharToString(Str);
  TextOut(DC,X,Y,PChar(TempStr),Length(TempStr));
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
 TempStr: String;
begin
  TempStr:=WideCharToString(Str);
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
 TempStr: String;
begin
  TempStr:=WideCharToString(Str);
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function GetTextExtentExPoint(DC: HDC; p2: PChar; p3, p4: Integer; p5,
  p6: PInteger; var p7: TSize): BOOL;
begin
  {$INFO Implement GetTextExtentExPoint}
end;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; p5,
  p6: PInteger; var p7: TSize): BOOL;
begin

end;

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;
begin
  //todo
end;
