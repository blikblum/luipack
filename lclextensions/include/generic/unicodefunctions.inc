
{$ifndef HAS_DRAWTEXTW}
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(lpString));
  Result := DrawText(hDC, PChar(TempStr),Length(TempStr), lpRect, uFormat);
end;
{$endif}

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr), Length(TempStr), Dx);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function GetTextExtentExPointW(DC: HDC; Str: PWideChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := DelphiCompat.GetTextExtentExPoint(DC, PChar(TempStr),
    Length(TempStr), MaxWidth, MaxCount, PartialWidths,Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := TextOut(DC,X,Y,PChar(TempStr),Length(TempStr));
end;
