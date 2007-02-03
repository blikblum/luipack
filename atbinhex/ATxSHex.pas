unit ATxSHex;
{$mode delphi}
{$H+}
interface

//Conversion from hex encoded string (for example, '10 20 AA FF': 4 chars) to normal string.
//Hex string must contain 2*N hex digits. Spaces are ignored.
function SHexToNormal(const HexStr: string; var ResStr: string): boolean;

//Conversion of string to hex form, digits are separated with spaces.
function SToHex(const s: string): string;

//Conversion from hex to integer.
function HexToIntDef(const s: string; const Default: Int64): Int64;

implementation

uses
  Windows, SysUtils, ATxSProc;

function SHexDigitToInt(Hex: char; var Int: DWORD): boolean;
var
  ch: char;
begin
  Result:= true;
  Int:= 0;
  ch:= UpCase(Hex);
  case ch of
    '0'..'9': Int:= Ord(ch)-Ord('0');
    'A'..'F': Int:= Ord(ch)-Ord('A')+10;
    else Result:= false;
  end;
end;

function SHexWordToInt(const Hex: string; var Int: DWORD): boolean;
var
  Int1, Int2: DWORD;
begin
  Result:= false;
  if Length(Hex)=1 then
    Result:= SHexDigitToInt(Hex[1], Int)
  else
  if Length(Hex)=2 then
    begin
    Result:= SHexDigitToInt(Hex[1], Int1) and SHexDigitToInt(Hex[2], Int2);
    if Result then Int:= Int1*16+Int2;
    end;
end;


function SHexToNormal(const HexStr: string; var ResStr: string): boolean;
var
  s: string;
  value: DWORD;
  i: integer;
begin
  ResStr:= '';
  Result:= false;

  s:= HexStr;
  SReplaceAll(s, ' ', '');

  if Length(s) mod 2 > 0 then Exit;

  for i:= 1 to Length(s) div 2 do
    begin
    if not SHexWordToInt(s[2*i-1]+s[2*i], value) then Exit;
    ResStr:= ResStr+Chr(value);
    end;

  Result:= true;
end;

function SToHex(const s: string): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to Length(s) do
    Result:= Result+IntToHex(Ord(s[i]), 2)+' ';
  if Result<>'' then
    Delete(Result, Length(Result), 1);
end;

function HexToIntDef(const s: string; const Default: Int64): Int64;
var
  i: integer;
  N: DWORD;
begin
  Result:= 0;
  for i:= 1 to Length(s) do
    begin
    if not SHexDigitToInt(s[i], N) then
      begin Result:= Default; Exit end;
    Result:= Result*$10 + N;
    end;
end;

end.
