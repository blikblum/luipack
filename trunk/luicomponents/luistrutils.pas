unit LuiStrUtils; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function Capitalize(const U: UnicodeString; const ExcludeWords: array of UnicodeString): UnicodeString;

function Capitalize(const S: AnsiString; const ExcludeWords: array of UnicodeString): AnsiString;

implementation

function MatchWords(const U: UnicodeString; const A: array of UnicodeString): Boolean;
var
  i: Integer;
begin
  for i := Low(A) to High(A) do
    if A[i] = U then
      Exit(True);
  Result := False;
end;

function GetNextWord(const S: UnicodeString; var Index: Integer): UnicodeString;
var
  i,j,l: Integer;
  pc : punicodechar;
begin
  if Index > Length(S) then
  begin
    Result := '';
    Exit;
  end;

  l := length(s);
  i := Index;
  //find start of next word
  pc:=@s[Index];
  while i <= l do
  begin
    if pc^ <> ' ' then
    begin
      break;
    end;
    inc(pc);
    inc(i);
  end;

  //find end of the word
  j := i;
  while j <= l do
  begin
    if pc^ = ' ' then
    begin
      break;
    end;
    inc(pc);
    inc(j);
  end;

  Index := J + 1;
  SetLength(Result,j-i);
  If ((j-i)>0) then
    Move(S[i],Result[1],(j-i)*2);
end;

procedure UppercaseFirstChar(var U: UnicodeString);
var
  C: UnicodeString;
begin
  C := U[1];
  C := UpCase(C);
  Delete(U, 1, 1);
  Insert(C, U, 1);
end;

function Capitalize(const U: UnicodeString; const ExcludeWords: array of UnicodeString): UnicodeString;
var
  LN: Integer;
  WordIndex: Integer;
  NextWord, LowerStr: UnicodeString;
begin
  //todo: set result length once than copy the values
  Result := '';
  LowerStr := Trim(widestringmanager.LowerUnicodeStringProc(U));

  WordIndex := 1;
  NextWord := GetNextWord(LowerStr, WordIndex);
  LN := Length(NextWord);
  while LN > 0 do
  begin
    //Uppercase first letters
    if not MatchWords(NextWord, ExcludeWords) then
      UppercaseFirstChar(NextWord);
    Result := Result + NextWord;
    NextWord := GetNextWord(LowerStr, WordIndex);
    LN := Length(NextWord);
    if LN > 0 then
      Result := Result + ' ';
  end;
end;

function Capitalize(const S: AnsiString; const ExcludeWords: array of UnicodeString): AnsiString;
begin
  Result := UTF8Encode(Capitalize(UTF8Decode(S), ExcludeWords));
end;

end.

