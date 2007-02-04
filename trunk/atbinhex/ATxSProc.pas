unit ATxSProc;

{$mode delphi}
{$H+}

interface

uses
  Windows,SysUtils;

type
  TDecodeRec = record SFrom, STo: string; end;

procedure SReplace(var S: string; const SFrom, STo: string);
function SReplaceFunc(const S: string; const SFrom, STo: string): string;
procedure SReplaceW(var S: WideString; const SFrom, STo: WideString);
procedure SReplaceAll(var S: string; const SFrom, STo: string);
procedure SReplaceAllW(var S: WideString; const SFrom, STo: WideString);
procedure SReplaceIAll(var S: string; const SFrom, STo: string);
function SDecodeW(const S: WideString; const decode: array of TDecodeRec): WideString;
procedure SDecodeSearchW(var S: WideString);
procedure SReplaceZeros(var S: string);
procedure SReplaceZerosW(var S: WideString);
procedure SDelLastSpaceW(var S: WideString);
procedure SDelLastSlashW(var S: WideString);
procedure SDelLastComma(var S: string);
function SReplaceTabsW(const S: WideString; TabSize: word): WideString;
function SFormatW(const Msg: WideString; Params: array of WideString): WideString;
function SetStringW(Buffer: PChar; BufSize: integer; SwapBytes: boolean): WideString;

function SExtractFileDir(const FileName: WideString): WideString;
function SExtractFilePath(const FileName: WideString): WideString;
function SExtractFileExt(const FileName: WideString): WideString;
function SExtractFileName(const FileName: WideString): WideString;
function SExtMatch(const FileName: WideString; const ExtList: string): boolean;

function ToOEM(const S: string): string;
function ToANSI(const S: string): string;
procedure SBufferToOEM(Buffer: PChar; BufSize: integer);
procedure SBufferToANSI(Buffer: PChar; BufSize: integer);
function SCompareIW(const S1, S2: WideString): integer;
procedure SToLower(var S: string);
procedure SToLowerW(var S: WideString);

function SFindText(const F, S: string; fForward, fWholeWords, fCaseSens, fLastBlock: boolean): integer;
function SFindTextW(const F, S: WideString; fForward, fWholeWords, fCaseSens, fLastBlock: boolean): integer;

function IMin(n1, n2: integer): integer;
function IMax(n1, n2: integer): integer;
function WMin(n1, n2: word): word;
function WMax(n1, n2: word): word;
function I64Min(const n1, n2: Int64): Int64;
function I64Max(const n1, n2: Int64): Int64;

procedure ILimitMin(var N: integer; Value: integer);
procedure ILimitMax(var N: integer; Value: integer);
procedure WLimitMin(var N: word; Value: word);
procedure WLimitMax(var N: word; Value: word);

function SExpandVars(const S: string): string; //currently only ANSI version needed
function SExpanded(const S: string): boolean;
procedure SSetEnvVar(const Name, Value: WideString);


implementation

procedure SReplace(var S: string; const SFrom, STo: string);
var
  i: integer;
begin
  i:= Pos(SFrom, S);
  if i>0 then
    begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
    end;
end;

function SReplaceFunc(const S: string; const SFrom, STo: string): string;
begin
  Result:= S;
  SReplace(Result, SFrom, STo);
end;

procedure SReplaceW(var S: WideString; const SFrom, STo: WideString);
var
  i: integer;
begin
  i:= Pos(SFrom, S);
  if i>0 then
    begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
    end;
end;

procedure SReplaceAll(var S: string; const SFrom, STo: string);
var
  i: integer;
begin
  repeat
    i:= Pos(SFrom, S); 
    if i=0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until false;
end;

procedure SReplaceAllW(var S: WideString; const SFrom, STo: WideString);
var
  i: integer;
begin
  repeat
    i:= Pos(SFrom, S); 
    if i=0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until false;
end;

procedure SReplaceIAll(var S: string; const SFrom, STo: string);
var
  i: integer;
begin
  repeat
    i:= Pos(LowerCase(SFrom), LowerCase(S)); 
    if i=0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until false;
end;

function SDecodeW(const S: WideString; const decode: array of TDecodeRec): WideString;
var
  i, j: integer;
  DoDecode: boolean;
begin
  Result:= '';
  i:= 1;
  repeat
    if i>Length(S) then Break;
    DoDecode:= false;
    for j:= Low(decode) to High(decode) do
      with decode[j] do
        if SFrom=Copy(S, i, Length(SFrom)) then
          begin
          DoDecode:= true;
          Result:= Result+STo;
          Inc(i, Length(SFrom));
          Break
          end;
    if DoDecode then Continue;
    Result:= Result+S[i];
    Inc(i);
  until false;
end;

procedure SDecodeSearchW(var S: WideString);
const
  DecodeRec: array[1..5] of TDecodeRec =
    ((SFrom: '\n'; STo: #13),
     (SFrom: '\r'; STo: #10),
     (SFrom: '\t'; STo: #9),
     (SFrom: '\\'; STo: '\'),
     (SFrom: '\0'; STo: #0));
begin
  S:= SDecodeW(S, DecodeRec);
end;


function SFormatW(const Msg: WideString; Params: array of WideString): WideString;
var
  i: integer;
begin
  Result:= Msg;
  for i:= Low(Params) to High(Params) do
    SReplaceW(Result, '%s', Params[i]);
end;

function ToOEM(const S: string): string;
begin
  SetLength(Result, Length(S));
  CharToOemBuff(PChar(S), PChar(Result), Length(S));
end;

function ToANSI(const S: string): string;
begin
  SetLength(Result, Length(S));
  OemToCharBuff(PChar(S), PChar(Result), Length(S));
end;

procedure SBufferToOEM(Buffer: PChar; BufSize: integer);
begin
  CharToOemBuff(Buffer, Buffer, BufSize);
end;

procedure SBufferToANSI(Buffer: PChar; BufSize: integer);
begin
  OemToCharBuff(Buffer, Buffer, BufSize);
end;

function LastDelimiter(const Delimiters, S: WideString): Integer;
var
  i: integer;
begin
  for i:= Length(S) downto 1 do
    if Pos(S[i], Delimiters)>0 then
      begin Result:= i; Exit end;
  Result:= 0;
end;

function SExtractFileDir(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  if (I > 1) and (FileName[I] = '\') 
    //and (not (char(FileName[I - 1]) in ['\', ':'])) //was in SysUtils!
    then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function SExtractFilePath(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function SExtractFileExt(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function SExtractFileName(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function SCompareIW(const S1, S2: WideString): integer;
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT
    then Result:= lstrcmpiW(PWChar(S1), PWChar(S2))
    else Result:= lstrcmpiA(PChar(string(S1)), PChar(string(S2)));
end;

function SDefaultDelimiters: string;
const
  Chars = ':;<=>?' + '@[\]^' + '`{|}~';
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Ord('/') do
    Result:= Result+Chr(i);
  Result:= Result+Chars;
end;

procedure SToLower(var S: string);
begin
  if S<>'' then
    begin
    UniqueString(S);
    CharLowerBuff(@S[1], Length(S));
    end;
end;

procedure SToLowerW(var S: WideString);
var
  i: integer;
begin
  if S<>'' then
    begin
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      begin
      CharLowerBuffW(@S[1], Length(S));
      end
    else
      begin
      //Need to check this code with Unicode strings under Win9x
      for i:= 1 to Length(S) do
        if S[i]<=#$FF then
          S[i]:= WideChar(AnsiLowerCase(char(S[i]))[1]);
      end;
    end;
end;

function SEquals(Buf1, Buf2: PChar; BufSize: integer): boolean;
var
  i: integer;
begin
  Result:= true;
  for i:= 0 to BufSize-1 do
    if Buf1[i]<>Buf2[i] then
      begin Result:= false; Break end;
end;

function SEqualsW(Buf1, Buf2: PWChar; BufSize: integer): boolean;
var
  i: integer;
begin
  Result:= true;
  for i:= 0 to BufSize-1 do
    if Buf1[i]<>Buf2[i] then
      begin Result:= false; Break end;
end;

function SFindText(const F, S: string; fForward, fWholeWords, fCaseSens, fLastBlock: boolean): integer;
var
  SBuf, FBuf, Delimiters: string;
  Match: boolean;
  LastPos, i: integer;
begin
  Result:= 0;
  if (S='') or (F='') then Exit;

  Delimiters:= SDefaultDelimiters;

  SBuf:= S;
  FBuf:= F;
  if not fCaseSens then
    begin
    SToLower(SBuf);
    SToLower(FBuf);
    end;

  LastPos:= Length(S)-Length(F)+1;

  if fForward then
    //Search forward
    for i:= 1 to LastPos do
      begin
      Match:= SEquals(@FBuf[1], @SBuf[i], Length(FBuf));

      if fWholeWords then
        Match:= Match
          and (fLastBlock or (i<LastPos))
          and ((i<=1) or (Pos(S[i-1], Delimiters)>0))
          and ((i>=LastPos) or (Pos(S[i+Length(F)], Delimiters)>0));

      if Match then
        begin
        Result:= i;
        Break
        end;
      end
    else
    //Search backward
    for i:= LastPos downto 1 do
      begin
      Match:= SEquals(@FBuf[1], @SBuf[i], Length(FBuf));

      if fWholeWords then
        Match:= Match
          and (fLastBlock or (i>1))
          and ((i<=1) or (Pos(S[i-1], Delimiters)>0))
          and ((i>=LastPos) or (Pos(S[i+Length(F)], Delimiters)>0));

      if Match then
        begin
        Result:= i;
        Break
        end;
      end;
end;

function SFindTextW(const F, S: WideString; fForward, fWholeWords, fCaseSens, fLastBlock: boolean): integer;
var
  SBuf, FBuf, Delimiters: WideString;
  Match: boolean;
  LastPos, i: integer;
begin
  Result:= 0;
  if (S='') or (F='') then Exit;

  Delimiters:= SDefaultDelimiters;

  SBuf:= S;
  FBuf:= F;
  if not fCaseSens then
    begin
    SToLowerW(SBuf);
    SToLowerW(FBuf);
    end;

  LastPos:= Length(S)-Length(F)+1;

  if fForward then
    //Search forward
    for i:= 1 to LastPos do
      begin
      Match:= SEqualsW(@FBuf[1], @SBuf[i], Length(FBuf));

      if fWholeWords then
        Match:= Match
          and (fLastBlock or (i<LastPos))
          and ((i<=1) or (Pos(S[i-1], Delimiters)>0))
          and ((i>=LastPos) or (Pos(S[i+Length(F)], Delimiters)>0));

      if Match then
        begin
        Result:= i;
        Break
        end;
      end
    else
    //Search backward
    for i:= LastPos downto 1 do
      begin
      Match:= SEqualsW(@FBuf[1], @SBuf[i], Length(FBuf));

      if fWholeWords then
        Match:= Match
          and (fLastBlock or (i>1))
          and ((i<=1) or (Pos(S[i-1], Delimiters)>0))
          and ((i>=LastPos) or (Pos(S[i+Length(F)], Delimiters)>0));

      if Match then
        begin
        Result:= i;
        Break
        end;
      end;
end;


function IMin(n1, n2: integer): integer;
begin
  if n1<n2 then Result:= n1 else Result:= n2;
end;

function IMax(n1, n2: integer): integer;
begin
  if n1>n2 then Result:= n1 else Result:= n2;
end;

function WMin(n1, n2: word): word;
begin
  if n1<n2 then Result:= n1 else Result:= n2;
end;

function WMax(n1, n2: word): word;
begin
  if n1>n2 then Result:= n1 else Result:= n2;
end;

function I64Min(const n1, n2: Int64): Int64;
begin
  if n1<n2 then Result:= n1 else Result:= n2;
end;

function I64Max(const n1, n2: Int64): Int64;
begin
  if n1>n2 then Result:= n1 else Result:= n2;
end;


procedure ILimitMin(var N: integer; Value: integer);
begin
  if N<Value then N:= Value;
end;

procedure ILimitMax(var N: integer; Value: integer);
begin
  if N>Value then N:= Value;
end;

procedure WLimitMin(var N: word; Value: word);
begin
  if N<Value then N:= Value;
end;

procedure WLimitMax(var N: word; Value: word);
begin
  if N>Value then N:= Value;
end;


function SExpandVars(const S: string): string;
var
  Buffer: array[0..4*1024-1] of char;
begin
  SetString(Result, Buffer, ExpandEnvironmentStrings(PChar(S), Buffer, SizeOf(Buffer))-1);
end;

procedure SSetEnvVar(const Name, Value: WideString);
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT
    then SetEnvironmentVariableW(PWChar(Name), PWChar(Value))
    else SetEnvironmentVariableA(PChar(string(Name)), PChar(string(Value)));
end;

procedure SReplaceZeros(var S: string);
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if S[i]=#0 then S[i]:= ' ';
end;

procedure SReplaceZerosW(var S: WideString);
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if S[i]=#0 then S[i]:= ' ';
end;

procedure SDelLastSpaceW(var S: WideString);
begin
  if (S<>'') and ((S[Length(S)]=' ') or (S[Length(S)]=#9)) then
    SetLength(S, Length(S)-1);
end;

procedure SDelLastSlashW(var S: WideString);
begin
  if (S<>'') and (S[Length(S)]='\') then
    SetLength(S, Length(S)-1);
end;

procedure SDelLastComma(var S: string);
begin
  if (S<>'') and (S[Length(S)]=',') then
    SetLength(S, Length(S)-1);
end;


function SFillW(ch: WideChar; Count: integer): WideString;
var
  i: integer;
begin
  SetLength(Result, Count);
  for i:= 1 to Length(Result) do
    Result[i]:= ch;
end;

function SReplaceTabsW(const S: WideString; TabSize: word): WideString;
begin
  Result:= S;
  SReplaceAllW(Result, #9, SFillW(' ', TabSize));
end;

function SExtMatch(const FileName: WideString; const ExtList: string): boolean;
var
  ext: string;
begin
  ext:= LowerCase(SExtractFileExt(FileName));
  if (ext<>'') and (ext[1]='.') then Delete(ext, 1, 1);
  Result:= Pos(','+ext+',', ','+ExtList+',')>0;
end;

function SExpanded(const S: string): boolean;
begin
  Result:= Pos('%', S)=0;
end;


function SetStringW(Buffer: PChar; BufSize: integer; SwapBytes: boolean): WideString;
var
  P: PChar;
  i, j: integer;
  ch: char;
begin
  Result:= '';
  if BufSize<2 then Exit;
  SetLength(Result, BufSize div 2);
  Move(Buffer^, Result[1], Length(Result)*2);
  if SwapBytes then
    begin
    P:= @Result[1];
    for i:= 1 to Length(Result) do
      begin
      j:= (i-1)*2;
      ch:= P[j];
      P[j]:= P[j+1];
      P[j+1]:= ch;
      end;
    end;
end;



end.
