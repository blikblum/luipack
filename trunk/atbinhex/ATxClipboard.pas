unit ATxClipboard;

interface

function SClearClipboard: boolean;
function SCopyToClipboard(const S: AnsiString; IsOEM: boolean = false): boolean;
function SCopyToClipboardW(const S: WideString): boolean;


implementation

uses
  Windows, SysUtils, ATxSProc;

function SClearClipboard: boolean;
begin
  if not OpenClipboard(0) then begin Result:= false; Exit end;
  EmptyClipboard;
  Result:= true;
end;

//CF_UNICODETEXT format is supported only under NT
function SCopyToClipboardW_NT(const S: WideString; DoClear: boolean): boolean;
var
  DataSize, BufferSize: integer;
  hData: HGLOBAL;
  gData: pointer;
begin
  DataSize:= Length(S)*2;
  if DataSize>0 then
    begin
    if DoClear then
      begin
      if not OpenClipboard(0) then begin Result:= false; Exit end;
      EmptyClipboard;
      end;

    BufferSize:= DataSize+2;
    hData:= GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, BufferSize);
    if hData<>0 then
      begin
      gData:= GlobalLock(hData);
      if gData<>nil then
        begin
        Move(S[1], gData^, BufferSize);
        GlobalUnlock(hData); 
        SetClipboardData(CF_UNICODETEXT, hData); 
        end;
      end;

    if DoClear then
      CloseClipboard;
    end;
  Result:= true;
end;

function SCopyToClipboard(const S: AnsiString; IsOEM: boolean = false): boolean;
const
  Formats: array[boolean] of integer = (CF_TEXT, CF_OEMTEXT);
var
  DataSize, BufferSize: integer;
  hData: HGLOBAL;
  gData: pointer;
begin
  DataSize:= Length(S);
  if DataSize>0 then
    begin
    if not OpenClipboard(0) then begin Result:= false; Exit end;
    EmptyClipboard;

    BufferSize:= DataSize+1;
    hData:= GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, BufferSize);
    if hData<>0 then
      begin
      gData:= GlobalLock(hData);
      if gData<>nil then
        begin
        Move(S[1], gData^, BufferSize);
        GlobalUnlock(hData); 
        SetClipboardData(Formats[IsOEM], hData); 
        end;
      end;

    //Also copy in CF_UNICODETEXT format for compatability with Windows controls
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      if IsOEM
        then SCopyToClipboardW_NT(WideString(ToANSI(S)), false)
        else SCopyToClipboardW_NT(WideString(S), false);

    CloseClipboard; 
    end;

  Result:= true;
end;


function SCopyToClipboardW(const S: WideString): boolean;
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT
    then Result:= SCopyToClipboardW_NT(S, true)
    else Result:= SCopyToClipboard(AnsiString(S));;
end;


end.
