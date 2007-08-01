function BeginDeferWindowPos(nNumWindows:longint):THandle;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function CopyImage(hImage:THANDLE; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
  //todo
end;

function GdiFlush: Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetACP:LongWord;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_GETBKCOLOR}
function GetBkColor(DC:HDC):COLORREF;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

{$ifndef HAS_GETCURRENTOBJECT}
function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function GetDCEx(hWnd:THandle; hrgnClip:HRGN; flags:DWORD):HDC;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetKeyboardLayout(dwLayout:DWORD):THandle;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetKeyboardState(lpKeyState:PBYTE):BOOLEAN;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetMapMode(DC: HDC): LongInt;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_GETTEXTEXTENTEXPOINT}
function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function GetTextAlign(hDC:HDC): LongWord;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetWindowDC(hWnd:THandle):HDC;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_INVERTRECT}
function InvertRect(DC: HDC; const lprc: TRect): Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function SetMapMode(DC: HDC; fnMapMode: LongInt): LongInt;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function SubtractRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): BOOLEAN;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState:PBYTE; lpChar:PWORD; uFlags:LongWord):longint;
begin
  //todo
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;



