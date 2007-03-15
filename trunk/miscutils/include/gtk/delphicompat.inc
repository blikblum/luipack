
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
var
 TempStr: UTF8String;
begin
  TempStr:=UTF8Encode(WideString(Str));
  Result:= ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr), Length(TempStr), Dx);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
var
  TempStr: UTF8String;
begin
  TempStr:=UTF8Encode(WideString(Str));
  TextOut(DC,X,Y,PChar(TempStr),Length(TempStr));
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr:=UTF8Encode(WideString(Str));
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr:=UTF8Encode(WideString(Str));
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
 TempStr: UTF8String;
begin
  TempStr:=UTF8Encode(WideString(lpString));
  Result:=DrawText(hDC,PChar(TempStr),Length(TempStr),lpRect,uFormat);
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
  Result:=False;
end;

function GetTextAlign(hDC:HDC): LongWord;
begin
  //todo
end;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord): BOOLEAN;
begin
  //todo
end;

function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
begin
  //todo
end;


function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
begin
  //todo
end;


function GdiFlush: Boolean;
begin
  //todo
end;


function GetWindowDC(hWnd:THandle):HDC;
begin
  //todo
end;


function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
begin
  //todo
end;


function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;
begin
  //todo
end;


function CreatePatternBrush(hbmp:HBITMAP):HBRUSH;
begin
  //todo
end;


function GetBkColor(DC:HDC):COLORREF;
begin
  //todo
end;


function GetDCEx(hWnd:THandle; hrgnClip:HRGN; flags:DWORD):HDC;
begin
  //todo
end;


function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
begin
  //todo
end;


function CopyImage(hImage:THANDLE; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;
begin
  //todo
end;


function SystemParametersInfo(uiAction, uiParam:LongWord; pvParam:Pointer; fWinIni:LongWord):Boolean;
begin
  //todo
end;


function GetKeyboardState(lpKeyState:PBYTE):BOOLEAN;
begin
  //todo
end;


function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState:PBYTE; lpChar:PWORD; uFlags:LongWord):longint;
begin
  //todo
end;


function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  //todo
end;


function BeginDeferWindowPos(nNumWindows:longint):THandle;
begin
  //todo
end;


function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;
begin
  //todo
end;


function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;
begin
  //todo
end;


function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;
begin
  //todo
end;


function SubtractRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): BOOLEAN;
begin
  //todo
end;


function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;
begin
  //todo
end;


function GetACP:LongWord;
begin
  //todo
end;


function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
begin
  //todo
end;


function GetKeyboardLayout(dwLayout:DWORD):THandle;
begin
  //todo
end;


function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
begin
  //todo
end;

function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;
begin
end;

function KillTimer(hWnd:THandle; nIDEvent: LongWord):Boolean;
begin
end;
