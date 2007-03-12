
  
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

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer;
  var lpRect: TRect; uFormat: LongWord): Integer;
begin
  Result:=Windows.DrawTextW(hDC,lpString,nCount,lpRect,uFormat);
end;

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;
begin
  Result:=Windows.InvertRect(hDC, lprc);
end;

function GetTextAlign(hDC: HDC): LongWord;
begin
  Result:=Windows.GetTextAlign(hDC);
end;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord
  ): BOOLEAN;
begin
  Result:=Windows.DrawFrameControl(DC,Rect,uType,uState);
end;

function ScrollDC(hDC: HDC; dx: longint; dy: longint; var lprcScroll: TRECT;
  var lprcClip: TRECT; hrgnUpdate: HRGN; lprcUpdate: PRECT): Boolean;
begin
  Result:=Windows.ScrollDC(hDC,dx,dy,lprcScroll,lprcClip,hrgnUpdate,lprcUpdate);
end;

function OffsetRgn(hrgn: HRGN; nxOffset, nYOffset: longint): longint;
begin
  Result:=Windows.OffsetRgn(hrgn,nxOffset,nYOffset);
end;

function GdiFlush: Boolean;
begin
  Result:=Windows.GdiFlush;
end;

function GetWindowDC(hWnd: THandle): HDC;
begin
  Result:=Windows.GetWindowDC(hWnd);
end;

function RedrawWindow(hWnd: THandle; lprcUpdate: PRECT; hrgnUpdate: HRGN;
  flags: LongWord): BOOLEAN;
begin
  Result:=Windows.RedrawWindow(hWnd,lprcUpdate,hrgnUpdate,flags);
end;

function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;
begin
  Result:=Windows.LPToDP(DC,Points,Count);
end;

function CreatePatternBrush(hbmp: HBITMAP): HBRUSH;
begin
  Result:=Windows.CreatePatternBrush(hbmp);
end;

function GetBkColor(DC: HDC): LCLType.COLORREF;
begin
  Result:=Windows.GetBkColor(DC);
end;

function GetDCEx(hWnd: THandle; hrgnClip: HRGN; flags: DWORD): HDC;
begin
  Result:=Windows.GetDCEx(hWnd,hrgnClip,flags);
end;

function SetBrushOrgEx(DC: HDC; nXOrg, nYOrg: longint; lppt: PPOINT): Boolean;
begin
  Result:=Windows.SetBrushOrgEx(DC,nXOrg,nYOrg,lppt);
end;

function CopyImage(hImage: THANDLE; uType: LongWord; cxDesired,
  cyDesired: LongInt; fuFlags: LongWord): THandle;
begin
  Result:=Windows.CopyImage(hImage,uType,cxDesired,cyDesired,fuFlags);
end;

function SystemParametersInfo(uiAction, uiParam: LongWord; pvParam: Pointer;
  fWinIni: LongWord): Boolean;
begin
  Result:=Windows.SystemParametersInfo(uiAction,uiParam,pvParam,fWinIni);
end;

function GetKeyboardState(lpKeyState: PBYTE): BOOLEAN;
begin
  Result:=Windows.GetKeyboardState(lpKeyState);
end;

function ToAscii(uVirtKey, uScanCode: LongWord; lpKeyState: PBYTE;
  lpChar: PWORD; uFlags: LongWord): longint;
begin
  Result:=Windows.ToAscii(uVirtKey,uScanCode,lpKeyState,lpChar,uFlags);
end;

function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  Result:=Windows.ImageList_DragShowNolock(fShow);
end;

function BeginDeferWindowPos(nNumWindows: longint): THandle;
begin
  Result:=Windows.BeginDeferWindowPos(nNumWindows);
end;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter: THandle; x, y, cx,
  cy: longint; uFlags: LongWord): THandle;
begin
  Result:=Windows.DeferWindowPos(hWinPosInfo,hWnd,hWndInsertAfter,x,y,cx,cy,uFlags);
end;

function EndDeferWindowPos(hWinPosInfo: THandle): Boolean;
begin
  Result:=Windows.EndDeferWindowPos(hWinPosInfo);
end;

function ScrollWindow(hWnd: THandle; XAmount, YAmount: longint; lpRect: PRECT;
  lpClipRect: PRECT): Boolean;
begin
  Result:=Windows.ScrollWindow(hWnd,XAmount,YAmount,lpRect,lpClipRect);
end;

function SubtractRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect
  ): BOOLEAN;
begin
  Result:=Windows.SubtractRect(lprcDst,lprcSrc1,lprcSrc2);
end;

function GetLocaleInfo(Locale, LCType: LongWord; lpLCData: PChar;
  cchData: longint): longint;
begin
  Result:=Windows.GetLocaleInfo(Locale,LCType,lpLCData,cchData);
end;

function GetACP: LongWord;
begin
  Result:=Windows.GetACP;
end;

function MultiByteToWideChar(CodePage, dwFlags: DWORD; lpMultiByteStr: PChar;
  cchMultiByte: longint; lpWideCharStr: PWideChar; cchWideChar: longint
  ): longint;
begin
  Result:=Windows.MultiByteToWideChar(CodePage,dwFlags,lpMultiByteStr,cchMultiByte,lpWideCharStr,cchWideChar);
end;

function GetKeyboardLayout(dwLayout: DWORD): THandle;
begin
  Result:=Windows.GetKeyboardLayout(dwLayout);
end;

procedure TimerCallBack(Handle: hwnd; Msg : DWORD; idEvent: UINT; dwTime: DWORD); stdcall;
var
  TimerInfo: TTimerRecord;
begin
  DebugLn('Executing Timer to Handle %d - ID: %d',[Handle, idEvent]);
  if FTimerList.GetTimerInfo(Handle,idEvent,TimerInfo) then
  with TimerInfo do
  begin
    if Notify <> nil then
      Notify(idEvent)
    else
    begin
      if Control <> nil then
        LCLSendTimerMsg(Control,idEvent,0);
    end;
  end
  else
    DebugLn('Warning - No TimerInfo found for %d-%d',[Handle,idEvent]);
end;

function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;
var
  WinInfo: PWindowInfo;
begin
  DebugLn('Adding Timer to Handle %d - ID: %d',[hWnd,nIDEvent]);
  WinInfo:=GetWindowInfo(hWnd);
  FTimerList.Add(hWnd,nIDEvent,lpTimerFunc,WinInfo^.WinControl);
  //todo: see the best way to set result when handle is 0
  Result:=Windows.SetTimer(hWnd,nIDEvent,uElapse,@TimerCallBack);
  DebugLn('SetTimer result %d',[Result]);
end;

function KillTimer(hWnd:THandle; nIDEvent: LongWord):Boolean;
begin
  Result:=Windows.KillTimer(hWnd,nIDEvent);
end;
