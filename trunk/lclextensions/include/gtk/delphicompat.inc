
  
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
var
 TempStr: UTF8String;
begin
  Logger.Send('ExtTextOutW');
  TempStr:=UTF8Encode(WideString(Str));
  Result:= ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr), Length(TempStr), Dx);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
var
  TempStr: UTF8String;
begin
  Logger.Send('TextOutW');
  TempStr:=UTF8Encode(WideString(Str));
  TextOut(DC,X,Y,PChar(TempStr),Length(TempStr));
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  Logger.Send('GetTextExtentPoint32W');
  TempStr:=UTF8Encode(WideString(Str));
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  Logger.Send('GetTextExtentPointW');
  TempStr:=UTF8Encode(WideString(Str));
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
  TempStr: UTF8String;
  TempRect: TRect;
  TextHeight: Integer;
  TM: TTextMetric;
begin
  Logger.Send('DrawTextW');
  TempRect:=lpRect;
  //fix position under gtk
  if (uFormat and DT_VCENTER) > 0 then
  begin
    GetTextMetrics(hDC, TM);
    //gtk overestimate height
    TextHeight := TM.tmHeight - 2;
    TempRect.Top:= (TempRect.Top + TempRect.Bottom - TextHeight) div 2;
  end;
  TempStr:=UTF8Encode(WideString(lpString));
  Result:=DrawText(hDC,PChar(TempStr),Length(TempStr),TempRect,uFormat);
  Logger.Send('Rect',TempRect);
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  lbearing, rbearing, width, ascent,descent: LongInt;
  UseFont : PGDKFont;
  IsDBCSFont: Boolean;
  NewCount,Accumulator,i: Integer;
begin
  //based in lcl code
  Result := GTKWidgetSet.IsValidDC(DC);
  if Result then
  with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GTKWidgetSet.GetDefaultGtkFont(false);
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
    end;
    If UseFont = nil then
      DebugLn('WARNING: [TGtkWidgetSet.GetTextExtentPoint] Missing font')
    else begin
      descent:=0;
      {
      UpdateDCTextMetric(TDeviceContext(DC));
      IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;

      if IsDBCSFont then begin
        NewCount:=Count*2;
        if FExtUTF8OutCacheSize<NewCount then begin
          ReAllocMem(FExtUTF8OutCache,NewCount);
          FExtUTF8OutCacheSize:=NewCount;
        end;
        NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
        gdk_text_extents(UseFont, FExtUTF8OutCache, NewCount,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end else begin
        gdk_text_extents(UseFont, Str, Count,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end;
      }
      gdk_text_extents(UseFont, Str, Count,@lbearing, @rBearing, @width, @ascent, @descent);
      Size.cX := Width;
      Size.cY := ascent+descent;
      if PartialWidths <> nil then
      begin
        Accumulator:=0;
        for i:= 0 to Count - 1 do
        begin
          Inc(Accumulator,gdk_char_width(UseFont,(Str+i)^));
          PartialWidths[i]:=Accumulator;
        end;
      end;
    end;
  end;
end;

function GetTextExtentExPointW(DC: HDC; Str: PWideChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  TempStr: UTF8String;
begin
  Logger.Send('GetTextExtentExPointW');
  TempStr:=UTF8Encode(WideString(Str));
  Result:=DelphiCompat.GetTextExtentExPoint(DC, PChar(TempStr),
  Length(TempStr), MaxWidth, MaxCount, PartialWidths,Size);
end;

function InvertRect(DC: HDC; const lprc: TRect): Boolean;
var
  DCOrigin: TPoint;
  Values: TGdkGCValues;
begin
  //todo: see the windows result when rect is invalid
  Result := GTKWidgetSet.IsValidDC(DC) and (lprc.Bottom > lprc.Top)
    and (lprc.Right > lprc.Left);
  if Result then
  begin
    with TDeviceContext(DC) do
    begin
      DCOrigin := GetDCOffset(TDeviceContext(DC));
      //todo: see if is necessary store old function
      gdk_gc_get_values(GC, @Values);
      gdk_gc_set_function(GC,GDK_INVERT);
      gdk_draw_rectangle(Drawable,GC,GdkTrue,
        DCOrigin.X + lprc.Left, DCOrigin.Y + lprc.Top,
        lprc.Right - lprc.Left, lprc.Bottom - lprc.Top);
      gdk_gc_set_function(GC,Values.thefunction);
    end;
  end;
end;

function GetTextAlign(hDC:HDC): LongWord;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord): BOOLEAN;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;

function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GdiFlush: Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetWindowDC(hWnd:THandle):HDC;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;



function CreatePatternBrush(hbmp:HBITMAP):HBRUSH;
var
  Log: TLogBrush;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
  //dummy implementation
  with Log do
  begin
    lbColor:=COLORREF(clBlack);
    lbHatch:=0;
    lbStyle:=BS_SOLID;
    Result:=CreateBrushIndirect(Log);
  end;
end;


function GetBkColor(DC:HDC):COLORREF;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetDCEx(hWnd:THandle; hrgnClip:HRGN; flags:DWORD):HDC;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function CopyImage(hImage:THANDLE; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function SystemParametersInfo(uiAction, uiParam:LongWord; pvParam:Pointer; fWinIni:LongWord):Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetKeyboardState(lpKeyState:PBYTE):BOOLEAN;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState:PBYTE; lpChar:PWORD; uFlags:LongWord):longint;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function BeginDeferWindowPos(nNumWindows:longint):THandle;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;
begin
Logger.SendCallStack('Dummy WinAPI Implementaion');
  //todo
end;


function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function SubtractRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): BOOLEAN;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetACP:LongWord;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function GetKeyboardLayout(dwLayout:DWORD):THandle;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
begin
  //todo
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;

function GetDoubleClickTime: UINT;
begin
  //todo: see if gtk has a value. Use Windows default for now
  Result:=500;
end;  

var
  CachedUnicodeFormat: TClipboardFormat;
  
function CF_UNICODETEXT: TClipboardFormat;
begin
  //todo: see what mime type gtk expects for utf16
  if CachedUnicodeFormat = 0 then
    CachedUnicodeFormat:= gdk_atom_intern('text/utf16',GdkFalse);
  Result := CachedUnicodeFormat;
end;


function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;
begin
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;

function KillTimer(hWnd:THandle; nIDEvent: LongWord):Boolean;
begin
  Logger.SendCallStack('Dummy WinAPI Implementaion');
end;


