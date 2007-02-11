//Used in DrawTextW
{
function GetTextAlign(DC: HDC): UINT;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetTextAlign');
  Result:=TA_TOP or TA_LEFT;
end;
}
//Used in DrawTextW, ShortenString, TVirtualTreeColumn.ComputeHeaderLayout, TVirtualTreeColumns.DrawButtonText,
// TVTEdit.AutoAdjustSize, TCustomVirtualStringTree.PaintNormalText, TCustomVirtualStringTree.WMSetFont
// TCustomVirtualStringTree.DoTextMeasuring
{
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;
var
 TempStr: String;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetTextExtentPoint32W');
  TempStr:=WideCharToString(Str);
  Result:=GetTextExtentPoint(DC, PChar(TempStr), Length(TempStr), Size);
end;
}
//Used in DrawTextW
{
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;
var
 TempStr: String;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'ExtTextOutW');
  TempStr:=WideCharToString(Str);
  Result:= ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr), Length(TempStr), Dx);
end;
}

//Used in TVirtualTreeHintWindow.CalcHintRect, TVirtualTreeColumn.ComputeHeaderLayout
//  TBaseVirtualTree.CollectSelectedNodesRTL, TBaseVirtualTree.DetermineHitPositionRTL
// TBaseVirtualTree.UpdateEditBounds, TBaseVirtualTree.GetDisplayRect, PaintTree,
// TStringEditLink.PrepareEdit, TCustomVirtualStringTree.ComputeNodeHeight etc
procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
  case Alignment of
  taLeftJustify: Alignment := taRightJustify;
  taRightJustify: Alignment := taLeftJustify;
  end;
end;

function INDEXTOOVERLAYMASK(i : longint) : longint;
{ return type might be wrong }
begin
  INDEXTOOVERLAYMASK:=i shl 8;
end;


function MAKEROP4(fore,back : longint) : DWORD;
begin
   MAKEROP4:=DWORD((DWORD(back shl 8) and $FF000000) or DWORD(fore));
end;

function Failed(Status : HRESULT) : BOOL;
begin
   Failed:=Status and HRESULT($80000000)<>0;
end;

function MapWindowPoints(hWndFrom, hWndTo: HWND; lpPoints: PPoint; cPoints: UINT): Integer;
var
  I:integer;
  FromRect,ToRect: TRect;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'MapWiindowsPoints');
  //todo: implement result
  GetWindowRect(hWndFrom,FromRect);
  GetWindowRect(hWndTo,ToRect);
  for i:=0 to cPoints - 1 do
  begin
    (lpPoints+i)^.x:=(FromRect.Left - ToRect.Left) + (lpPoints+i)^.x;
    (lpPoints+i)^.y:=(FromRect.Top - ToRect.Top) + (lpPoints+i)^.y;
  end;
end;

{$ifndef NeedWindows}

//Dummy function. Used in many places
function ImageList_DrawEx(himl:THandle; i:longint; hdcDst:HDC; x:longint;
  y:longint;dx:longint; dy:longint; rgbBk:COLORREF; rgbFg:COLORREF; fStyle:UINT):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'ImageList_DrawEx');
end;

//Used in TVirtualTreeColumns.AnimatedResize
function GetWindowDC(hWnd:HWND):HDC;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetWindowDC');
end;

//Used in TVTDragImage.DragTo, TVirtualTreeColumns.AnimatedResize, TBaseVirtualTree.ToggleCallback,
//TBaseVirtualTree.TileBackground

function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT;
  var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'ScrollDC');
end;


// TCustomVirtualTreeOptions.SetPaintOptions, TVTHeader.Invalidate, TVTColors.SetColor
//CMEnabledChanged, TBaseVirtualTree.WMEnable, TBaseVirtualTree.WMVScroll,
// TBaseVirtualTree.UpdateWindowAndDragImage, TBaseVirtualTree.RepaintNode
function RedrawWindow(hWnd:HWND; lprcUpdate:PRECT; hrgnUpdate:HRGN;
  flags:LongWord):Boolean; overload;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'RedrawWindow');
end;

function RedrawWindow(hWnd:HWND; var lprcUpdate:TRECT; hrgnUpdate:HRGN;
  flags:UINT):Boolean; overload;
begin
end;
//Used in LimitPaintingToArea
function LPtoDP(_para1:HDC; _para2:PPOINT; _para3:longint):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'LPtoDP');
end;

//Used to the brush of dottedline
function CreatePatternBrush(_para1:HBITMAP):HBRUSH;
var
  ALog: TLogBrush;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'CreatePatternBrush');
  //lcl_todo
  with ALog do
  begin
    lbStyle:=0;
    lbColor:=0;
    lbHatch:=0;
  end;
  Result:=CreateBrushIndirect(ALog);
end;

function GetBkColor(_para1:HDC):COLORREF;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetBkColor');
end;


function ImageList_DragShowNolock(fShow:Boolean): Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'ImageList_DragShowNoLock');
end;

function GetKeyboardState(lpKeyState:PBYTE):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetKeyboardState');
end;

function ToAscii(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE;
  lpChar: PChar; uFlags:UINT):longint;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'ToAscii');
end;

function SystemParametersInfo(uiAction:UINT; uiParam:UINT;
   pvParam:Pointer; fWinIni:UINT):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'SystemParameterInfo');
end;

function SetTimer(hWnd:HWND; nIDEvent:UINT; uElapse:UINT; lpTimerFunc:Pointer):UINT;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'SetTimer');
end;

function SubtractRect(lprcDst:TRECT; lprcSrc1:TRECT; lprcSrc2:TRECT):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'SubtractRect');
end;

function DeferWindowPos(hWinPosInfo:THandle; hWnd:THandle; hWndInsertAfter:THandle;
  x:longint; y:longint;cx:longint; cy:longint; uFlags:UINT):THandle;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'DeferWindowPos');
end;

function BeginDeferWindowPos(nNumWindows:longint):THandle;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'BeginDeferWindowPos');
end;

function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'EndDeferWindowpos');
end;

function GetLocaleInfo(Locale:DWord; LCType:DWord; lpLCData:PChar; cchData:longint):longint;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetLocaleInfo');
end;

function GetACP:UINT;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetACP');
end;

function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:PChar;
  cchMultiByte:longint; lpWideCharStr:PWideChar; cchWideChar:longint):longint;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'MultiByteToWideChar');
end;

function GetKeyboardLayout(dwLayout:DWORD):THandle;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetKeyboardLayout');
end;

function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'DefWindowProc');
end;

function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'GetDCEx');
end;

function OffsetRgn(_para1:HRGN; _para2:longint; _para3:longint):longint;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'OffsetRegion');
end;

function SetBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:PPOINT):Boolean;
begin
  Logger.AddCheckPoint(lcDummyFunctions,'SetBrushOrgEx');
end;

{$endif}
