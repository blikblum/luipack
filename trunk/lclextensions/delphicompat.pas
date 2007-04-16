unit delphicompat;

{ Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  LMessages, Types, LCLType, Classes;

const
  //Messages
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;
  WM_CHAR = LM_CHAR;
  WM_KEYDOWN = LM_KEYDOWN;
  WM_KILLFOCUS = LM_KILLFOCUS;
  
  //Misc Constants
  MAXSHORT = $7FFF;
  
  IMAGE_BITMAP = 0;
  IMAGE_CURSOR = 2;
  IMAGE_ENHMETAFILE = 1;
  IMAGE_ICON = 1;
  LR_COPYDELETEORG = 8;
  LR_COPYRETURNORG = 4;
  LR_MONOCHROME = 1;
  LR_CREATEDIBSECTION = 8192;
  LR_DEFAULTSIZE = 64;
  
  //drawedge constants
  EDGE_SUNKEN = 10;
  BF_RECT = 15;
  BF_ADJUST = 8192;
  
  //systemparametersinfo
  SPI_GETWORKAREA = 48;
  
type
  //TWM* types
  TMessage = TLMessage;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;
  TWMChar = TLMChar;
  TWMKeyDown = TLMKeyDown;
  TWMKillFocus = TLMKillFocus;
  
//Unicode functions

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;

//GDI Functions

function GetTextExtentExPoint(DC: HDC; p2: PChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

function InvertRect(DC: HDC; const lprc: TRECT): Boolean;

function GetTextAlign(hDC:HDC): LongWord;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord): BOOLEAN;

function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;

function GdiFlush: Boolean;

function GetWindowDC(hWnd:THandle):HDC;

function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;

function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;

function CreatePatternBrush(hbmp:HBITMAP):HBRUSH;

function GetBkColor(DC:HDC):COLORREF;

function GetDCEx(hWnd:THandle; hrgnClip:HRGN; flags:DWORD):HDC;

function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall;

//misc

function CopyImage(hImage:THANDLE; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;

function SystemParametersInfo(uiAction, uiParam:LongWord; pvParam:Pointer; fWinIni:LongWord):Boolean;

function GetKeyboardState(lpKeyState:PBYTE):BOOLEAN;

function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState:PBYTE; lpChar:PWORD; uFlags:LongWord):longint;

function ImageList_DragShowNolock(fShow: Boolean): Boolean;

function BeginDeferWindowPos(nNumWindows:longint):THandle;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;

function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;

function SubtractRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): BOOLEAN;

function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;

function GetACP:LongWord;

function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;

function GetKeyboardLayout(dwLayout:DWORD):THandle;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;

function MAKEROP4(fore,back : longint) : DWORD;

function INDEXTOOVERLAYMASK(i : longint) : longint;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);

//clipboard

function CF_UNICODETEXT: TClipboardFormat;

//timer

type
  TTimerNotify = procedure (TimerId: LongWord) of Object;

  TLMTimer = record
    Msg: Cardinal;
    TimerID: LongWord;
    TimerProc: LPARAM; //TTimerNotify;
    Result: LRESULT;
  end;


function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;

function KillTimer(hWnd:THandle; nIDEvent: LongWord):Boolean;

implementation


uses
{$i uses.inc}
  maps, LCLProc, LCLMessageGlue, Controls, multiloglcl, filechannel, ipcchannel;

const
  //Logger  classes
  lcInfo = 0;
  lcStack = 1;

type
  TTimerRecord = record
    Control: TControl;
    Notify: TTimerNotify;
  end;

  { TTimerList }

  TTimerList = class
  private
    FList: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl);
    function GetTimerInfo(Handle: hWnd; idEvent:LongWord; out TimerInfo: TTimerRecord):Boolean;
  end;

var
  FTimerList: TTimerList;
  Logger: TLCLLogger;
  
  
function MakeQWord(d1, d2: dword): QWord;
begin
  Result:=(QWord(d2) shl 32) or d1;
end;
  
{ TTimerList }

constructor TTimerList.Create;
begin
  //todo: see 64bit (itu16??)
  FList:=TMap.Create(itu8,SizeOf(TTimerRecord));
end;

destructor TTimerList.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TTimerList.Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl);
var
  AID: QWord;
  ATimerRec: TTimerRecord;
begin
  ATimerRec.Notify:= NotifyFunc;
  ATimerRec.Control:= WinControl;
  AId:=MakeQWord(hWnd,ID);
  with FList do
  begin
    if HasId(AID) then
      SetData(AID, ATimerRec)
    else
      Add(AID,ATimerRec);
  end;
end;

function TTimerList.GetTimerInfo(Handle: hWnd; idEvent: LongWord; out
  TimerInfo: TTimerRecord): Boolean;
begin
  Result:= FList.GetData(MakeQWord(Handle,idEvent),TimerInfo);
end;


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
  Result:=i shl 8;
end;

function MAKEROP4(fore,back : longint) : DWORD;
begin
   Result:=DWORD((DWORD(back shl 8) and $FF000000) or DWORD(fore));
end;


{$i delphicompat.inc}

initialization
  FTimerList:=TTimerList.Create;
  Logger:= TLCLLogger.Create;
  {$ifdef DEBUG_DELPHICOMPAT}
  Logger.Channels.Add(TFileChannel.Create('delphicompat.log'));
  Logger.ActivateClasses:=[lcInfo,lcStack];
  Logger.MaxStackCount:=3;
  {$else}
  Logger.ActiveClasses:=[];
  {$endif}

finalization
  FTimerList.Free;
  Logger.Free;
end.

