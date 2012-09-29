{ This file is part of Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$define HAS_INVERTRECT}
{$define HAS_DRAWTEXTW}
{$define HAS_GETCURRENTOBJECT}
{$define HAS_GETBKCOLOR}
{$define HAS_GETTEXTEXTENTEXPOINT}
{$define HAS_SCROLLWINDOW}

{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}


procedure pango_extents_to_pixels (ink_rect: PPangoRectangle;
			  logical_rect: PPangoRectangle); cdecl; external 'libpango-1.0.so.0';

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := GTK2WidgetSet.StretchCopyArea(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
   0, XSrc, YSrc, Rop);
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
  TempStr: UTF8String;
  TempRect: TRect;
  TextHeight: Integer;
  TM: TTextMetric;
begin
  //Logger.Send('DrawTextW');
  TempRect := lpRect;
  //fix position under gtk (lcl bug 8565)
  if (uFormat and DT_VCENTER) > 0 then
  begin
    GetTextMetrics(hDC, TM);
    //gtk overestimate height
    TextHeight := TM.tmHeight - 2;
    TempRect.Top := (TempRect.Top + TempRect.Bottom - TextHeight) div 2;
  end;
  TempStr := UTF8Encode(WideString(lpString));
  Result := DrawText(hDC, PChar(TempStr), GetUTF8ByteCount(TempStr, nCount),
    TempRect, uFormat);
  //Logger.Send('Rect',TempRect);
end;

function GetBkColor(DC:HDC):COLORREF;
begin
  if GTK2WidgetSet.IsValidDC(DC) then
    Result := TGtkDeviceContext(DC).CurrentBackColor.ColorRef
  else
    Result := CLR_INVALID;
end;

function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := 0;
  if GTK2WidgetSet.IsValidDC(hdc) then
  {$ifdef LAZARUS_1_0}
  with TGtk2DeviceContext(hdc) do
  {$else}
  with TGtkDeviceContext(hdc) do
  {$endif}
  begin
    case uObjectType of
      OBJ_BITMAP: Result := HGDIOBJ(CurrentBitmap);
      OBJ_BRUSH: Result := HGDIOBJ(CurrentBrush);
      OBJ_FONT: Result := HGDIOBJ(CurrentFont);
      OBJ_PEN: Result := HGDIOBJ(CurrentPen);
    end;
  end;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  layout: PPangoLayout;
  i: Integer;
  Rect: TPangoRectangle;
  iter : PPangoLayoutIter;
begin
  Result := GTK2WidgetSet.IsValidDC(DC);
  if Result then
  {$ifdef LAZARUS_1_0}
  with TGtk2DeviceContext(DC) do
  {$else}
  with TGtkDeviceContext(DC) do
  {$endif}
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then
      layout := GTK2WidgetSet.GetDefaultGtkFont(false)
    else
      layout := CurrentFont^.GDIFontObject;
    pango_layout_set_text(layout, Str, Count);
    if PartialWidths = nil then
      pango_layout_get_pixel_size (layout, @Size.cx, @Size.cy)
    else
      begin
        i := 0;
        Size.cx := 0;
        Size.cy := 0;
        iter := pango_layout_get_iter(layout);
        repeat
          pango_layout_iter_get_char_extents(iter,@Rect);
          pango_extents_to_pixels(nil,@Rect);
          inc(Size.cx, Rect.Width);
          PartialWidths[i] := Size.cx;
          if Size.cy < Rect.Height then
            Size.cy := Rect.Height;
          inc(i);
        until not pango_layout_iter_next_char(iter);
        pango_layout_iter_free(iter);
      end;
  end;
end;

function InvertRect(DC: HDC; const lprc: TRect): Boolean;
var
  DCOrigin: TPoint;
  Values: TGdkGCValues;
begin
  //todo: see the windows result when rect is invalid
  Result := GTK2WidgetSet.IsValidDC(DC) and (lprc.Bottom > lprc.Top)
    and (lprc.Right > lprc.Left);
  if Result then
  begin
    {$ifdef LAZARUS_1_0}
    with TGtk2DeviceContext(DC) do
    {$else}
    with TGtkDeviceContext(DC) do
    {$endif}
    begin
      DCOrigin := Offset;
      //todo: see if is necessary store old function
      gdk_gc_get_values(GC, @Values);
      gdk_gc_set_function(GC,GDK_INVERT);
      gdk_draw_rectangle(Drawable,GC,1,
        DCOrigin.X + lprc.Left, DCOrigin.Y + lprc.Top,
        lprc.Right - lprc.Left, lprc.Bottom - lprc.Top);
      gdk_gc_set_function(GC,Values._function);
    end;
  end;
end;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;
begin
  //gtk implementation does nothing if lpRect and lpClipRect are not nil
  Result := LCLIntf.ScrollWindowEx(hWnd, XAmount, YAmount, nil, nil, 0, nil, SW_INVALIDATE);
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

type

  TTimerID = record
    hWnd: THandle;
    idEvent: UINT_PTR;
  end;

  { TGtk2TimerInfo }

  TGtk2TimerInfo = class
  private
    FControl: TControl;
    FNotify: TTimerNotify;
    FId: UINT_PTR;
    FTimerHandle: guint;
  public
    constructor Create(hWnd: THandle; idEvent: UINT_PTR; NotifyFunc: TTimerNotify);
    procedure Reset(hWnd: THandle; idEvent: UINT_PTR; NotifyFunc: TTimerNotify);
  end;

  { TTimerList }

  TTimerList = class
  private
    FMap: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(hWnd: THandle; idEvent: UINT_PTR; NotifyFunc: TTimerNotify): TGtk2TimerInfo;
    function Find(hWnd: THandle; idEvent: UINT_PTR): TGtk2TimerInfo;
  end;

var
  FTimerList: TTimerList;

{ TGtk2TimerInfo }

constructor TGtk2TimerInfo.Create(hWnd: THandle; idEvent: UINT_PTR;
  NotifyFunc: TTimerNotify);
begin
  //todo: make a custom GetLCLObject
  if hWnd <> 0 then
    FControl := TControl(GetLCLObject(PGtkWidget(hWnd)))
  else
    FControl := nil;
  FId := idEvent;
  FNotify := NotifyFunc;
end;

procedure TGtk2TimerInfo.Reset(hWnd: THandle; idEvent: UINT_PTR;
  NotifyFunc: TTimerNotify);
begin
  //todo: make a custom GetLCLObject
  if hWnd <> 0 then
    FControl := TControl(GetLCLObject(PGtkWidget(hWnd)))
  else
    FControl := nil;
  if FTimerHandle <> 0 then
    g_source_remove(FTimerHandle);
  FId := idEvent;
  FNotify := NotifyFunc;
end;

{ TTimerList }

constructor TTimerList.Create;
begin
  FMap := TMap.Create({$ifdef CPU64}itu16{$else}itu8{$endif}, SizeOf(TGtk2TimerInfo));
end;

destructor TTimerList.Destroy;
var
  Iterator: TMapIterator;
  TimerInfo: TGtk2TimerInfo;
begin
  Iterator := TMapIterator.Create(FMap);
  with Iterator do
  begin
    while not EOM do
    begin
      GetData(TimerInfo);
      TimerInfo.Free;
      Next;
    end;
    Destroy;
  end;
  FMap.Destroy;
end;

function TTimerList.Get(hWnd: THandle; idEvent: UINT_PTR; NotifyFunc: TTimerNotify): TGtk2TimerInfo;
var
  TimerID: TTimerID;
begin
  TimerID.hWnd := hWnd;
  TimerID.idEvent := idEvent;
  with FMap do
  begin
    if HasId(TimerID) then
    begin
      // DebugLn('Reset timer for HWnd: %d ID: %d AID: %d', [hWnd, ID, TimerID]);
      GetData(TimerID, Result);
      Result.Reset(hWnd, idEvent, NotifyFunc);
    end
    else
    begin
      // DebugLn('Create timer for HWnd: %d ID: %d AID: %d', [hWnd, ID, TimerID]);
      Result := TGtk2TimerInfo.Create(hWnd, idEvent, NotifyFunc);
      if hWnd = 0 then
      begin
        TimerID.idEvent := PtrUInt(Result);
        Result.FId := PtrUInt(Result);
      end;
      Add(TimerID, Result);
    end;
  end;
end;

function TTimerList.Find(hWnd: THandle; idEvent: UINT_PTR): TGtk2TimerInfo;
var
  DataPtr: ^TGtk2TimerInfo;
  TimerID: TTimerID;
begin
  Result := nil;
  TimerID.hWnd := hWnd;
  TimerID.idEvent := idEvent;
  // DebugLn('GetTimerInfo for HWnd: %d ID: %d AID: %d', [hWnd, ID, TimerID]);
  DataPtr := FMap.GetDataPtr(TimerID);
  if DataPtr <> nil then
    Result := DataPtr^;
end;

function gtkTimerCB(Data: gPointer): gBoolean; cdecl;
begin
  Result := GdkFalse;  // assume: timer will stop
  with TGtk2TimerInfo(Data) do
  begin
    //DebugLn('gtkTimerCalled for TimerHandle: %d',[TimerHandle]);
    if FTimerHandle <> 0 then
    begin
      if FNotify <> nil then
      begin
        FNotify(FId);
        Result := GdkTrue;
      end
      else
      begin
        if FControl <> nil then
        begin
          LCLSendTimerMsg(FControl, FId, 0);
          Result := GdkTrue;
        end;
      end;
    end;
  end;
end;

function SetTimer(hWnd:THandle; nIDEvent:UINT_PTR; uElapse:LongWord; lpTimerFunc:TTimerNotify):UINT_PTR;
var
  TimerInfo: TGtk2TimerInfo;
begin
  TimerInfo := FTimerList.Get(hWnd, nIDEvent, lpTimerFunc);
  try
    TimerInfo.FTimerHandle := g_timeout_add(uElapse, @gtkTimerCB, TimerInfo);
    if hWnd = 0 then
      Result := PtrUInt(TimerInfo)
    else
      Result := nIdEvent;
  except
    Result := 0;
  end;
  //DebugLn('SetTimer HWnd: %d ID: %d TimerHandle: %d',[hWnd,nIDEvent,TimerInfo.FTimerHandle]);
end;

function KillTimer(hWnd:THandle; nIDEvent:UINT_PTR):Boolean;
var
  TimerInfo: TGtk2TimerInfo;
begin
  //todo: properly handle Result
  Result := True;
  TimerInfo := FTimerList.Find(hWnd,nIDEvent);
  if (TimerInfo <> nil) and (TimerInfo.FTimerHandle <> 0) then
  begin
    //DebugLn('KillTimer HWnd: %d ID: %d TimerHandle: %d',[hWnd,nIDEvent,TimerInfo^.TimerHandle]);
    g_source_remove(TimerInfo.FTimerHandle);
    //next time gtkTimerCB be called the timeout will be destroied automatically
    //todo: see if is really necessary to set TimerHandle to 0 and check in gtkTimerCB
    TimerInfo.FTimerHandle := 0;
  end;
  //else
  //  DebugLn('KillTimer Could not find the timer info of HWnd: %d ID: %d',[hWnd,nIDEvent]);
end;



