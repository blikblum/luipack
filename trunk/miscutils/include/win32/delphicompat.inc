
  
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

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;
begin
  Result:=Windows.InvertRect(hDC, lprc);
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
  DebugLn('Adding Timer to Handle %d - ID: %d',[hWnd,idEvent]);
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
