
Procedure TimerCallBack(Handle: hwnd; msg : DWORD; idEvent: UINT; dwTime: DWORD); stdcall;
var
  TimerInfo: TTimerRecord;
  Message: TLMTimer;
begin
  if FTimerList.GetTimerInfo(Handle,idEvent,TimerInfo) then
  with TimerInfo do
  begin
    if Notify <> nil then
      Notify(idEvent)
    else
    begin
      with Message do
      begin
        Msg:=LM_TIMER;
        TimerID:=idEvent;
        TimerProc:=0;
        Result:=0;
        DeliverMessage(Control,Message);
      end;
    end;
  end
  else
    DebugLn('Warning - No TimerInfo found for %d-%d',[Handle,idEvent]);
end;

function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;
var
  WinInfo: PWindowInfo;
begin
  WinInfo:=GetWindowInfo(hWnd);
  FTimerList.Add(hWnd,nIDEEvent,lpTimerFunc,WinInfo^.WinControl);
  Windows.SetTimer(hWnd,nIDEvent,uElapse,@TimerCallBack);
end;
