
function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
var
  i: Integer;
  XOffset, YOffset: SmallInt;
  FromRect, ToRect: TRect;
begin
  if hWndFrom <> 0 then
    GetWindowRect(hWndFrom, FromRect)
  else
  begin
    FromRect.Left := 0;
    FromRect.Top := 0;
  end;
  if hWndTo <> 0 then
    GetWindowRect(hWndTo, ToRect)
  else
  begin
    ToRect.Left := 0;
    ToRect.Top := 0;
  end;
  XOffset := (FromRect.Left - ToRect.Left);
  YOffset := (FromRect.Top - ToRect.Top);
  for i := 0 to cPoints - 1 do
  begin
    PPoint(@lpPoints)[i].x := XOffset + PPoint(@lpPoints)[i].x;
    PPoint(@lpPoints)[i].y := YOffset + PPoint(@lpPoints)[i].y;
  end;
  Result := MakeLong(XOffset, YOffset);
end;

{$ifndef HAS_GETDOUBLECLICKTIME}
function GetDoubleClickTime: UINT;
begin
  //todo: see if gtk has a value. Use Windows default for now
  Result := 500;
end;
{$endif}

{$ifndef HAS_REDRAWWINDOW}
function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
begin
  //todo: see if there's a better way of doing this
  Result := LCLIntf.InvalidateRect(hWnd, lprcUpdate, (RDW_ERASE and flags) > 0);
end;
{$endif}
