{$ifdef READ_CLASS_INTERFACE}
  procedure HandleResize(AWidth, AHeight: TfpgCoord); override;
//  procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
//  procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
{$endif}


{$ifdef READ_CLASS_IMPLEMENTATION}

procedure TLuiImage.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  if not (lioAutoSize in FOptions) then
    inherited HandleResize(AWidth, AHeight);
end;

procedure TLuiImage.UpdateHorizontalScrollBar;
{
var
  ScrollInfo: TScrollInfo;
}
begin
{
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FHScrollRange;
  ScrollInfo.nPos := FHScrollOffset;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);
}
end;

procedure TLuiImage.UpdateVerticalScrollBar;
{
var
  ScrollInfo: TScrollInfo;
}
begin
{
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FVScrollRange;
  ScrollInfo.nPos := FVScrollOffset;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Vert, ScrollInfo, True);
}
end;

procedure TLuiImage.UpdateScrollBars;
var
  VScrollVisible, HScrollVisible: Boolean;
begin
{
  FVScrollOffset := 0;
  FVScrollRange := 0;
  FHScrollOffset := 0;
  FHScrollRange := 0;
  //shortcut
  if not (FViewStyle in [livNormal, livScale]) then
  begin
    ShowScrollBar(Handle, SB_BOTH, False);
    Exit;
  end;
  //vertical scrollbar
  FVScrollRange := FPadding.Top + FPadding.Bottom + GetImageHeight;
  VScrollVisible := (FVScrollRange - Height) > 0;
  DebugLn('VScrollVisible ', BoolToStr(VScrollVisible, True));
  ShowScrollBar(Handle, SB_Vert, VScrollVisible);
  //horizontal scrollbar (takes into account vertical scroll - if any)
  FHScrollRange := FPadding.Left + FPadding.Right + GetImageWidth;
  HScrollVisible := (FHScrollRange - ClientWidth) > 0;
  DebugLn('HScrollVisible ', BoolToStr(HScrollVisible, True));
  ShowScrollBar(Handle, SB_Horz, HScrollVisible);
  //update vertical scrollbar to take the horizontal scrollbar size into account
  if HScrollVisible and not VScrollVisible then
  begin
    VScrollVisible := (FVScrollRange - ClientHeight) > 0;
    if VScrollVisible then
      ShowScrollBar(Handle, SB_Vert, VScrollVisible);
  end;

  if VScrollVisible then
    UpdateVerticalScrollBar;
  if HScrollVisible then
    UpdateHorizontalScrollBar;
}
  Exclude(FStates, lisScrollBarsUpdatePending);
end;

{$endif}
