
{$ifdef READ_CLASS_INTERFACE}

  procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer) override;
  procedure DoOnResize; override;
  procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
  procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;

{$endif}


{$ifdef READ_CLASS_IMPLEMENTATION}

procedure TLuiImage.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  //Prevents resize the control
  if lioAutoSize in FOptions then
  begin
    AWidth := Width;
    AHeight := Height;
  end;
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TLuiImage.DoOnResize;
begin
  inherited DoOnResize;
  if not (lisAutoSizePending in FStates) and (FPicture.Data.Size <> 0) then
  begin
    if FViewStyle in [livNormal, livScale] then
      Include(FStates, lisScrollBarsUpdatePending);
    Include(FStates, lisPaddingCalcPending);
    Include(FStates, lisScaleCalcPending);
    if lioAutoSize in FOptions then
      Include(FStates, lisAutoSizePending);
    Changed;
  end;
end;

procedure TLuiImage.WMHScroll(var Message: TLMHScroll);
begin
  DebugLn('#WMHScroll#');
  case Message.ScrollCode of
    SB_BOTTOM:
      FHScrollOffset := FHScrollRange - ClientWidth;
    SB_ENDSCROLL:;
      //??;
    SB_LINEUP:
      FHScrollOffset := Max(FHScrollOffset - 10, 0);
    SB_LINEDOWN:
      FHScrollOffset := Min(FHScrollOffset + 10, FHScrollRange - ClientWidth);
    SB_PAGEUP:
      FHScrollOffset := Max(FHScrollOffset - ClientWidth, 0);
    SB_PAGEDOWN:
      FHScrollOffset := Min(FHScrollOffset + ClientWidth, FHScrollRange - ClientWidth);
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FHScrollOffset := Message.Pos;
      end;
    SB_TOP:
      FHScrollOffset := 0;
  end;
  UpdateHorizontalScrollBar;
  Message.Result := 0;
  Redraw;
end;

procedure TLuiImage.WMVScroll(var Message: TLMVScroll);
begin
  DebugLn('#WMVScroll#');
  case Message.ScrollCode of
    SB_BOTTOM:
      FVScrollOffset := FVScrollRange - ClientHeight;
    SB_ENDSCROLL:;
      //??;
    SB_LINEUP:
      FVScrollOffset := Max(FVScrollOffset - 10, 0);
    SB_LINEDOWN:
      FVScrollOffset := Min(FVScrollOffset + 10, FVScrollRange - ClientHeight);
    SB_PAGEUP:
      FVScrollOffset := Max(FVScrollOffset - ClientHeight, 0);
    SB_PAGEDOWN:
      FVScrollOffset := Min(FVScrollOffset + ClientHeight, FVScrollRange - ClientHeight);
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FVScrollOffset := Message.Pos;
      end;
    SB_TOP:
      FVScrollOffset := 0;
  end;
  UpdateVerticalScrollBar;
  Message.Result := 0;
  Redraw;
end;

procedure TLuiImage.UpdateHorizontalScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FHScrollRange;
  ScrollInfo.nPos := FHScrollOffset;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);
end;

procedure TLuiImage.UpdateVerticalScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FVScrollRange;
  ScrollInfo.nPos := FVScrollOffset;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Vert, ScrollInfo, True);
end;

procedure TLuiImage.UpdateScrollBars;
var
  VScrollVisible, HScrollVisible: Boolean;
begin
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

  Exclude(FStates, lisScrollBarsUpdatePending);
end;

{$endif}
