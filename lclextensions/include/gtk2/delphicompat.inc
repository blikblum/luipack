{$define GTK2}
{$i ../gtk/delphicompat.inc}

procedure pango_extents_to_pixels (ink_rect: PPangoRectangle;
			  logical_rect: PPangoRectangle); cdecl; external 'libpango-1.0.so.0';

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  layout: PPangoLayout;
  i, j: Integer;
  Rect: TPangoRectangle;
  iter : PPangoLayoutIter;
begin
  Result := GTKWidgetSet.IsValidDC(DC);
  if Result then
  with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then
      layout := GTKWidgetSet.GetDefaultGtkFont(false)
    else
      layout := CurrentFont^.GDIFontObject;
    pango_layout_set_text(layout, Str, Count);
    pango_layout_get_pixel_size (layout, @Size.cx, @Size.cy);
    if PartialWidths <> nil then
    begin
      i := 0;
      j := 0;
      iter := pango_layout_get_iter(layout);
      repeat
        pango_layout_iter_get_char_extents(iter,@Rect);
        pango_extents_to_pixels(nil,@Rect);
        inc(j, Rect.Width);
        PartialWidths[i] := j;
        inc(i);
      until not pango_layout_iter_next_char(iter);
      pango_layout_iter_free(iter);
    end;
  end;
end;

