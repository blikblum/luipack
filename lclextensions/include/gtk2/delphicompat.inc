{$define GTK2}
{$i ../gtk/delphicompat.inc}

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  layout: PPangoLayout;
  i, j, Accumulator: Integer;
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
      Accumulator := 0;
      for i:= 0 to Count - 1 do
      begin
        pango_layout_set_text(layout, @PChar(Str + i)^, 1);
        pango_layout_get_pixel_size (layout, @j, nil);
        inc(Accumulator,j);
        PartialWidths[i] := Accumulator;
      end;
    end;
  end;
end;

