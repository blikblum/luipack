

function CreateBitmapMask(BitmapDC: HDC; Width, Height: Integer; TransparentColor: TColor): HBITMAP;
var
  OldColor: COLORREF;
  OldObj: HBITMAP;
  MaskDC: HDC;
begin
  Result := Windows.CreateBitmap(Width,Height,1,1,nil);
  MaskDC := Windows.CreateCompatibleDC(BitmapDC);
  
  OldObj := Windows.SelectObject(MaskDC,Result);
  OldColor := Windows.SetBkColor(BitmapDC, Windows.COLORREF(ColorToRGB(TransparentColor)));

  Windows.BitBlt(MaskDC,0,0,Width,Height,BitmapDC,0,0,NOTSRCCOPY);

  Windows.SetBkColor(BitmapDC,OldColor);
  Windows.SelectObject(MaskDC,OldObj);
  Windows.DeleteDC(MaskDC);
end;

function OSSupportsUTF16: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;
