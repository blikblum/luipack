uses
  CairoWin32;
  
function CreateSurfaceFromDC(DC: HDC): Pcairo_surface_t;
begin
  Result := cairo_win32_surface_create(DC);
end;

procedure TCairoLCLFont.CreateHandle;
//code borrowed from LCL
const
  LF_BOOL: array[Boolean] of Byte = (0, 255);
  LF_WEIGHT: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
var
  ALogFont: TLogFont;
  TempFont: HFONT;
  
  procedure SetLogFontName(const NewName: string);
  var
    l: Integer;
  begin
    l := Min(High(ALogFont.lfFaceName) - Low(ALogFont.lfFaceName), Length(NewName));
    if l > 0 then
      Move(NewName[1], ALogFont.lfFaceName[Low(ALogFont.lfFaceName)], l);
    ALogFont.lfFaceName[Low(ALogFont.lfFaceName) + l] := #0;
  end;

begin
  FillChar(ALogFont, SizeOf(ALogFont), 0);
  with ALogFont do
  begin
    lfHeight := FHeight;
    //lfWidth := 0;
    //lfEscapement := 0;
    //lfOrientation := 0;
    lfWeight := LF_WEIGHT[fsBold in FStyle];
    lfItalic := LF_BOOL[fsItalic in FStyle];
    lfUnderline := LF_BOOL[fsUnderline in FStyle];
    lfStrikeOut := LF_BOOL[fsStrikeOut in FStyle];
    lfCharSet := Byte(FCharset);
    SetLogFontName(Name);

    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  TempFont := CreateFontIndirect(ALogFont);
  FHandle := cairo_win32_font_face_create_for_hfont(TempFont);
  DeleteObject(TempFont);
end;
