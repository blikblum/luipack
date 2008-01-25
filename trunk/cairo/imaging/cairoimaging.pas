unit CairoImaging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingTypes, CairoClasses;
  
//todo: remove LCL dependency

type

  TCairoImagingOption = (cioAutoTransparentColor);

  TCairoImagingOptions = set of TCairoImagingOption;

  TCairoImagingTransparentMode = (citDefault, citMaskNonAlpha, citForceMaskColor, citNone);
  
  { TCairoImagingPicture }

  TCairoImagingPicture = class
  private
    FData: TImageData;
    FOnChange: TNotifyEvent;
    FSurface: TCairoImageSurface;
    FUseAlpha: Boolean;
    FOptions: TCairoImagingOptions;
    FTransparent: Boolean;
    FMaskColor: TColor24Rec;
    FTransparentMode: TCairoImagingTransparentMode;
    procedure Changed;
    procedure CreateSurface;
    procedure FreeResources;
    procedure Mask(const AMaskColor: TColor24Rec; ClearAlpha: Boolean);
    procedure PrepareTransparency;
    procedure SetOptions(const AValue: TCairoImagingOptions);
    procedure SetMaskColor(const AValue: TColor24Rec);
    procedure SetTransparentMode(const AValue: TCairoImagingTransparentMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    property Data: TImageData read FData;
    property Options: TCairoImagingOptions read FOptions write SetOptions;
    property TransparentMode: TCairoImagingTransparentMode read FTransparentMode write SetTransparentMode;
    property MaskColor: TColor24Rec read FMaskColor write SetMaskColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Surface: TCairoImageSurface read FSurface;
    
  end;

implementation

uses
  cairo14;

type
  TRGBA = record
    Blue:  Byte;
    Green: Byte;
    Red:   Byte;
    Alpha: Byte;
  end;
  PRGBA = ^TRGBA;
  
procedure MultiplyAlphaChannel(Pixel: PRGBA; ByteCount: PtrUInt);
begin
  ByteCount := ByteCount shr 2;
  while ByteCount > 0 do
  begin
    case Pixel^.Alpha of
      0:
        begin
          Pixel^.Red   := 0;
          Pixel^.Green := 0;
          Pixel^.Blue  := 0;
        end;
      255:
        begin
          //do nothing
        end;
    else
      Pixel^.Red   := (Pixel^.Red   * Pixel^.Alpha) div 255;
      Pixel^.Green := (Pixel^.Green * Pixel^.Alpha) div 255;
      Pixel^.Blue  := (Pixel^.Blue  * Pixel^.Alpha) div 255;
    end;
    Inc(Pixel);
    Dec(ByteCount);
  end;
end;


{ TCairoImagingPicture }

procedure TCairoImagingPicture.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCairoImagingPicture.CreateSurface;
const
  TransparentFormatMap: array[Boolean] of cairo_format_t = (CAIRO_FORMAT_RGB24, CAIRO_FORMAT_ARGB32);
begin
  FSurface := TCairoImageSurface.Create(FData.Bits, TransparentFormatMap[FTransparent],
    FData.Width, FData.Height, FData.Width * 4);
end;

procedure TCairoImagingPicture.FreeResources;
begin
  FSurface.Free;
  FSurface := nil;
  FreeImage(FData);
end;

procedure TCairoImagingPicture.Mask(const AMaskColor: TColor24Rec; ClearAlpha: Boolean);
//todo: see size of TColor32Rec
var
  Pixel: PColor32Rec;
  ByteCount: PtrUInt;
begin
  Pixel := FData.Bits;
  ByteCount := FData.Size shr 2;
  while ByteCount > 0 do
  begin
    //todo: optimize this comparation
    with Pixel^ do
    begin
      if (R = AMaskColor.R) and
        (G = AMaskColor.G) and
        (B = AMaskColor.B) then
        Color := $00000000
      else
        if ClearAlpha then
          A := 255;
    end;
    Inc(Pixel);
    Dec(ByteCount);
  end;
end;

procedure TCairoImagingPicture.PrepareTransparency;
const
  TransparentFormatMap: array[Boolean] of TImageFormat = (ifX8R8G8B8, ifA8R8G8B8);
var
  FormatInfo: TImageFormatInfo;
  AMaskColor: TColor24Rec;
begin
  GetImageFormatInfo(FData.Format, FormatInfo);
  FUseAlpha := FormatInfo.HasAlphaChannel;
  case FTransparentMode of
    citDefault:
      begin
        FTransparent := FUseAlpha;
      end;
    citMaskNonAlpha:
      begin
        FTransparent := True;
      end;
    citForceMaskColor:
      begin
        FUseAlpha := False;
        FTransparent := True;
      end;
    citNone:
      begin
        FTransparent := False;
      end;
  end;
  ConvertImage(FData, TransparentFormatMap[FTransparent]);
  
  if FTransparent then
  begin
    if FUseAlpha then
      MultiplyAlphaChannel(FData.Bits, FData.Size)
    else
    begin
      if cioAutoTransparentColor in FOptions then
        AMaskColor := GetPixel32(FData, 0, 0).Color24Rec
      else
        AMaskColor := FMaskColor;
      Mask(AMaskColor, FormatInfo.HasAlphaChannel);
    end;
  end;
end;

procedure TCairoImagingPicture.SetOptions(
  const AValue: TCairoImagingOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TCairoImagingPicture.SetMaskColor(const AValue: TColor24Rec);
begin

end;

procedure TCairoImagingPicture.SetTransparentMode(
  const AValue: TCairoImagingTransparentMode);
begin
  if FTransparentMode=AValue then exit;
  FTransparentMode:=AValue;
end;

constructor TCairoImagingPicture.Create;
begin
  with FMaskColor do
  begin
    R := 255;
    //G := 0;
    B := 255;
  end;
end;

destructor TCairoImagingPicture.Destroy;
begin
  FreeResources;
end;

procedure TCairoImagingPicture.LoadFromFile(const FileName: String);
begin
  FreeResources;
  if not LoadImageFromFile(FileName, FData) then
    raise Exception.Create('Error loading "' + FileName + '"');
  PrepareTransparency;
  CreateSurface;
  Changed;
end;

end.

