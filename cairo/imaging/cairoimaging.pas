unit CairoImaging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingTypes, CairoClasses;
  
//todo: remove LCL dependency

type

  TCairoImagingOption = (cioAutoMaskColor, cioAllowChangesAfterLoad);

  TCairoImagingOptions = set of TCairoImagingOption;

  TCairoImagingState = (cisReloadDataPending);

  TCairoImagingStates = set of TCairoImagingState;

  TCairoImagingTransparencyMode = (citDefault, citMaskNonAlpha, citForceMaskColor, citNone);
  
  { TCairoImagingPicture }

  TCairoImagingPicture = class
  private
    FData: TImageData;
    FFormatInfo: TImageFormatInfo;
    FOriginalData: TImageData;
    FOnChange: TNotifyEvent;
    FSurface: TCairoImageSurface;
    FOptions: TCairoImagingOptions;
    FTransparent: Boolean;
    FMaskColor: TColor24Rec;
    FStates: TCairoImagingStates;
    FTransparencyMode: TCairoImagingTransparencyMode;
    procedure Changed;
    procedure CreateSurface;
    procedure FreeResources;
    procedure Mask(const AMaskColor: TColor24Rec; ClearAlpha: Boolean);
    procedure PrepareTransparency;
    procedure Reload;
    procedure SetOptions(AValue: TCairoImagingOptions);
    procedure SetTransparencyMode(AValue: TCairoImagingTransparencyMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    procedure UpdateMask;
    property Data: TImageData read FData;
    property Height: Integer read FData.Height;
    property Options: TCairoImagingOptions read FOptions write SetOptions;
    property TransparencyMode: TCairoImagingTransparencyMode read FTransparencyMode write SetTransparencyMode;
    property MaskColor: TColor24Rec read FMaskColor write FMaskColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Surface: TCairoImageSurface read FSurface;
    property Width: Integer read FData.Width;
  end;

implementation

uses
  Cairo;

procedure MultiplyAlphaChannel(Pixel: PColor32Rec; ByteCount: PtrUInt);
begin
  ByteCount := ByteCount shr 2;
  while ByteCount > 0 do
  begin
    case Pixel^.A of
      0:
        begin
          Pixel^.R := 0;
          Pixel^.G := 0;
          Pixel^.B := 0;
        end;
      255:
        begin
          //do nothing
        end;
    else
      Pixel^.R := (Pixel^.R * Pixel^.A) div 255;
      Pixel^.G := (Pixel^.G * Pixel^.A) div 255;
      Pixel^.B := (Pixel^.B * Pixel^.A) div 255;
    end;
    Inc(Pixel);
    Dec(ByteCount);
  end;
end;


{ TCairoImagingPicture }

procedure TCairoImagingPicture.Changed;
begin
  if cisReloadDataPending in FStates then
  begin
    //todo see a way to update the bits directly without need to free the surface or
    //call CloneImage
    //Move(FOriginalData.Bits^, FData.Bits^, FOriginalData.Size);
    CloneImage(FOriginalData, FData);
    PrepareTransparency;
    FSurface.Free;
    CreateSurface;
    Exclude(FStates, cisReloadDataPending);
  end;
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
  FreeImage(FOriginalData);
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
  AMaskColor: TColor24Rec;
  UseAlpha: Boolean;
begin
  UseAlpha := FFormatInfo.HasAlphaChannel;
  case FTransparencyMode of
    citDefault:
      begin
        FTransparent := UseAlpha;
      end;
    citMaskNonAlpha:
      begin
        FTransparent := True;
      end;
    citForceMaskColor:
      begin
        UseAlpha := False;
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
    if UseAlpha then
      MultiplyAlphaChannel(FData.Bits, FData.Size)
    else
    begin
      if cioAutoMaskColor in FOptions then
        AMaskColor := GetPixel32(FData, 0, 0).Color24Rec
      else
        AMaskColor := FMaskColor;
      Mask(AMaskColor, FFormatInfo.HasAlphaChannel);
    end;
  end;
end;

procedure TCairoImagingPicture.SetOptions(
  AValue: TCairoImagingOptions);
var
  EffectiveChange: Boolean;
begin
  if FOptions = AValue then
    Exit;
  EffectiveChange := False;
  if (cioAllowChangesAfterLoad in FOptions) and
    ((cioAutoMaskColor in AValue) <> (cioAutoMaskColor in FOptions)) and
    (FData.Size > 0) and
    (FTransparencyMode in [citForceMaskColor, citMaskNonAlpha]) then
  begin
    EffectiveChange := True;
    Include(FStates, cisReloadDataPending);
  end;
  FOptions := AValue;
  if EffectiveChange then
    Changed;
end;

procedure TCairoImagingPicture.SetTransparencyMode(
  AValue: TCairoImagingTransparencyMode);
begin
  if FTransparencyMode = AValue then
    Exit;
  FTransparencyMode := AValue;
  Reload;
end;

constructor TCairoImagingPicture.Create;
begin
  with FMaskColor do
  begin
    //fuchsia
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
  GetImageFormatInfo(FData.Format, FFormatInfo);
  if (cioAllowChangesAfterLoad in FOptions) and (FOriginalData.Size = 0) then
    CloneImage(FData, FOriginalData);
  PrepareTransparency;
  CreateSurface;
  Changed;
end;

procedure TCairoImagingPicture.UpdateMask;
begin
  if FTransparencyMode in [citMaskNonAlpha, citForceMaskColor] then
    Reload;
end;

procedure TCairoImagingPicture.Reload;
begin
  if (cioAllowChangesAfterLoad in FOptions) and (FData.Size > 0) then
  begin
    Include(FStates, cisReloadDataPending);
    Changed;
  end;
end;


end.

