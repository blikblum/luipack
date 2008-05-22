{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit vampyreimaging_base; 

interface

uses
  Imaging, ImagingBitmap, ImagingCanvases, ImagingClasses, ImagingColors, 
    ImagingDds, ImagingExport, ImagingFormats, ImagingGif, ImagingIO, 
    ImagingJpeg, ImagingNetworkGraphics, ImagingPortableMaps, ImagingTarga, 
    ImagingTypes, ImagingUtility, ImagingExtras, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('vampyreimaging_base', @Register); 
end.
