{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit luicomponents; 

interface

uses
    LuiConfig, IniConfigProvider, register_luicomponents, LuiOrderedDataset, 
  LuiRecordBuffer, LuiMessages, LuiDateUtils, LuiRTTIUtils, LuiStrUtils, 
  LuiMiscUtils, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_luicomponents', @register_luicomponents.Register); 
end; 

initialization
  RegisterPackage('luicomponents', @Register); 
end.
