{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vtvutils_package;

interface

uses
  VTComboEditLink, VTController, VirtualNodeInterfaces, VTObjectTree, VTJSON, 
  register_vtvutils, VTJSONEx, CollectionVirtualTreeMediator, JSONFilterView, 
  JSONCollectionView, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_vtvutils', @register_vtvutils.Register);
end;

initialization
  RegisterPackage('vtvutils_package', @Register);
end.
