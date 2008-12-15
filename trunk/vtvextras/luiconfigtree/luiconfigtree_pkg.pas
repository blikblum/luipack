{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit luiconfigtree_pkg; 

interface

uses
LuiConfigTree, register_luiconfigtree, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_luiconfigtree', @register_luiconfigtree.Register); 
end; 

initialization
  RegisterPackage('luiconfigtree_pkg', @Register); 
end.
