{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit multiloglaz; 

interface

uses
  multilog, registermultilog, logtreeview, sharedlogger, multiloglcl, 
    filechannel, ipcchannel, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registermultilog', @registermultilog.Register); 
end; 

initialization
  RegisterPackage('multiloglaz', @Register); 
end.
