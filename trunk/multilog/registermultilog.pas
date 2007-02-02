{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit registermultilog;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf, logtreeview;
  
procedure Register;

implementation

procedure RegisterUnitLogTreeView;
begin
  RegisterComponents('MultiLog',[TLogTreeView]);
end;  

procedure Register;

begin
  RegisterUnit('logtreeview',@RegisterUnitLogTreeView);
end;

initialization

{$i icons.lrs}
 
end.
