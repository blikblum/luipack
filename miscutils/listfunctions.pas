unit ListFunctions;

{ String Lists functions

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}


{$mode objfpc}
{$H+}

interface

uses
  classes,sysutils;
  

procedure StripList(List,ExcludeList:TStringList; TrimName: Boolean);

implementation

procedure StripList(List, ExcludeList:TStringList;  TrimName: Boolean);
{
  function MatchExclude (const ALine: String): Boolean;
  var
    k: Integer;
  begin
    Result:=False;
    for k:= 0 to ExcludeList.Count-1 do
    begin
      if Pos(ExcludeList[k],NomeAtual) > 0 then
      begin
        Result:=True;
        Exit;
      end;  
    end;  
  end;  
}
var 
  i,iExclude,PosStrip:Integer;
  NomeAtual:String;
begin
  With List do
  begin
    Add('----Junk Line -> It Must be Removed');
    i:=1;
    while  i < Count do
    begin
      NomeAtual:=Trim(Strings[i]);
      iExclude:=0;
      if ExcludeList <> nil then
        while iExclude < ExcludeList.Count do
        begin  
          if Pos(ExcludeList[iExclude],NomeAtual) <> 0 then
          begin
            Delete(i);
            NomeAtual:=Trim(Strings[i]);
            iExclude:=0;
          end
          else
            inc(iExclude);        
        end;
      if TrimName then
      begin
        PosStrip:=Pos('(',NomeAtual);
        If PosStrip <> 0 then
          System.Delete(NomeAtual,PosStrip,Length(NomeAtual));
        PosStrip:=Pos('[',NomeAtual);  
        If PosStrip <> 0 then
          System.Delete(NomeAtual,PosStrip,Length(NomeAtual));
      end;
      
      Strings[i]:=NomeAtual;
      if NomeAtual = Strings[i-1] then 
      begin
        Delete(i);      
      end  
      else 
        inc(i);
    end;
    Delete(Count-1);
  end;
end;

end. 
