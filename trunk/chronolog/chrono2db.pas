unit Chrono2Db;


{
  Functions to save TChronoLog results to a sqlite3 db file

  Copyright (C) 2005 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ChronoLog;

function SaveLogToDb(ALog: TChronoLog;const AFilename: String):boolean;

implementation

uses
  sqlite3ds, db;
   
function SaveLogToDb(ALog: TChronoLog; const AFilename: String): boolean;
var
  ADb:TSqlite3Dataset;
  i,LastSessionId:Integer;
begin
  ADb := TSqlite3Dataset.Create(nil);
  with ADb do
  begin
    FileName := AFilename;
    if FileExists(Filename) then
    begin
      if (not TableExists('sessions')) or (not TableExists('results')) then
      begin
        Destroy;
        raise Exception.Create(AFilename + ' is not a valid database');
      end;
    end
    else
    begin
      //Create Sessions Table
      FieldDefs.Clear;
      FieldDefs.Add('Code',ftAutoInc);
      FieldDefs.Add('Name',ftString);
      FieldDefs.Add('Comments',ftString);
      FieldDefs.Add('SystemInfo',ftString);
      FieldDefs.Add('Date',ftDateTime);
      CreateTable('sessions');
      // Create Results table
      FieldDefs.Clear;
      FieldDefs.Add('Code',ftAutoInc);
      FieldDefs.Add('Action',ftString);
      FieldDefs.Add('Time',ftLargeInt);
      FieldDefs.Add('SessionId',ftInteger);
      CreateTable('results');
    end;
    TableName := 'sessions';
    Open;
    Append;
    FieldByName('Name').AsString := ALog.SessionName;
    FieldByName('Comments').AsString := ALog.Comments;
    FieldByName('Date').AsDateTime := Now;
    FieldByName('SystemInfo').AsString := ALog.GetSystemInfo;
    Post;
    LastSessionId := FieldByName('Code').AsInteger;
    ApplyUpdates;
    Close;
    Sql := '';
    TableName := 'results';
    Open;
    for i:= 0 to ALog.Count - 1 do
    begin
      Append;
      FieldByName('Action').AsString := ALog.GetActionStr(i);
      TLargeintField(FieldByName('Time')).AsLargeInt := ALog.AsMicroSecond(i);
      FieldbyName('SessionId').AsInteger := LastSessionId;
      Post;
    end;
    ApplyUpdates;
    Destroy;
  end;
end;


end.

