unit Sqlite3Wrapper;

{
  Sqlite3Wrapper is a thin wrapper around sqlite3 api

  Copyright (C) 2008 Luiz Americo Pereira Camara
  pascalive@bol.com.br

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
  Classes, SysUtils, sqlite3;
  
const
  SQLITE_OK = sqlite3.SQLITE_OK;
  SQLITE_ROW = sqlite3.SQLITE_ROW;

type

  TSqlite3DataReader = class;
  
  { TSqlite3Query }

  TSqlite3Query = class
  private
    FStatement: Pointer;
    FReturnCode: Integer;
    procedure Reset;
  public
    function GetInteger(Column: Integer = 0; Finalize: Boolean = True): Integer;
    procedure GetList(List: TStrings; FillObjects: Boolean = False; Finalize: Boolean = True);
    function GetString(Column: Integer = 0; Finalize: Boolean = True): String;
    function IsEmpty: Boolean;
    function IsNull(Column: Integer = 0; Finalize: Boolean = False): Boolean;
    
  end;

  { TSqlite3Database }

  TSqlite3Database = class(TComponent)
  private
    FFileName: String;
    FHandle: Pointer;
    FReturnCode: Integer;
    FQuery: TSqlite3Query;
    FSharedHandle: Boolean;
    procedure SetFileName(const AValue: String);
    procedure SetHandle(AValue: Pointer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;
    function Query(const SQL: String): TSqlite3Query;
    procedure Open;
    procedure ExecSql(const SQL: String);
    procedure ExecSql(const SQL: String; Arguments: array of const);
    function LastInsertRowId: Integer;
    procedure Prepare(const SQL: String; Reader: TSqlite3DataReader);
    function ReturnString: String;
    property FileName: String read FFileName write SetFileName;
    property Handle: Pointer read FHandle write SetHandle;
    property ReturnCode: Integer read FReturnCode;
  end;

  { TSqlite3DataReader }

  TSqlite3DataReader = class
  private
    FStatement: Pointer;
    function GetFieldNames(Index: Integer): String;
  protected
    function GetFieldCount: Integer;
  public
    destructor Destroy; override;
    procedure Finalize;
    function GetFieldIndex(const FieldName: String): Integer;
    function GetInteger(AFieldIndex: Integer): Integer; //inline;
    function GetInteger(const FieldName: String): Integer;
    function GetString(AFieldIndex: Integer): String; //inline;
    function GetString(const FieldName: String): String;
    function Step: Boolean;
    property FieldCount: Integer read GetFieldCount;
    property FieldNames[Index: Integer]: String read GetFieldNames;
  end;

implementation

{ TSqlite3Database }

procedure TSqlite3Database.SetFileName(const AValue: String);
begin
  if FFileName <> AValue then
  begin
    Close;
    FFileName := AValue;
  end;
end;

procedure TSqlite3Database.SetHandle(AValue: Pointer);
begin
  if FHandle <> AValue then
  begin
    Close;
    FHandle := AValue;
    FSharedHandle := True;
  end;
end;

constructor TSqlite3Database.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery := TSqlite3Query.Create;
end;

destructor TSqlite3Database.Destroy;
begin
  FQuery.Destroy;
  Close;
  inherited Destroy;
end;

procedure TSqlite3Database.Close;
begin
  if FHandle <> nil then
  begin
    if not FSharedHandle then
      sqlite3_close(FHandle);
    FHandle := nil;
  end;
end;

function TSqlite3Database.Query(const SQL: String): TSqlite3Query;
begin
  FReturnCode := sqlite3_prepare(FHandle, PChar(SQL), -1, @FQuery.FStatement, nil);
  if FReturnCode = SQLITE_OK then
  begin
    Result := FQuery;
    FQuery.Reset;
  end
  else
    raise Exception.Create('Error in Query: ' + ReturnString);
end;

procedure TSqlite3Database.Open;
var
  stm: Pointer;
  ErrorStr: String;
begin
  if FHandle <> nil then
    Exit;
  FReturnCode := sqlite3_open(PChar(FFileName), @FHandle);
  if FReturnCode = SQLITE_OK then
  begin
    FReturnCode := sqlite3_prepare(FHandle, 'Select Name from sqlite_master LIMIT 1', -1, @stm, nil);
    sqlite3_finalize(stm);
  end;
  FSharedHandle := False;
  if FReturnCode <> SQLITE_OK then
  begin
    //is necessary to get ReturnString before sqlite3_close call
    ErrorStr := 'Error opening "' + FFileName + '": ' + ReturnString;
    sqlite3_close(FHandle);
    FHandle := nil;
    raise Exception.Create(ErrorStr);
  end;
end;

procedure TSqlite3Database.ExecSql(const SQL: String);
var
  stm: Pointer;
begin
  FReturnCode := sqlite3_prepare(FHandle, PChar(SQL), -1, @stm, nil);
  if FReturnCode = SQLITE_OK then
  begin
    FReturnCode := sqlite3_step(stm);
    sqlite3_finalize(stm);
  end
  else
    raise Exception.Create('Error in ExecSql: ' + ReturnString);
end;

procedure TSqlite3Database.ExecSql(const SQL: String;
  Arguments: array of const);
begin
  ExecSql(Format(SQL, Arguments));
end;

function TSqlite3Database.LastInsertRowId: Integer;
begin
  Result := sqlite3_last_insert_rowid(FHandle);
end;

procedure TSqlite3Database.Prepare(const SQL: String; Reader: TSqlite3DataReader);
begin
  FReturnCode := sqlite3_prepare(FHandle, PChar(SQL), -1, @Reader.FStatement, nil);
  if FReturnCode <> SQLITE_OK then
    raise Exception.Create('Error in Prepare: ' + ReturnString);
end;

function TSqlite3Database.ReturnString: String;
begin
  case FReturnCode of
    SQLITE_OK           : Result := 'SQLITE_OK';
    SQLITE_ERROR        : Result := 'SQLITE_ERROR';
    SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL';
    SQLITE_PERM         : Result := 'SQLITE_PERM';
    SQLITE_ABORT        : Result := 'SQLITE_ABORT';
    SQLITE_BUSY         : Result := 'SQLITE_BUSY';
    SQLITE_LOCKED       : Result := 'SQLITE_LOCKED';
    SQLITE_NOMEM        : Result := 'SQLITE_NOMEM';
    SQLITE_READONLY     : Result := 'SQLITE_READONLY';
    SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT';
    SQLITE_IOERR        : Result := 'SQLITE_IOERR';
    SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT';
    SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND';
    SQLITE_FULL         : Result := 'SQLITE_FULL';
    SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN';
    SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL';
    SQLITE_EMPTY        : Result := 'SQLITE_EMPTY';
    SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA';
    SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG';
    SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT';
    SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH';
    SQLITE_MISUSE       : Result := 'SQLITE_MISUSE';
    SQLITE_NOLFS        : Result := 'SQLITE_NOLFS';
    SQLITE_AUTH         : Result := 'SQLITE_AUTH';
    SQLITE_FORMAT       : Result := 'SQLITE_FORMAT';
    SQLITE_RANGE        : Result := 'SQLITE_RANGE';
    SQLITE_ROW          : Result := 'SQLITE_ROW';
    SQLITE_NOTADB       : Result := 'SQLITE_NOTADB';
    SQLITE_DONE         : Result := 'SQLITE_DONE';
  else
    Result := 'Unknow Return Value';
  end;
  Result := Result + ' - ' + sqlite3_errmsg(FHandle);
end;

function TSqlite3DataReader.Step: Boolean;
begin
  Result := sqlite3_step(FStatement) = SQLITE_ROW;
end;

function TSqlite3DataReader.GetFieldIndex(const FieldName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to FieldCount - 1 do
    if stricomp(PChar(FieldName), sqlite3_column_name(FStatement,i)) = 0 then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TSqlite3DataReader.GetFieldNames(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FieldCount) then
    Result := sqlite3_column_name(FStatement, Index)
  else
    raise Exception.Create('GetFieldNames - Index out of bounds');
end;

function TSqlite3DataReader.GetFieldCount: Integer;
begin
  Result := sqlite3_column_count(FStatement);
end;

destructor TSqlite3DataReader.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TSqlite3DataReader.Finalize;
begin
  if FStatement <> nil then
  begin
    sqlite3_finalize(FStatement);
    FStatement := nil;
  end;
end;

function TSqlite3DataReader.GetInteger(AFieldIndex: Integer): Integer;
begin
  Result := sqlite3_column_int(FStatement,AFieldIndex);
end;

function TSqlite3DataReader.GetInteger(const FieldName: String): Integer;
var
  i: Integer;
begin
  i := GetFieldIndex(FieldName);
  if i <> -1 then
    Result := sqlite3_column_int(FStatement,i)
  else
    raise Exception.Create('TSqlite3Wrapper - Field "' + FieldName + '" not found');
end;

function TSqlite3DataReader.GetString(AFieldIndex: Integer): String;
begin
  Result := String(sqlite3_column_text(FStatement, AFieldIndex));
end;

function TSqlite3DataReader.GetString(const FieldName: String): String;
var
  i: Integer;
begin
  i := GetFieldIndex(FieldName);
  if i <> -1 then
    Result := String(sqlite3_column_text(FStatement, i))
  else
    raise Exception.Create('TSqlite3Wrapper - Field "'+FieldName+'" not found');
end;

{ TSqlite3Query }

procedure TSqlite3Query.Reset;
begin
  FReturnCode := sqlite3_step(FStatement);
end;

function TSqlite3Query.GetInteger(Column: Integer = 0; Finalize: Boolean = True): Integer;
begin
  Result := sqlite3_column_int(FStatement, Column);
  if Finalize then
    sqlite3_finalize(FStatement);
end;

procedure TSqlite3Query.GetList(List: TStrings; FillObjects: Boolean; Finalize: Boolean = True);

  procedure FillStrings;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      List.Add(String(sqlite3_column_text(FStatement, 0)));
      FReturnCode := sqlite3_step(FStatement);
    end;
  end;

  procedure FillStringsAndObjects;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      List.AddObject(String(sqlite3_column_text(FStatement, 0)),
        TObject(PtrInt(sqlite3_column_int(FStatement, 1))));
      FReturnCode := sqlite3_step(FStatement);
    end;
  end;

begin
  if (List <> nil) and (sqlite3_column_count(FStatement) > 0) then
  begin
    if FillObjects and (sqlite3_column_count(FStatement) > 1) then
      FillStringsAndObjects
    else
      FillStrings;
  end;
  if Finalize then
    sqlite3_finalize(FStatement);
end;

function TSqlite3Query.GetString(Column: Integer = 0; Finalize: Boolean = True): String;
begin
  Result := String(sqlite3_column_text(FStatement, Column));
  if Finalize then
    sqlite3_finalize(FStatement);
end;

function TSqlite3Query.IsEmpty: Boolean;
begin
  Result := FReturnCode <> SQLITE_ROW;
end;

function TSqlite3Query.IsNull(Column: Integer = 0; Finalize: Boolean = False): Boolean;
begin
  Result := sqlite3_column_type(FStatement, Column) = SQLITE_NULL;
  if Finalize then
    sqlite3_finalize(FStatement);
end;

end.

