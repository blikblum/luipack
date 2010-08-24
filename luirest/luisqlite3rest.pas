unit LuiSqlite3REST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, Sqlite3Wrapper, LuiRESTSQL;

type

  { TSqlite3RESTDatabase }

  TSqlite3RESTDatabase = class(TSqlite3Database, ISQLResponseBuilder)
  public
    procedure BuildJSON(const Sql: String; Response: TResponse);
  end;



implementation

uses
  fpjson;

{ TSqlite3RESTDatabase }

procedure TSqlite3RESTDatabase.BuildJSON(const Sql: String; Response: TResponse);
var
  Reader: TSqlite3DataReader;
  Data: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
begin
  Data := TJSONArray.Create;
  Reader := TSqlite3DataReader.Create;
  try
    Prepare(Sql, Reader);
    while Reader.Step do
    begin
      Obj := TJSONObject.Create;
      for i := 0 to Reader.FieldCount - 1 do
      begin
        //todo improve it
        Obj.Strings[Reader.FieldNames[i]] := Reader.GetString(i);
      end;
      Data.Add(Obj);
    end;
  finally
    //todo write directly to stream to avoid TStrings parsing
    Response.Contents.Text := Data.AsJSON;
    Data.Destroy;
    Reader.Destroy;
  end;
end;

end.

