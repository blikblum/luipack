unit Sqlite3JSONFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3;

procedure RegisterJSONFunctions(SqliteHandle: Pointer);

implementation

uses
  fpjson, LuiJSONUtils, Sqlite3Ext;

const
  SQLITE_DETERMINISTIC = $800;

procedure JSONPath(context: PSqlite3_Context; argc: LongInt; argv: PPSqlite3_Value); cdecl;
var
  PathData, FieldData: TJSONData;
  StrValue: String;
begin
  sqlite3_result_null(context);
  //todo: report error when calling with wrong arguments
  if (argc <> 2) or (sqlite3_value_type(argv[0]) <> SQLITE_TEXT) or
    (sqlite3_value_type(argv[1]) <> SQLITE_TEXT) then
    Exit;
  if not TryStrToJSON(sqlite3_value_text(argv[0]), FieldData) then
    Exit;
  PathData := FieldData.FindPath(sqlite3_value_text(argv[1]));
  if PathData <> nil then
  begin
    case PathData.JSONType of
      jtNumber:
        begin
          case TJSONNumber(PathData).NumberType of
            ntFloat:
              sqlite3_result_double(context, PathData.AsFloat);
            ntInteger:
              sqlite3_result_int(context, PathData.AsInteger);
            ntInt64:
              sqlite3_result_int64(context, PathData.AsInt64);
          end;
        end;
      jtString:
        begin
          StrValue := PathData.AsString;
          sqlite3_result_text(context, PChar(StrValue), Length(StrValue), sqlite3_destructor_type(SQLITE_TRANSIENT));
        end;
      jtBoolean:
        begin
          if PathData.AsBoolean then
            sqlite3_result_int(context, 1)
          else
            sqlite3_result_int(context, 0);
        end;
      jtArray, jtObject:
        begin
          StrValue := PathData.AsJSON;
          sqlite3_result_text(context, PChar(StrValue), Length(StrValue), sqlite3_destructor_type(SQLITE_TRANSIENT));
        end;
    end;
  end;
  //todo: cache field data
  FieldData.Destroy;
end;


procedure RegisterJSONFunctions(SqliteHandle: Pointer);
begin
  sqlite3_create_function(SqliteHandle, 'JSONPath', 2, SQLITE_UTF8 or SQLITE_DETERMINISTIC, nil, @JSONPath, nil, nil);
end;

end.

