unit LuiJSONSchema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject): Boolean;

implementation

function DataIsType(Data: TJSONData; const DataType: String): Boolean;
begin
  case DataType of
    'object': Result := Data.JSONType = jtObject;
    'array': Result := Data.JSONType = jtArray;
    'integer': Result := (Data.JSONType = jtNumber) and (TJSONNumber(Data).NumberType in [ntInt64, ntQWord, ntInteger]);
    'number': Result := Data.JSONType = jtNumber;
    'string': Result := Data.JSONType = jtString;
    'boolean': Result := Data.JSONType = jtBoolean;
    'null': Result := Data.JSONType = jtNull;
  end;
end;

function ValidateType(Data: TJSONData; SchemaData: TJSONObject): Boolean;
var
  TypeData: TJSONData;
  TypeDataArray: TJSONArray absolute TypeData;
  i: Integer;
begin
  Result := False;
  TypeData := SchemaData.Find('type');
  if TypeData = nil then
    Exit;
  case TypeData.JSONType of
    jtString:
      Result := DataIsType(Data, TypeData.AsString);
    jtArray:
      begin
        for i := 0 to TypeDataArray.Count - 1 do
        begin
          if TypeDataArray[i].JSONType = jtString then
            Result := DataIsType(Data, TypeDataArray[i].AsString)
          else
            ;//todo: error
          if Result then
            Exit;
        end;
      end;
  end;
end;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject): Boolean;
begin
  Result := ValidateType(Data, SchemaData);
end;

end.

