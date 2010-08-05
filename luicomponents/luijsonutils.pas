unit LuiJSONUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String): TJSONData;

function StringToJSONData(const JSONStr: TJSONStringType): TJSONData;

implementation

uses
  jsonparser;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsBoolean
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsInteger
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsString
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String): TJSONData;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i]
  else
    Result := nil;
end;

function StringToJSONData(const JSONStr: TJSONStringType): TJSONData;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr);
  try
    Result := Parser.Parse;
  finally
    Parser.Destroy;
  end;
end;

end.

