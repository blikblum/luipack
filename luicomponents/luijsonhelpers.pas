unit LuiJSONHelpers;

{$mode objfpc}{$H+}

interface

uses
  fpjson;

type

{ TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  public
    function Find(const PropName: String; out PropData: TJSONData): Boolean; overload;
    function Find(const PropName: String; out PropData: TJSONObject): Boolean; overload;
    function Find(const PropName: String; out PropData: TJSONArray): Boolean; overload;
    function Find(const PropName: String; out PropValue: Integer): Boolean; overload;
    function Find(const PropName: String; out PropValue: Int64): Boolean; overload;
    function Find(const PropName: String; out PropValue: Double): Boolean; overload;
    function Find(const PropName: String; out PropValue: Boolean): Boolean; overload;
    function Find(const PropName: String; out PropValue: String): Boolean; overload;
  end;

  { TJSONArrayHelper }

  TJSONArrayHelper = class helper for TJSONArray
  public
    function IndexOf(const ItemValue: Variant): Integer; overload;
    function IndexOf(const Properties: array of Variant): Integer; overload;
  end;

implementation

uses
  sysutils, LuiJSONUtils;

type
  JSONHelperException = class(Exception);

{ TJSONObjectHelper }

function TJSONObjectHelper.Find(const PropName: String; out PropData: TJSONData): Boolean;
begin
  PropData := Self.Find(PropName);
  Result := PropData <> nil;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropData: TJSONObject): Boolean;
var
  Data: TJSONData absolute PropData;
begin
  Data := Self.Find(PropName, jtObject);
  Result := PropData <> nil;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropData: TJSONArray): Boolean;
var
  Data: TJSONData absolute PropData;
begin
  Data := Self.Find(PropName, jtArray);
  Result := PropData <> nil;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropValue: Integer): Boolean;
var
  Data: TJSONData;
begin
  Data := Self.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsInteger
  else
    PropValue := 0;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropValue: Int64): Boolean;
var
  Data: TJSONData;
begin
  Data := Self.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsInt64
  else
    PropValue := 0;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropValue: Double): Boolean;
var
  Data: TJSONData;
begin
  Data := Self.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsFloat
  else
    PropValue := 0;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropValue: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := Self.Find(PropName, jtBoolean);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsBoolean
  else
    PropValue := False;
end;

function TJSONObjectHelper.Find(const PropName: String; out PropValue: String): Boolean;
var
  Data: TJSONData;
begin
  Data := Self.Find(PropName, jtString);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsString
  else
    PropValue := '';
end;

{ TJSONArrayHelper }

function TJSONArrayHelper.IndexOf(const ItemValue: Variant): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if SameValue(Items[Result], ItemValue) then
      Exit;
  end;
  Result := -1;
end;

function TJSONArrayHelper.IndexOf(const Properties: array of Variant): Integer;
var
  PropCount, i: Integer;
  ItemData, PropData: TJSONData;
  ObjMatches: Boolean;
begin
  PropCount := Length(Properties);
  Result := -1;
  if odd(PropCount) then
    raise JSONHelperException.Create('Properties must have even length');
  PropCount := PropCount div 2;
  for Result := 0 to Self.Count - 1 do
  begin
    ItemData := Self.Items[Result];
    ObjMatches := False;
    if ItemData.JSONType = jtObject then
    begin
      for i := 0 to PropCount - 1 do
      begin
        PropData := ItemData.FindPath(Properties[i * 2]);
        ObjMatches := (PropData <> nil) and SameValue(PropData, Properties[(i * 2) + 1]);
        if not ObjMatches then
          break;
      end;
    end;
    if ObjMatches then
      Exit;
  end;
  Result := -1;
end;

end.

