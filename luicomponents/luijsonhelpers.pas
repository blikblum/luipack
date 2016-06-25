unit LuiJSONHelpers;

{$mode objfpc}{$H+}

interface

uses
  fpjson;

type

{ TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  public

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

