unit LuiJSONHelpers;

{$mode objfpc}{$H+}

interface

uses
  fpjson;

type

  { TJSONDataHelper }

  TJSONDataHelper = class helper for TJSONData
  public
    function FindPath(const Path: String; AType : TJSONType): TJSONData; overload;
    function FindPath(const Path: String; out PathData: TJSONData): Boolean; overload;
    function FindPath(const Path: String; out PathData: TJSONObject): Boolean; overload;
    function FindPath(const Path: String; out PathData: TJSONArray): Boolean; overload;
    function FindPath(const Path: String; out PathValue: Integer): Boolean; overload;
    function FindPath(const Path: String; out PathValue: Int64): Boolean; overload;
    function FindPath(const Path: String; out PathValue: Double): Boolean; overload;
    function FindPath(const Path: String; out PathValue: Boolean): Boolean; overload;
    function FindPath(const Path: String; out PathValue: String): Boolean; overload;
    function GetPath(const Path: String; Default: Integer): Integer; overload;
    function GetPath(const Path: String; Default: Int64): Int64; overload;
    function GetPath(const Path, Default: String): String; overload;
    function GetPath(const Path: String; Default: Double): Double; overload;
    function GetPath(const Path: String; Default: Boolean): Boolean; overload;
    function SetPath(const Path: String; ValueData: TJSONData): Boolean;
  end;

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
    procedure Merge(SourceData: TJSONObject);
  end;

  { TJSONArrayHelper }

  TJSONArrayHelper = class helper for TJSONArray
  public
    function Filter(const Expression: String; out ArrayData: TJSONArray): Boolean;
    function Find(const Properties: array of Variant; out ItemData: TJSONObject): Boolean; overload;
    function IndexOf(const ItemValue: Variant): Integer; overload;
    function IndexOf(const Properties: array of Variant): Integer; overload;
  end;

implementation

uses
  sysutils, LuiJSONUtils, LuiJSONClasses;

type
  JSONHelperException = class(Exception);

function SetArrayPath(Data: TJSONArray; const Path: String;
  ValueData: TJSONData): Boolean;
begin
  Result := False;
end;

function SetObjectPath(Data: TJSONObject; const Path: String;
  ValueData: TJSONData): Boolean;
var
  PropName: String;
  Len, StartPos, EndPos: Integer;
  PropData: TJSONData;
begin
  Result := False;
  //adapted from fpjson
  if Path = '' then
    Exit;
  Len := Length(Path);
  StartPos := 1;
  while (StartPos < Len) and (Path[StartPos] = '.') do
    Inc(StartPos);
  EndPos := StartPos;
  while (EndPos <= Len) and (not (Path[EndPos] in ['.', '['])) do
    Inc(EndPos);
  PropName := Copy(Path, StartPos, EndPos - StartPos);
  if PropName = '' then
    Exit;
  //path completely handled
  if EndPos > Len then
  begin
    Result := True;
    Data.Elements[PropName] := ValueData;
  end
  else
  begin
    PropData := Data.Find(PropName);
    if PropData <> nil then
    begin
      case PropData.JSONType of
        jtObject:
        begin
          if Path[EndPos] = '.' then
            Result := SetObjectPath(TJSONObject(PropData),
              Copy(Path, EndPos, Len - EndPos + 1), ValueData);
        end;
        jtArray:
          begin
            if Path[EndPos] = '[' then
              Result := SetArrayPath(TJSONArray(PropData),
                Copy(Path, EndPos, Len - EndPos + 1), ValueData);
          end;
      end;
    end;
  end;
end;


{ TJSONDataHelper }

function TJSONDataHelper.FindPath(const Path: String; AType: TJSONType): TJSONData;
begin
  Result := Self.FindPath(Path);
  if Assigned(Result) and (Result.JSONType <> AType) then
    Result := nil;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathData: TJSONData): Boolean;
begin
  PathData := Self.FindPath(Path);
  Result := PathData <> nil;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathData: TJSONObject): Boolean;
var
  FoundData: TJSONData absolute PathData;
begin
  FoundData := Self.FindPath(Path, jtObject);
  Result := PathData <> nil;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathData: TJSONArray): Boolean;
var
  FoundData: TJSONData absolute PathData;
begin
  FoundData := Self.FindPath(Path, jtArray);
  Result := PathData <> nil;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathValue: Integer): Boolean;
var
  PathData: TJSONData;
begin
  PathData := Self.FindPath(Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsInteger
  else
    PathValue := 0;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathValue: Int64): Boolean;
var
  PathData: TJSONData;
begin
  PathData := Self.FindPath(Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsInt64
  else
    PathValue := 0;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathValue: Double): Boolean;
var
  PathData: TJSONData;
begin
  PathData := Self.FindPath(Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsFloat
  else
    PathValue := 0;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathValue: Boolean): Boolean;
var
  PathData: TJSONData;
begin
  PathData := Self.FindPath(Path, jtBoolean);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsBoolean
  else
    PathValue := False;
end;

function TJSONDataHelper.FindPath(const Path: String; out PathValue: String): Boolean;
var
  PathData: TJSONData;
begin
  PathData := Self.FindPath(Path, jtString);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsString
  else
    PathValue := '';
end;

function TJSONDataHelper.GetPath(const Path: String; Default: Integer): Integer;
begin
  if not Self.FindPath(Path, Result) then
    Result := Default;
end;

function TJSONDataHelper.GetPath(const Path: String; Default: Int64): Int64;
begin
  if not Self.FindPath(Path, Result) then
    Result := Default;
end;

function TJSONDataHelper.GetPath(const Path, Default: String): String;
begin
  if not Self.FindPath(Path, Result) then
    Result := Default;
end;

function TJSONDataHelper.GetPath(const Path: String; Default: Double): Double;
begin
  if not Self.FindPath(Path, Result) then
    Result := Default;
end;

function TJSONDataHelper.GetPath(const Path: String; Default: Boolean): Boolean;
begin
  if not Self.FindPath(Path, Result) then
    Result := Default;
end;

function TJSONDataHelper.SetPath(const Path: String; ValueData: TJSONData): Boolean;
begin
  Result := False;
  case Self.JSONType of
    jtObject:
      Result := SetObjectPath(TJSONObject(Self), Path, ValueData);
    jtArray:
      Result := SetArrayPath(TJSONArray(Self), Path, ValueData);
  end;
end;

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

procedure TJSONObjectHelper.Merge(SourceData: TJSONObject);
var
  SrcIndex, DestIndex: Integer;
  PropName: String;
  SrcPropData, DestPropData: TJSONData;
begin
  for SrcIndex := 0 to SourceData.Count - 1 do
  begin
    PropName := SourceData.Names[SrcIndex];
    SrcPropData := SourceData.Items[SrcIndex];
    DestIndex := IndexOfName(PropName);
    if DestIndex = -1 then
      Elements[PropName] := SrcPropData.Clone
    else
    begin
      DestPropData := Items[DestIndex];
      if ((SrcPropData.JSONType = jtObject) and (DestPropData.JSONType = jtObject)) then
        TJSONObject(DestPropData).Merge(TJSONObject(SrcPropData));
    end;
  end;
end;

{ TJSONArrayHelper }

function TJSONArrayHelper.Filter(const Expression: String; out ArrayData: TJSONArray): Boolean;
var
  ExpressionParser: TJSONExpressionParser;
  i: Integer;
begin
  Result := False;
  ArrayData := CreateWeakJSONArray;
  try
    ExpressionParser := TJSONExpressionParser.Create(nil);
    try
      for i := 0 to Count - 1 do
      begin
        ExpressionParser.Data := Objects[i];
        // todo: find a way to avoid reparsing the expresion and only update the variable values
        ExpressionParser.Expression := Expression;
        if ExpressionParser.AsBoolean then
          ArrayData.Add(ExpressionParser.Data);
        ExpressionParser.Clear;
      end;
    finally
      ExpressionParser.Destroy;
    end;
  except
    ArrayData.Destroy;
  end;
  Result := True;
end;

function TJSONArrayHelper.Find(const Properties: array of Variant; out ItemData: TJSONObject): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Properties);
  if i > -1 then
    ItemData := TJSONObject(Items[i])
  else
    ItemData := nil;
  Result := ItemData <> nil;
end;

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

