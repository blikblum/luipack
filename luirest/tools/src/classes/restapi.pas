unit RESTAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONClasses;

type

  { TRESTAPI }

  TRESTAPI = class(TComponent)
  private
    FBaseURL: String;
    FData: TJSONObject;
    FEndPointListData: TJSONArray;
    FFileName: String;
    FIsModified: Boolean;
    procedure ParseGroupData(GroupData: TJSONObject);
    procedure ParseEndPointData(EndPointData: TJSONObject);
    function GetBaseURL: String;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    procedure Modified;
    procedure Save;
    property BaseURL: String read FBaseURL;
    property Data: TJSONObject read FData;
    property EndPointListData: TJSONArray read FEndPointListData;
    property FileName: String read FFileName;
    property IsModified: Boolean read FIsModified;
  end;

implementation

uses
  LuiJSONUtils, LuiJSONHelpers, strutils, math;

{ TRESTAPI }

procedure TRESTAPI.ParseGroupData(GroupData: TJSONObject);
var
  ItemsData: TJSONArray;
  ItemData: TJSONObject;
  i: Integer;
begin
  //todo account to differences in v2 and v2.1 formats item <> items
  if GroupData.Find('item', ItemsData) then
  begin
    for i := 0 to ItemsData.Count - 1 do
    begin
      ItemData := ItemsData.Objects[i];
      if ItemData.IndexOfName('request') >= 0 then
        FEndPointListData.Add(ItemData)
      else
        ParseGroupData(ItemData);
    end;
  end;
end;

procedure TRESTAPI.ParseEndPointData(EndPointData: TJSONObject);
begin

end;

function FindCommonString(const Str1, Str2: String): String;
var
  StrLen, CharPos: Integer;
begin
  StrLen:= Min(Length(Str1), Length(Str2));
  CharPos := 1;
  while CharPos <= StrLen do
  begin
    if Str1[CharPos] <> Str2[CharPos] then
      break;
    Inc(CharPos);
  end;
  Result := Copy(Str1, 1, CharPos - 1);
end;

function TRESTAPI.GetBaseURL: String;
var
  i: Integer;
  EndPointData: TJSONObject;
  URL: String;
begin
  Result := '';
  if FEndPointListData.Count = 0 then
    Exit;
  Result := FEndPointListData.GetPath('[0].request.url', '');
  for i := 1 to FEndPointListData.Count - 1 do
  begin
    EndPointData := FEndPointListData.Objects[i];
    URL := EndPointData.GetPath('request.url', '');
    Result := FindCommonString(Result, URL);
  end;
end;

destructor TRESTAPI.Destroy;
begin
  FEndPointListData.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TRESTAPI.LoadFromFile(const FileName: String);
begin
  FreeAndNil(FEndPointListData);
  FreeAndNil(FData);
  FIsModified := False;
  if not TryReadJSONFile(FileName, FData) then
    raise Exception.CreateFmt('Unable to open file "%s"', [FileName]);
  FFileName := FileName;
  FEndPointListData := CreateWeakJSONArray;
  ParseGroupData(FData);
  FBaseURL := GetBaseURL;
end;

procedure TRESTAPI.Modified;
begin
  FIsModified := True;
end;

procedure TRESTAPI.Save;
begin
  WriteJSONFile(FData, FFileName, []);
  FIsModified := False;
end;

end.

