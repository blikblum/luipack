unit LuiJSONUtils;

{$mode objfpc}{$H+}

//todo: JSONChain (Pluck,Pick,Find,Join)

interface

uses
  Classes, SysUtils, fpjson, db;

type
  //specialized json string types. Useful for e.g. property editors
  TJSONDataString = type TJSONStringType;
  TJSONObjectString = type TJSONStringType;
  TJSONArrayString = type TJSONStringType;

  { TJSONFile }

  //todo: remove it
  TJSONFile = class
  public
    class function Load(const AFileName: String): TJSONData; static;
    class procedure Save(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions); static;
  end;

  TJSONArraySortCompare = function(JSONArray: TJSONArray; Index1, Index2: Integer): Integer;

  TDatasetToJSONOption = (djoSetNull, djoCurrentRecord, djoPreserveCase);

  TDatasetToJSONOptions = set of TDatasetToJSONOption;

  TCopyJSONObjectOption = (cjoSetUndefined, cjoSetNull, cjoOverwrite);

  TCopyJSONObjectOptions = set of TCopyJSONObjectOption;

function CompareJSONData(Data1, Data2: TJSONData): Integer;

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject; const Properties: array of String;
  Options: TCopyJSONObjectOptions = [cjoOverwrite, cjoSetNull]);

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject);

function CreateWeakJSONArray: TJSONArray;

function FindJSONObject(JSONArray: TJSONArray; const ObjProps: array of Variant): TJSONObject;

function FindJSONPath(Data: TJSONData; const Path: String; AType : TJSONType): TJSONData;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONData): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONObject): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONArray): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Integer): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Int64): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Double): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Boolean): Boolean;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: String): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONData): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONObject): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONArray): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Integer): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Int64): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Double): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Boolean): Boolean;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: String): Boolean;

{$ifndef LUIJSONUTILS_WITHOUTDEPRECATED}

function GetJSONIndexOf(JSONArray: TJSONArray; const ItemValue: Variant): Integer; inline; deprecated 'Use LuiJSONHelpers functions instead';

function GetJSONIndexOf(JSONArray: TJSONArray; const ObjProps: array of Variant): Integer; inline; deprecated 'Use LuiJSONHelpers functions instead';

{$endif}

function GetJSONPath(Data: TJSONData; const Path: String; Default: Integer): Integer;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Int64): Int64;

function GetJSONPath(Data: TJSONData; const Path, Default: String): String;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Double): Double;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Boolean): Boolean;

function ReadJSONFile(const AFileName: String): TJSONData;

function SameValue(JSONData: TJSONData; Value: Variant): Boolean;

function SameJSONArray(JSONArray1, JSONArray2: TJSONArray): Boolean;

function SameJSONObject(JSONObj1, JSONObj2: TJSONObject): Boolean;

function SetJSONPath(Data: TJSONData; const Path: String; ValueData: TJSONData): Boolean;

procedure SortJSONArray(JSONArray: TJSONArray);

procedure SortJSONArray(JSONArray: TJSONArray; CompareFn: TJSONArraySortCompare);

procedure SetJSONPropValue(JSONObj: TJSONObject; const PropName: String; Value: Variant; SetNull: Boolean = False);

function StrToJSON(const JSONStr: TJSONStringType): TJSONData;

function StreamToJSON(Stream: TStream): TJSONData;

function TryReadJSONFile(const FileName: String; out JSONData: TJSONData): Boolean;

function TryReadJSONFile(const FileName: String; out JSONArray: TJSONArray): Boolean;

function TryReadJSONFile(const FileName: String; out JSONObject: TJSONObject): Boolean;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONData: TJSONData): Boolean;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONArray: TJSONArray): Boolean;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONObject: TJSONObject): Boolean;

function TryStreamToJSON(Stream: TStream; out JSONData: TJSONData): Boolean;

function TryStreamToJSON(Stream: TStream; out JSONArray: TJSONArray): Boolean;

function TryStreamToJSON(Stream: TStream; out JSONObject: TJSONObject): Boolean;

procedure WriteJSONFile(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions);

function DatasetToJSON(Dataset: TDataset; Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType): TJSONData;

procedure DatasetToJSON(Dataset: TDataset; JSONArray: TJSONArray; Options: TDatasetToJSONOptions);

procedure DatasetToJSON(Dataset: TDataset; JSONObject: TJSONObject; Options: TDatasetToJSONOptions);

procedure DatasetToJSON(Dataset: TDataset; JSONArray: TJSONArray;
  Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType);

procedure DatasetToJSON(Dataset: TDataset; JSONObject: TJSONObject;
  Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType);

implementation

uses
  jsonparser, Variants, math, LuiJSONHelpers, contnrs;

type

  TFieldMap = record
    Field: TField;
    Name: String;
  end;

  TFieldMaps = array of TFieldMap;

function CompareJSONObject(JSONObj1, JSONObj2: TJSONObject): Integer;
var
  Obj1Index, Obj1Count: Integer;
  Obj1Data, Obj2Data: TJSONData;
  PropName: String;
begin
  Obj1Count := JSONObj1.Count;
  Result := CompareValue(Obj1Count, JSONObj2.Count);
  Obj1Index := 0;
  while (Result = 0) and (Obj1Index < Obj1Count) do
  begin
    PropName := JSONObj1.Names[Obj1Index];
    Obj2Data := JSONObj2.Find(PropName);
    Obj1Data := JSONObj1.Items[Obj1Index];
    Result := CompareJSONData(Obj1Data, Obj2Data);
    Inc(Obj1Index);
  end;
end;

function CompareJSONArray(JSONArray1, JSONArray2: TJSONArray): Integer;
var
  ItemIndex, Array1Count: Integer;
begin
  Array1Count := JSONArray1.Count;
  Result := CompareValue(Array1Count, JSONArray2.Count);
  ItemIndex := 0;
  while (Result = 0) and (ItemIndex < Array1Count) do
  begin
    Result := CompareJSONData(JSONArray1.Items[ItemIndex], JSONArray2.Items[ItemIndex]);
    Inc(ItemIndex);
  end;
end;

function CompareJSONData(Data1, Data2: TJSONData): Integer;
var
  Data1Type, Data2Type: TJSONtype;
begin
  if Data1 <> nil then
  begin
    if Data2 <> nil then
    begin
      //data1 <> nil, data2 <> nil
      Data1Type := Data1.JSONType;
      Data2Type := Data2.JSONType;
      if Data1Type = Data2Type then
      begin
        case Data1Type of
        jtNumber:
          Result := CompareValue(Data1.AsInt64, Data2.AsInt64);
        jtString:
          Result := CompareText(Data1.AsString, Data2.AsString);
        jtBoolean:
          Result := CompareValue(Ord(Data1.AsBoolean), Ord(Data2.AsBoolean));
        jtObject:
          Result := CompareJSONObject(TJSONObject(Data1), TJSONObject(Data2));
        jtArray:
          Result := CompareJSONArray(TJSONArray(Data1), TJSONArray(Data2));
        else
          Result := 0;
        end;
      end
      else
      begin
        if Data1Type = jtNull then
          Result := -1
        else
          //Data2type = jtNull or two values have different types and cannot be compared
          //VarCompareValue raises an exception when trying to compare such values
          Result := 1
      end;
    end
    else
    begin
      //data1 <> nil, data2 = nil
      if Data1.JSONType <> jtNull then
        Result := 1
      else
        Result := 0;
    end;
  end
  else
  begin
    if Data2 = nil then
    begin
      //data1 = nil, data2 = nil
      Result := 0
    end
    else
    begin
      //data1 = nil, data2 <> nil
      if Data2.JSONType <> jtNull then
        Result := -1
      else
        Result := 0;
    end;
  end;
end;

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject; const Properties: array of String;
  Options: TCopyJSONObjectOptions);
var
  PropName: String;
  PropIndex, SrcIndex, DestIndex: Integer;
begin
  if Length(Properties) > 0 then
  begin
    for PropIndex := Low(Properties) to High(Properties) do
    begin
      PropName := Properties[PropIndex];
      SrcIndex := SrcObj.IndexOfName(PropName);
      if SrcIndex <> -1 then
      begin
        DestIndex := DestObj.IndexOfName(PropName);
        if SrcObj.Items[SrcIndex].JSONType = jtNull then
        begin
          if cjoOverwrite in Options then
          begin
            if DestIndex <> - 1 then
            begin
              if (cjoSetNull in Options) then
                DestObj.Items[DestIndex] := TJSONNull.Create
              else
                DestObj.Delete(DestIndex);
            end else
            begin
              if (cjoSetNull in Options) then
                DestObj.Nulls[PropName] := True;
            end;
          end else
          begin
            if (DestIndex = -1) and (cjoSetNull in Options) then
              DestObj.Nulls[PropName] := True;
          end;
        end else
        begin
          if cjoOverwrite in Options then
          begin
            if DestIndex = -1 then
              DestObj.Elements[PropName] := SrcObj.Items[SrcIndex].Clone
            else
              DestObj.Items[DestIndex] := SrcObj.Items[SrcIndex].Clone;
          end else
          begin
            if DestIndex = -1 then
              DestObj.Elements[PropName] := SrcObj.Items[SrcIndex].Clone;
          end;
        end;
      end else
      begin
        DestIndex := DestObj.IndexOfName(PropName);
        if cjoOverwrite in Options then
        begin
          if cjoSetUndefined in Options then
          begin
            if DestIndex <> -1 then
              DestObj.Items[DestIndex] := TJSONNull.Create
            else
              DestObj.Nulls[PropName] := True;
          end else
          begin
            if DestIndex <> -1 then
              DestObj.Delete(DestIndex);
          end;
        end else
        begin
          if DestIndex = -1 then
          begin
            if cjoSetUndefined in Options then
              DestObj.Nulls[PropName] := True
          end;
        end;
      end;
    end;
  end
  else
  begin
    //all properties
    if cjoOverwrite in Options then
      CopyJSONObject(SrcObj, DestObj)
    else
    begin
      for SrcIndex := 0 to SrcObj.Count - 1 do
      begin
        PropName := SrcObj.Names[SrcIndex];
        if DestObj.IndexOfName(PropName) = -1 then
          DestObj.Elements[PropName] := SrcObj.Items[SrcIndex].Clone;
      end;
    end;
  end;
end;

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject);
var
  i: Integer;
begin
  for i := 0 to SrcObj.Count - 1 do
    DestObj.Elements[SrcObj.Names[i]] := SrcObj.Items[i].Clone;
end;

type
  TJSONArrayAccess = class(TJSONData)
  private
    FList: TFPObjectList;
  end;

function CreateWeakJSONArray: TJSONArray;
begin
  Result := CreateJSONArray([]);
  TJSONArrayAccess(Result).FList.OwnsObjects := False;
end;

function FindJSONObject(JSONArray: TJSONArray; const ObjProps: array of Variant): TJSONObject;
var
  i: Integer;
begin
  i := JSONArray.IndexOf(ObjProps);
  if i > -1 then
    Result := TJSONObject(JSONArray.Items[i])
  else
    Result := nil;
end;

function FindJSONPath(Data: TJSONData; const Path: String; AType: TJSONType): TJSONData;
begin
  Result := Data.FindPath(Path);
  if Assigned(Result) and (Result.JSONType <> AType) then
    Result := nil;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONData): Boolean;
begin
  PathData := Data.FindPath(Path);
  Result := PathData <> nil;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONObject): Boolean;
var
  FoundData: TJSONData absolute PathData;
begin
  FoundData := FindJSONPath(Data, Path, jtObject);
  Result := PathData <> nil;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathData: TJSONArray): Boolean;
var
  FoundData: TJSONData absolute PathData;
begin
  FoundData := FindJSONPath(Data, Path, jtArray);
  Result := PathData <> nil;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Integer): Boolean;
var
  PathData: TJSONData;
begin
  PathData := FindJSONPath(Data, Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsInteger
  else
    PathValue := 0;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Int64): Boolean;
var
  PathData: TJSONData;
begin
  PathData := FindJSONPath(Data, Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsInt64
  else
    PathValue := 0;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Double): Boolean;
var
  PathData: TJSONData;
begin
  PathData := FindJSONPath(Data, Path, jtNumber);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsFloat
  else
    PathValue := 0;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: Boolean): Boolean;
var
  PathData: TJSONData;
begin
  PathData := FindJSONPath(Data, Path, jtBoolean);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsBoolean
  else
    PathValue := False;
end;

function FindJSONPath(Data: TJSONData; const Path: String; out PathValue: String): Boolean;
var
  PathData: TJSONData;
begin
  PathData := FindJSONPath(Data, Path, jtString);
  Result := PathData <> nil;
  if Result then
    PathValue := PathData.AsString
  else
    PathValue := '';
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONData): Boolean;
begin
  PropData := JSONObj.Find(PropName);
  Result := PropData <> nil;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONObject): Boolean;
var
  Data: TJSONData absolute PropData;
begin
  Data := JSONObj.Find(PropName, jtObject);
  Result := PropData <> nil;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropData: TJSONArray): Boolean;
var
  Data: TJSONData absolute PropData;
begin
  Data := JSONObj.Find(PropName, jtArray);
  Result := PropData <> nil;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Integer): Boolean;
var
  Data: TJSONData;
begin
  Data := JSONObj.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsInteger
  else
    PropValue := 0;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Int64): Boolean;
var
  Data: TJSONData;
begin
  Data := JSONObj.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsInt64
  else
    PropValue := 0;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Double): Boolean;
var
  Data: TJSONData;
begin
  Data := JSONObj.Find(PropName, jtNumber);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsFloat
  else
    PropValue := 0;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := JSONObj.Find(PropName, jtBoolean);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsBoolean
  else
    PropValue := False;
end;

function FindJSONProp(JSONObj: TJSONObject; const PropName: String; out PropValue: String): Boolean;
var
  Data: TJSONData;
begin
  Data := JSONObj.Find(PropName, jtString);
  Result := Data <> nil;
  if Result then
    PropValue := Data.AsString
  else
    PropValue := '';
end;

{$ifndef LUIJSONUTILS_WITHOUTDEPRECATED}

function GetJSONIndexOf(JSONArray: TJSONArray; const ItemValue: Variant): Integer;
begin
  Result := JSONArray.IndexOf(ItemValue);
end;

function GetJSONIndexOf(JSONArray: TJSONArray; const ObjProps: array of Variant): Integer;
begin
  Result := JSONArray.IndexOf(ObjProps);
end;

{$endif}

function GetJSONPath(Data: TJSONData; const Path: String; Default: Integer): Integer;
begin
  if not FindJSONPath(Data, Path, Result) then
    Result := Default;
end;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Int64): Int64;
begin
  if not FindJSONPath(Data, Path, Result) then
    Result := Default;
end;

function GetJSONPath(Data: TJSONData; const Path, Default: String): String;
begin
  if not FindJSONPath(Data, Path, Result) then
    Result := Default;
end;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Double): Double;
begin
  if not FindJSONPath(Data, Path, Result) then
    Result := Default;
end;

function GetJSONPath(Data: TJSONData; const Path: String; Default: Boolean): Boolean;
begin
  if not FindJSONPath(Data, Path, Result) then
    Result := Default;
end;

function ReadJSONFile(const AFileName: String): TJSONData;
var
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  Result := nil;
  //todo: handle UTF-8
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('TJSONFile - File "%s" does not exist', [AFileName]);
  Stream := nil;
  Parser := nil;
  try
    try
      Stream := TFileStream.Create(AFileName, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      Result := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      raise Exception.CreateFmt('TJSONFile - Error loading "%s" : %s', [AFileName, E.Message]);
    on E: EParserError do
    begin
       FreeAndNil(Result);
       raise Exception.CreateFmt('TJSONFile - Error parsing "%s" : %s', [AFileName, E.Message]);
    end;
  end;
end;

//based in TStringList.QuickSort
procedure JSONArrayQuickSort(JSONArray: TJSONArray; L, R: Integer; CompareFn: TJSONArraySortCompare);
var
  Pivot, vL, vR: Integer;
begin
  if R - L <= 1 then begin // a little bit of time saver
    if L < R then
      if CompareFn(JSONArray, L, R) > 0 then
        JSONArray.Exchange(L, R);

    Exit;
  end;

  vL := L;
  vR := R;

  Pivot := L + Random(R - L); // they say random is best

  while vL < vR do begin
    while (vL < Pivot) and (CompareFn(JSONArray, vL, Pivot) <= 0) do
      Inc(vL);

    while (vR > Pivot) and (CompareFn(JSONArray, vR, Pivot) > 0) do
      Dec(vR);

    JSONArray.Exchange(vL, vR);

    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;

  if Pivot - 1 >= L then
    JSONArrayQuickSort(JSONArray, L, Pivot - 1, CompareFn);
  if Pivot + 1 <= R then
    JSONArrayQuickSort(JSONArray, Pivot + 1, R, CompareFn);
end;

function JSONArraySimpleCompare(JSONArray: TJSONArray; Index1, Index2: Integer): Integer;
begin
  Result := CompareJSONData(JSONArray.Items[Index1], JSONArray.Items[Index2]);
end;

function SameValue(JSONData: TJSONData; Value: Variant): Boolean;
begin
  Result := False;
  case VarType(Value) of
    varnull:  Result := JSONData.JSONType = jtNull;
    varstring: Result := (JSONData.JSONType = jtString) and (JSONData.AsString = Value);
    vardouble, vardate: Result := (JSONData.JSONType = jtNumber) and (JSONData.AsFloat = Value);
    varinteger, varlongword, varshortint: Result := (JSONData.JSONType = jtNumber) and (JSONData.AsInteger = Value);
    varint64, varqword: Result := (JSONData.JSONType = jtNumber) and (JSONData.AsInt64 = Value);
    varboolean: Result := (JSONData.JSONType = jtBoolean) and (JSONData.AsBoolean = Value);
    //nil is interpreted as olestream
    varolestr: Result := (Value = nil) and (JSONData.JSONType = jtNull);
  end;
end;

function SameJSONArray(JSONArray1, JSONArray2: TJSONArray): Boolean;
begin
  if (JSONArray1 = nil) or (JSONArray2 = nil) then
    Exit(False);
  Result := CompareJSONArray(JSONArray1, JSONArray2) = 0;
end;

function SameJSONObject(JSONObj1, JSONObj2: TJSONObject): Boolean;
begin
  if (JSONObj1 = nil) or (JSONObj2 = nil) then
    Exit(False);
  Result := CompareJSONObject(JSONObj1, JSONObj2) = 0;
end;

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

function SetJSONPath(Data: TJSONData; const Path: String;
  ValueData: TJSONData): Boolean;

begin
  Result := False;
  case Data.JSONType of
    jtObject:
      Result := SetObjectPath(TJSONObject(Data), Path, ValueData);
    jtArray:
      Result := SetArrayPath(TJSONArray(Data), Path, ValueData);
  end;
end;

procedure SortJSONArray(JSONArray: TJSONArray);
begin
  JSONArrayQuickSort(JSONArray, 0, JSONArray.Count - 1, @JSONArraySimpleCompare);
end;

procedure SortJSONArray(JSONArray: TJSONArray; CompareFn: TJSONArraySortCompare);
begin
  JSONArrayQuickSort(JSONArray, 0, JSONArray.Count - 1, CompareFn);
end;

procedure SetJSONPropValue(JSONObj: TJSONObject; const PropName: String; Value: Variant; SetNull: Boolean = False);
var
  VariantType: tvartype;
begin
  VariantType := VarType(Value);
  case VariantType of
    varnull:
    begin
      if SetNull then
        JSONObj.Elements[PropName] := TJSONNull.Create;
    end;
    varstring: JSONObj.Elements[PropName] := TJSONString.Create(Value);
    vardouble, vardate: JSONObj.Elements[PropName] := TJSONFloatNumber.Create(Value);
    varinteger, varlongword: JSONObj.Elements[PropName] := TJSONIntegerNumber.Create(Value);
    varint64, varqword: JSONObj.Elements[PropName] := TJSONInt64Number.Create(Value);
    varboolean: JSONObj.Elements[PropName] := TJSONBoolean.Create(Value);
  else
    raise Exception.CreateFmt('SetJSONPropValue - Type %d not handled', [VariantType]);
  end
end;

function StrToJSON(const JSONStr: TJSONStringType): TJSONData;
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

function TryReadJSONFile(const FileName: String; out JSONData: TJSONData): Boolean;
begin
  JSONData := nil;
  try
    JSONData := TJSONFile.Load(FileName);
  except
    //
  end;
  Result := JSONData <> nil;
end;

function TryReadJSONFile(const FileName: String; out JSONArray: TJSONArray): Boolean;
var
  Data: TJSONData absolute JSONArray;
begin
  Data := nil;
  try
    Data := TJSONFile.Load(FileName);
  except
    //
  end;
  Result := (Data <> nil) and (Data.JSONType = jtArray);
  if not Result then
    FreeAndNil(Data);
end;

function TryReadJSONFile(const FileName: String; out JSONObject: TJSONObject): Boolean;
var
  Data: TJSONData absolute JSONObject;
begin
  Data := nil;
  try
    Data := TJSONFile.Load(FileName);
  except
    //
  end;
  Result := (Data <> nil) and (Data.JSONType = jtObject);
  if not Result then
    FreeAndNil(Data);
end;

function TryParseJSON(Parser: TJSONParser; var JSONData: TJSONData): Boolean;
begin
  Result := True;
  JSONData := nil;
  try
    JSONData := Parser.Parse;
  except
    Result := False;
  end;
  Result := Result and (JSONData <> nil);
  if not Result then
    FreeAndNil(JSONData);
end;

function TryParseJSON(Parser: TJSONParser; var JSONArray: TJSONArray): Boolean;
var
  JSONData: TJSONData;
begin
  Result := True;
  JSONArray := nil;
  JSONData := nil;
  try
    JSONData := Parser.Parse;
  except
    Result := False;
  end;
  Result := Result and (JSONData <> nil) and (JSONData.JSONType = jtArray);
  if Result then
    JSONArray := TJSONArray(JSONData)
  else
    JSONData.Free;
end;

function TryParseJSON(Parser: TJSONParser; var JSONObject: TJSONObject): Boolean;
var
  JSONData: TJSONData;
begin
  Result := True;
  JSONObject := nil;
  JSONData := nil;
  try
    JSONData := Parser.Parse;
  except
    Result := False;
  end;
  Result := Result and (JSONData <> nil) and (JSONData.JSONType = jtObject);
  if Result then
    JSONObject := TJSONObject(JSONData)
  else
    JSONData.Free;
end;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONData: TJSONData): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr);
  Result := TryParseJSON(Parser, JSONData);
  Parser.Destroy;
end;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONArray: TJSONArray): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr);
  Result := TryParseJSON(Parser, JSONArray);
  Parser.Destroy;
end;

function TryStrToJSON(const JSONStr: TJSONStringType; out JSONObject: TJSONObject): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr);
  Result := TryParseJSON(Parser, JSONObject);
  Parser.Destroy;
end;

function TryStreamToJSON(Stream: TStream; out JSONData: TJSONData): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(Stream);
  Result := TryParseJSON(Parser, JSONData);
  Parser.Destroy;
end;

function TryStreamToJSON(Stream: TStream; out JSONArray: TJSONArray): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(Stream);
  Result := TryParseJSON(Parser, JSONArray);
  Parser.Destroy;
end;

function TryStreamToJSON(Stream: TStream; out JSONObject: TJSONObject): Boolean;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(Stream);
  Result := TryParseJSON(Parser, JSONObject);
  Parser.Destroy;
end;

function StreamToJSON(Stream: TStream): TJSONData;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(Stream);
  try
    Result := Parser.Parse;
  finally
    Parser.Destroy;
  end;
end;

procedure WriteJSONFile(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions);
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := AData.FormatJSON(FormatOptions);
    Output.SaveToFile(AFileName);
  finally
    Output.Destroy;
  end;
end;

procedure CopyFieldsToJSONObject(Fields: TFieldMaps; JSONObject: TJSONObject; SetNull: Boolean);
var
  i: Integer;
begin
  for i := 0 to Length(Fields) - 1 do
    SetJSONPropValue(JSONObject, Fields[i].Name, Fields[i].Field.AsVariant, SetNull);
end;

//todo implement array of arrays
procedure DatasetToJSON(Dataset: TDataset; JSONArray: TJSONArray; Options: TDatasetToJSONOptions);
var
  OldRecNo: Integer;
  RecordData: TJSONObject;
begin
  if Dataset.IsEmpty then
    Exit;
  if djoCurrentRecord in Options then
  begin
    RecordData := TJSONObject.Create;
    DatasetToJSON(Dataset, RecordData, Options);
    JSONArray.Add(RecordData);
  end
  else
  begin
    Dataset.DisableControls;
    OldRecNo := Dataset.RecNo;
    try
      Dataset.First;
      while not Dataset.EOF do
      begin
        RecordData := TJSONObject.Create;
        DatasetToJSON(Dataset, RecordData, Options);
        JSONArray.Add(RecordData);
        Dataset.Next;
      end;
    finally
      Dataset.RecNo := OldRecNo;
      Dataset.EnableControls;
    end;
  end;
end;

function DatasetToJSON(Dataset: TDataset; Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType): TJSONData;
begin
  if not (djoCurrentRecord in Options) then
  begin
    Result := TJSONArray.Create;
    if ExtOptions = '' then
      DatasetToJSON(Dataset, TJSONArray(Result), Options)
    else
      DatasetToJSON(Dataset, TJSONArray(Result), Options, ExtOptions);
  end
  else
  begin
    Result := TJSONObject.Create;
    if ExtOptions = '' then
      DatasetToJSON(Dataset, TJSONObject(Result), Options)
    else
      DatasetToJSON(Dataset, TJSONObject(Result), Options, ExtOptions);
  end;
end;

procedure DatasetToJSON(Dataset: TDataset; JSONObject: TJSONObject; Options: TDatasetToJSONOptions);
var
  i: Integer;
  Field: TField;
  SetNull, PreserveCase: Boolean;
begin
  SetNull := djoSetNull in Options;
  PreserveCase := djoPreserveCase in Options;
  for i := 0 to Dataset.Fields.Count - 1 do
  begin
    Field := Dataset.Fields[i];
    if not PreserveCase then
      SetJSONPropValue(JSONObject, LowerCase(Field.FieldName), Field.AsVariant, SetNull)
    else
      SetJSONPropValue(JSONObject, Field.FieldName, Field.AsVariant, SetNull);
  end;
end;

procedure OptionsToFieldMaps(Dataset: TDataset; FieldsData: TJSONArray; var Result: TFieldMaps);
var
  i: Integer;
  FieldData: TJSONData;
  FieldObj: TJSONObject absolute FieldData;
  Field: TField;
  FieldName: String;
begin
  SetLength(Result, FieldsData.Count);
  for i := 0 to FieldsData.Count - 1 do
  begin
    FieldData := FieldsData.Items[i];
    case FieldData.JSONType of
      jtString:
      begin
        FieldName := FieldData.AsString;
        Field := Dataset.FieldByName(FieldName);
      end;
      jtObject:
      begin
        Field := nil;
        FieldName := FieldObj.Get('mapping', '');
        if FieldName <> '' then
          Field := Dataset.FieldByName(FieldName);
        FieldName := FieldObj.Get('name', FieldName);
        if Field = nil then
        begin
          //mapping not found
          Field := Dataset.FieldByName(FieldName);
        end;
      end;
      else
        raise Exception.Create('DatasetToJSON - Error parsing fields property');
    end;
    Result[i].Field := Field;
    Result[i].Name := FieldName;
  end;
end;

procedure DatasetToJSON(Dataset: TDataset; JSONArray: TJSONArray;
  Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType);
var
  RecordData: TJSONObject;
  ExtOptionsData: TJSONData;
  FieldsData: TJSONArray;
  FieldMaps: TFieldMaps;
  OldRecNo: Integer;
begin
  if Dataset.IsEmpty then
    Exit;
  ExtOptionsData := nil;
  if (ExtOptions <> '') and not TryStrToJSON(ExtOptions, ExtOptionsData) then
    raise Exception.Create('Unable to convert ExtOptions to JSON');
  try
    FieldsData := nil;
    if ExtOptionsData <> nil then
    begin
      case ExtOptionsData.JSONType of
        jtArray:
          FieldsData := TJSONArray(ExtOptionsData);
        jtObject:
          FindJSONProp(TJSONObject(ExtOptionsData), 'fields', FieldsData);
      else
        raise Exception.Create('ExtOptions is an invalid JSON type');
      end;
    end;
    if FieldsData = nil then
      DatasetToJSON(Dataset, JSONArray, Options)
    else
    begin
      OptionsToFieldMaps(Dataset, FieldsData, {%H-}FieldMaps);
      if djoCurrentRecord in Options then
      begin
        RecordData := TJSONObject.Create;
        CopyFieldsToJSONObject(FieldMaps, RecordData, djoSetNull in Options);
        JSONArray.Add(RecordData);
      end
      else
      begin
        Dataset.DisableControls;
        OldRecNo := Dataset.RecNo;
        try
          Dataset.First;
          while not Dataset.EOF do
          begin
            RecordData := TJSONObject.Create;
            CopyFieldsToJSONObject(FieldMaps, RecordData, djoSetNull in Options);
            JSONArray.Add(RecordData);
            Dataset.Next;
          end;
        finally
          Dataset.RecNo := OldRecNo;
          Dataset.EnableControls;
        end;
      end;
    end;
  finally
    ExtOptionsData.Free;
  end;
end;

procedure DatasetToJSON(Dataset: TDataset; JSONObject: TJSONObject;
  Options: TDatasetToJSONOptions; const ExtOptions: TJSONStringType);
var
  ExtOptionsData: TJSONData;
  FieldsData: TJSONArray;
  FieldMaps: TFieldMaps;
begin
  ExtOptionsData := nil;
  if (ExtOptions <> '') and not TryStrToJSON(ExtOptions, ExtOptionsData) then
    raise Exception.Create('Unable to convert ExtOptions to JSON');
  try
    FieldsData := nil;
    if ExtOptionsData <> nil then
    begin
      case ExtOptionsData.JSONType of
        jtArray:
          FieldsData := TJSONArray(ExtOptionsData);
        jtObject:
          FindJSONProp(TJSONObject(ExtOptionsData), 'fields', FieldsData);
      else
        raise Exception.Create('ExtOptions is an invalid JSON type');
      end;
    end;
    if FieldsData = nil then
      DatasetToJSON(Dataset, JSONObject, Options)
    else
    begin
      OptionsToFieldMaps(Dataset, FieldsData, {%H-}FieldMaps);
      CopyFieldsToJSONObject(FieldMaps, JSONObject, djoSetNull in Options);
    end;
  finally
    ExtOptionsData.Free;
  end;
end;

{ TJSONFile }

class function TJSONFile.Load(const AFileName: String): TJSONData;
begin
  Result := ReadJSONFile(AFileName);
end;

class procedure TJSONFile.Save(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions);
begin
  WriteJSONFile(AData, AFileName, FormatOptions);
end;

end.

