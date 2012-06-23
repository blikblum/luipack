unit LuiJSONUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, db;

type

  { TJSONFile }

  TJSONFile = class
  private
    FData: TJSONData;
    FFileName: String;
  public
    destructor Destroy; override;
    procedure Load;
    class function Load(const AFileName: String): TJSONData;
    class procedure Save(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions);
    property Data: TJSONData read FData ;
    property FileName: String read FFileName write FFileName;
  end;

  TJSONArraySortCompare = function(JSONArray: TJSONArray; Index1, Index2: Integer): Integer;


function CompareJSONData(Data1, Data2: TJSONData): Integer;

//todo: implement Overwrite, RemoveNull, SetUndefinedAsNull
procedure CopyJSONObject(SrcObj, DestObj: TJSONObject; const Properties: array of String; SetUndefined: Boolean = False);

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject);

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Double): Double;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String): TJSONData;

function GetJSONPropValue(JSONObj: TJSONObject; const PropName: String): Variant;

function JSONArrayIndexOf(JSONArray: TJSONArray; const ItemValue: Variant): Integer;

function JSONArrayIndexOf(JSONArray: TJSONArray; const Properties: array of Variant): Integer;

procedure RemoveJSONProp(JSONObj: TJSONObject; const PropName: String);

function SameValue(JSONData: TJSONData; Value: Variant): Boolean;

procedure SortJSONArray(JSONArray: TJSONArray);

procedure SortJSONArray(JSONArray: TJSONArray; CompareFn: TJSONArraySortCompare);

procedure SetJSONPropValue(JSONObj: TJSONObject; const PropName: String; Value: Variant);

function FileToJSONData(const FileName: String): TJSONData;

function StringToJSONData(const JSONStr: TJSONStringType): TJSONData;

function StreamToJSONData(Stream: TStream): TJSONData;

procedure DatasetToJSONArray(JSONArray: TJSONArray; Dataset: TDataset);

function DatasetToJSONData(Dataset: TDataset; const Options: String): TJSONData;

procedure DatasetToJSONObject(JSONObject: TJSONObject; Dataset: TDataset);

implementation

uses
  jsonparser, Variants, math;

function CompareJSONData(Data1, Data2: TJSONData): Integer;
const
  RelationshipIntegerMap: array[TVariantRelationship] of Integer = (0, -1, 1, 0);
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
        else
          Result := 0;
        end;
      end
      else
      begin
        if (Data1Type in [jtObject, jtArray]) or (Data2Type in [jtObject, jtArray]) then
          Result := 0
        else
        begin
          if Data1Type = jtNull then
            Result := -1
          else if Data2Type = jtNull then
            Result := 1
          else
            Result := RelationshipIntegerMap[VarCompareValue(Data1.Value, Data2.Value)];
        end;
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
  SetUndefined: Boolean);
var
  PropertyName: String;
  i, j: Integer;
begin
  for i := 0 to Length(Properties) - 1 do
  begin
    PropertyName := Properties[i];
    j := SrcObj.IndexOfName(PropertyName);
    if j <> -1 then
      DestObj.Elements[PropertyName] := SrcObj.Items[j].Clone
    else
    begin
      if SetUndefined then
        DestObj.Nulls[PropertyName] := True;
    end;
  end;
end;

procedure CopyJSONObject(SrcObj, DestObj: TJSONObject);
var
  i: Integer;
begin
  for i := 0 to SrcObj.Count - 1 do
    DestObj.Add(SrcObj.Names[i], SrcObj.Items[i].Clone);
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := GetJSONProp(JSONObj, PropName);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
    Result := Data.AsBoolean
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;
var
  Data: TJSONData;
begin
  Data := GetJSONProp(JSONObj, PropName);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
    Result := Data.AsInteger
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Double): Double;
var
  Data: TJSONData;
begin
  Data := GetJSONProp(JSONObj, PropName);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
    Result := Data.AsFloat
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;
var
  Data: TJSONData;
begin
  Data := GetJSONProp(JSONObj, PropName);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
    Result := Data.AsString
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

function GetJSONPropValue(JSONObj: TJSONObject; const PropName: String): Variant;
var
  Data: TJSONData;
begin
  Data := GetJSONProp(JSONObj, PropName);
  if Data = nil then
    Result := Null
  else
    Result := Data.Value;
end;

function JSONArrayIndexOf(JSONArray: TJSONArray; const ItemValue: Variant): Integer;
begin
  for Result := 0 to JSONArray.Count - 1 do
  begin
    if SameValue(JSONArray.Items[Result], ItemValue) then
      Exit;
  end;
  Result := -1;
end;

function JSONArrayIndexOf(JSONArray: TJSONArray; const Properties: array of Variant): Integer;
var
  PropCount, i: Integer;
  ItemData, PropData: TJSONData;
  ItemObj: TJSONObject absolute ItemData;
  ObjMatches: Boolean;
begin
  PropCount := Length(Properties);
  Result := -1;
  if odd(PropCount) then
    raise Exception.Create('JSONArrayIndexOf - Properties must have even length');
  PropCount := PropCount div 2;
  for Result := 0 to JSONArray.Count - 1 do
  begin
    ItemData := JSONArray.Items[Result];
    ObjMatches := False;
    if ItemData.JSONType = jtObject then
    begin
      for i := 0 to PropCount - 1 do
      begin
        PropData := GetJSONProp(ItemObj, Properties[i * 2]);
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

procedure RemoveJSONProp(JSONObj: TJSONObject; const PropName: String);
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    JSONObj.Delete(i);
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
    varinteger, varlongword: Result := (JSONData.JSONType = jtNumber) and (JSONData.AsInteger = Value);
    varint64, varqword: Result := (JSONData.JSONType = jtNumber) and (JSONData.AsInt64 = Value);
    varboolean: Result := (JSONData.JSONType = jtBoolean) and (JSONData.AsBoolean = Value);
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

procedure SetJSONPropValue(JSONObj: TJSONObject; const PropName: String; Value: Variant);
var
  VariantType: tvartype;
begin
  VariantType := VarType(Value);
  case VariantType of
    varnull: JSONObj.Elements[PropName] := TJSONNull.Create;
    varstring: JSONObj.Elements[PropName] := TJSONString.Create(Value);
    vardouble, vardate: JSONObj.Elements[PropName] := TJSONFloatNumber.Create(Value);
    varinteger, varlongword: JSONObj.Elements[PropName] := TJSONIntegerNumber.Create(Value);
    varint64, varqword: JSONObj.Elements[PropName] := TJSONInt64Number.Create(Value);
    varboolean: JSONObj.Elements[PropName] := TJSONBoolean.Create(Value);
  else
    raise Exception.CreateFmt('SetJSONPropValue - Type %d not handled', [VariantType]);
  end
end;

function FileToJSONData(const FileName: String): TJSONData;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := StreamToJSONData(Stream);
  finally
    Stream.Destroy;
  end;
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

function StreamToJSONData(Stream: TStream): TJSONData;
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

//todo implement array of arrays
//todo implement null as undefined
procedure DatasetToJSONArray(JSONArray: TJSONArray; Dataset: TDataset);
var
  OldRecNo: Integer;
  RecordData: TJSONObject;
begin
  if Dataset.IsEmpty then
    Exit;
  Dataset.DisableControls;
  OldRecNo := Dataset.RecNo;
  try
    Dataset.First;
    while not Dataset.EOF do
    begin
      RecordData := TJSONObject.Create;
      DatasetToJSONObject(RecordData, Dataset);
      JSONArray.Add(RecordData);
      Dataset.Next;
    end;
  finally
    Dataset.RecNo := OldRecNo;
    Dataset.EnableControls;
  end;
end;

function DatasetToJSONData(Dataset: TDataset; const Options: String): TJSONData;
var
  OptionsData: TJSONData;
begin
  OptionsData := StringToJSONData(Options);
  try
    if OptionsData = nil then
    begin
      Result := TJSONArray.Create;
      DatasetToJSONArray(TJSONArray(Result), Dataset);
    end
    else
    begin
      case OptionsData.JSONType of
        jtObject:
        begin
          if GetJSONProp(TJSONObject(OptionsData), 'copyrecord', False) then
          begin
            Result := TJSONObject.Create;
            DatasetToJSONObject(TJSONObject(Result), Dataset);
          end
          else
          begin
            Result := TJSONArray.Create;
            DatasetToJSONArray(TJSONArray(Result), Dataset);
          end;
        end;
        jtArray:
          ;
      end;
    end;
  finally
    OptionsData.Free;
  end;
end;

procedure DatasetToJSONObject(JSONObject: TJSONObject; Dataset: TDataset);
var
  i: Integer;
  Field: TField;
begin
  for i := 0 to Dataset.Fields.Count - 1 do
  begin
    Field := Dataset.Fields[i];
    //todo: add option to preserve case
    //todo: add option to map fields
    SetJSONPropValue(JSONObject, LowerCase(Field.FieldName), Field.AsVariant);
  end;
end;

{ TJSONFile }

destructor TJSONFile.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TJSONFile.Load;
var
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  //todo: handle UTF-8
  if not FileExists(FFileName) then
    raise Exception.CreateFmt('TJSONFile - File "%s" does not exist', [FFileName]);
  FreeAndNil(FData);
  Stream := nil;
  Parser := nil;
  try
    try
      Stream := TFileStream.Create(FFileName, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      FData := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      raise Exception.CreateFmt('TJSONFile - Error loading "%s" : %s', [FFileName, E.Message]);
    on E: EParserError do
    begin
       FData.Free;
       raise Exception.CreateFmt('TJSONFile - Error parsing "%s" : %s', [FFileName, E.Message]);
    end;
  end;
end;

class function TJSONFile.Load(const AFileName: String): TJSONData;
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
       Result.Free;
       raise Exception.CreateFmt('TJSONFile - Error parsing "%s" : %s', [AFileName, E.Message]);
    end;
  end;
end;

class procedure TJSONFile.Save(AData: TJSONData; const AFileName: String; FormatOptions: TFormatOptions);
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

end.

