unit QuickDefinitionParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  { TModelFieldsQuickDefinitionParser }

  TModelFieldsQuickDefinitionParser = class
  private
    FSource: TStrings;
  public
    constructor Create(Source: TStrings);
    function Parse: TJSONArray;
  end;

implementation

uses
  strutils;

function StringToFieldType(const S: String): String;
begin
  if AnsiMatchText(S, ['INT', 'INTEGER']) then
    Result := 'dftInteger'
  else if AnsiMatchText(S, ['FLOAT', 'REAL']) then
    Result := 'dftFloat'
  else if AnsiMatchText(S, ['STR', 'STRING']) then
    Result := 'dftString'
  else if AnsiMatchText(S, ['MEMO', 'TEXT']) then
    Result := 'dtMemo'
  else if AnsiMatchText(S, ['BOOL', 'BOOLEAN']) then
    Result := 'dftBoolean'
  else if AnsiMatchText(S, ['INT64', 'LARGEINT']) then
    Result := 'dftLargeInt'
  else if AnsiMatchText(S, ['DATE']) then
    Result := 'dftDate'
  else if AnsiMatchText(S, ['DATETIME']) then
    Result := 'dftDateTime'
  else if AnsiMatchText(S, ['TIME']) then
    Result := 'dftTime'
  else if AnsiMatchText(S, ['BLOB']) then
    Result := 'dftBlob'
  else
    raise Exception.CreateFmt('Unable to map "%s" to a field type', [S]);
end;

procedure ParseFieldInfo(const S: String; FieldData: TJSONObject);
var
  FirstSpacePos, SecondSpacePos: Integer;
  Segment: String;
begin
  FirstSpacePos := Pos(' ', S);
  if FirstSpacePos = 0 then
  begin
    FieldData.Add('FieldName', S);
  end
  else
  begin
    SecondSpacePos := PosEx(' ', S, FirstSpacePos + 1);
    if SecondSpacePos = 0 then
    begin
      Segment := Trim(Copy(S, FirstSpacePos + 1, Length(S)));
      FieldData.Add('FieldType', StringToFieldType(Segment));
    end
    else
    begin
      Segment := Copy(S, FirstSpacePos + 1, SecondSpacePos - FirstSpacePos - 1);
      FieldData.Add('FieldType', StringToFieldType(Segment));
      Segment := Trim(Copy(S, SecondSpacePos + 1, Length(S)));
      if Segment <> '' then
        FieldData.Add('DisplayLabel', Segment);
    end;
  end;
end;

procedure ParseConstraintInfo(const S: String; ConstraintsData: TJSONObject);
var
  SpacePos: Integer;
  Name, Value: String;
  NumberValue: Double;
begin
  SpacePos := Pos(' ', S);
  if SpacePos = 0 then
  begin
    if AnsiMatchText(S, ['KEY', 'PRIMARYKEY']) then
      ConstraintsData.Booleans['primarykey'] := True
    else
      raise Exception.CreateFmt('Invalid constraint declaration: "%s"', [S]);
  end
  else
  begin
    Name := Copy(S, 1, SpacePos - 1);
    Value := Copy(S, SpacePos + 1, Length(S));


  end;
end;

{ TModelFieldsQuickDefinitionParser }

constructor TModelFieldsQuickDefinitionParser.Create(Source: TStrings);
begin
  FSource := Source;
end;

function TModelFieldsQuickDefinitionParser.Parse: TJSONArray;
var
  i: Integer;
  Line: String;
  FieldData, ConstraintsData: TJSONObject;
begin
  if FSource = nil then
    raise Exception.Create('FSource = nil');
  FieldData := nil;
  ConstraintsData := nil;
  Result := TJSONArray.Create;
  try
    for i := 0 to FSource.Count - 1 do
    begin
      Line := TrimRight(FSource[i]);
      if Length(Line) = 0 then
        continue;
      if Line[0] <> ' ' then
      begin
        //new field
        if ConstraintsData <> nil then
        begin
          if (ConstraintsData.Count > 0) and (FieldData <> nil) then
            FieldData.Add('constraints', ConstraintsData.AsJSON);
          FreeAndNil(ConstraintsData);
        end;
        FieldData := TJSONObject.Create;
        Result.Add(FieldData);
        ParseFieldInfo(Line, FieldData);
      end
      else
      begin
        //constraint
        if ConstraintsData = nil then
          ConstraintsData := TJSONObject.Create;
        ParseConstraintInfo(Trim(Line), ConstraintsData);
      end;
    end;
    if ConstraintsData <> nil then
    begin
      if (ConstraintsData.Count > 0) and (FieldData <> nil) then
        FieldData.Add('constraints', ConstraintsData.AsJSON);
      FreeAndNil(ConstraintsData);
    end;
  except
     FreeAndNil(Result);
  end;
end;

end.

