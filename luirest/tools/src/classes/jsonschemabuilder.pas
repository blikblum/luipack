unit JSONSchemaBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TPrimitivePropertyEvent = procedure(const Path: String; DefinitionData: TJSONObject) of object;

  { TJSONSchemaBuilder }

  TJSONSchemaBuilder = class
  private
    FOnPrimitiveProperty: TPrimitivePropertyEvent;
    FCurrentPath: TStringList;
    function CreateArrayDefinition(Data: TJSONArray): TJSONObject;
    function CreateObjectDefinition(Data: TJSONObject): TJSONObject;
    function CreatePrimitiveDefinition(Data: TJSONData): TJSONObject;
    procedure CreateArrayDefinition(Data: TJSONArray; Result: TJSONObject);
    procedure CreateObjectDefinition(Data: TJSONObject; Result: TJSONObject);
    procedure CreatePrimitiveDefinition(Data: TJSONData; Result: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Build(Data: TJSONData): TJSONObject;
    property OnPrimitiveProperty: TPrimitivePropertyEvent read FOnPrimitiveProperty write FOnPrimitiveProperty;
  end;

function CreateJSONSchema(Data: TJSONData): TJSONObject;

implementation

uses
  LuiJSONHelpers, LuiJSONUtils;

function CreateJSONSchema(Data: TJSONData): TJSONObject;
var
  Builder: TJSONSchemaBuilder;
begin
  Builder := TJSONSchemaBuilder.Create;
  try
    Result := Builder.Build(Data);
  finally
    Builder.Destroy;
  end;
end;

procedure TJSONSchemaBuilder.CreatePrimitiveDefinition(Data: TJSONData; Result: TJSONObject);
var
  PropType: String;
begin
  case Data.JSONType of
    jtNumber:
      begin
        case TJSONNumber(Data).NumberType of
          ntInt64, ntInteger: PropType := 'integer';
          ntFloat: PropType := 'number';
        end;
      end;
    jtString:
      PropType := 'string';
    jtNull:
      PropType := 'null';
    jtBoolean:
      PropType := 'boolean';
  end;
  Result.Strings['type'] := PropType;
  if Assigned(FOnPrimitiveProperty) then
    FOnPrimitiveProperty(FCurrentPath.DelimitedText, Result);
end;

constructor TJSONSchemaBuilder.Create;
begin
  FCurrentPath := TStringList.Create;
  FCurrentPath.Delimiter := '.';
end;

destructor TJSONSchemaBuilder.Destroy;
begin
  FCurrentPath.Destroy;
  inherited Destroy;
end;

function TJSONSchemaBuilder.CreateArrayDefinition(Data: TJSONArray): TJSONObject;
begin
  Result := TJSONObject.Create;
  CreateArrayDefinition(Data, Result);
end;

function TJSONSchemaBuilder.CreateObjectDefinition(Data: TJSONObject): TJSONObject;
begin
  Result := TJSONObject.Create;
  CreateObjectDefinition(Data, Result);
end;

function TJSONSchemaBuilder.CreatePrimitiveDefinition(Data: TJSONData): TJSONObject;
begin
  Result := TJSONObject.Create;
  CreatePrimitiveDefinition(Data, Result);
end;

procedure TJSONSchemaBuilder.CreateArrayDefinition(Data: TJSONArray; Result: TJSONObject);
var
  ItemsSchemaData, PropertiesData, ItemData, SampleData: TJSONObject;
  ItemPropData: TJSONData;
  NullProps: TStringList;
  i, k: Integer;
  PropName, PropType: String;
begin
  ItemsSchemaData := TJSONObject.Create;
  Result.Strings['type'] := 'array';
  Result.Objects['items'] := ItemsSchemaData;

  //check each item format
  //todo: improve heuristics
  NullProps := TStringList.Create;
  NullProps.Duplicates := dupIgnore;
  SampleData := TJSONObject.Create;
  try
    for i := 0 to Data.Count - 1 do
    begin
      ItemData := Data.Objects[i];
      for k := 0 to ItemData.Count - 1 do
      begin
        ItemPropData := ItemData.Items[k];
        PropName := ItemData.Names[k];
        if ItemPropData.JSONType = jtNull then
          NullProps.Add(PropName)
        else
        begin
          if SampleData.IndexOfName(PropName) = -1  then
            SampleData.Add(PropName, ItemPropData.Clone);
        end;
      end;
    end;
    for i := 0 to NullProps.Count - 1 do
    begin
      PropName := NullProps[i];
      if SampleData.IndexOfName(PropName) = -1 then
        SampleData.Nulls[PropName] := True;
    end;
    CreateObjectDefinition(SampleData, ItemsSchemaData);
    if ItemsSchemaData.Find('properties', PropertiesData) then
    begin
      for i := 0 to NullProps.Count - 1 do
      begin
        PropName := NullProps[i];
        PropType := PropertiesData.GetPath(PropName + '.type', 'null');
        if PropType <> 'null' then
          SetJSONPath(PropertiesData, PropName + '.type', TJSONArray.Create([PropType, 'null']));
      end;
    end;
  finally
    NullProps.Destroy;
    SampleData.Destroy;
  end;
end;

procedure TJSONSchemaBuilder.CreateObjectDefinition(Data: TJSONObject; Result: TJSONObject);
var
  PropertiesData: TJSONObject;
  RequiredData: TJSONArray;
  i: Integer;
  PropName: String;
  PropData: TJSONData;
begin
  PropertiesData := TJSONObject.Create;
  RequiredData := TJSONArray.Create;
  Result.Strings['type'] := 'object';
  Result.Objects['properties'] := PropertiesData;
  Result.Arrays['required'] := RequiredData;
  for i := 0 to Data.Count - 1 do
  begin
    PropName := Data.Names[i];
    PropData := Data.Items[i];
    RequiredData.Add(PropName);
    FCurrentPath.Add(PropName);
    case PropData.JSONType of
      jtNumber, jtNull, jtString, jtBoolean:
        PropertiesData.Add(PropName, CreatePrimitiveDefinition(PropData));
      jtObject:
        PropertiesData.Add(PropName, CreateObjectDefinition(TJSONObject(PropData)));
      jtArray:
        PropertiesData.Add(PropName, CreateArrayDefinition(TJSONArray(PropData)));
    end;
    FCurrentPath.Delete(FCurrentPath.Count - 1);
  end;
end;

{ TJSONSchemaBuilder }

function TJSONSchemaBuilder.Build(Data: TJSONData): TJSONObject;
begin
  Result := TJSONObject.Create([
    '$schema', 'http://json-schema.org/draft-04/schema#'
  ]);
  case Data.JSONType of
    jtObject: CreateObjectDefinition(TJSONObject(Data), Result);
    jtArray: CreateArrayDefinition(TJSONArray(Data), Result);
    jtNull, jtNumber, jtBoolean, jtString: CreatePrimitiveDefinition(Data, Result);
  else
    raise Exception.Create('Invalid json data type');
  end;     
end;

end.

