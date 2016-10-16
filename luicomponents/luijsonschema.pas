unit LuiJSONSchema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject): Boolean;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject; out Errors: TStrings): Boolean;

implementation

uses
  math, LuiJSONHelpers, RegExpr;

type

  { TJSONSchemaValidator }

  TJSONSchemaValidator = class
  private
    FRegex: TRegExpr;
    FRootSchemaData: TJSONObject;
    FErrors: TStrings;
    procedure AddError(const Error: String);
    procedure AddError(const Error: String; const Args: array of const);
    procedure CheckAdditionalProperties(Data, PropertiesData, PatternPropertiesData: TJSONObject; AdditionalPropertiesData: TJSONData);
    procedure CheckPatternProperties(Data, PatternPropertiesData: TJSONObject);
    procedure CheckType(Data, TypeData: TJSONData);
    procedure DoValidate(Data: TJSONData; SchemaData: TJSONObject);
    function MatchesPattern(const Str, Pattern: String): Boolean;
    function PropertyMatchesPattern(const PropName: String;
      PatternPropertiesData: TJSONObject): Boolean;
    procedure ValidateArray(Data: TJSONArray; SchemaData: TJSONObject);
    procedure ValidateArrayList(Data: TJSONArray; SchemaData: TJSONObject);
    procedure ValidateArrayTuple(Data, TupleSchemaData: TJSONArray);
    procedure ValidateNumber(Number: Double; SchemaData: TJSONObject);
    procedure ValidateObject(Data, SchemaData: TJSONObject);
    procedure ValidateString(const Str: String; SchemaData: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Validate(Data: TJSONData; SchemaData: TJSONObject): Boolean;
    property Errors: TStrings read FErrors;
  end;

function DataIsType(Data: TJSONData; const DataType: String): Boolean;
begin
  case DataType of
    'object': Result := Data.JSONType = jtObject;
    'array': Result := Data.JSONType = jtArray;
    'integer': Result := (Data.JSONType = jtNumber) and
       (TJSONNumber(Data).NumberType in [ntInt64, {$IF FPC_FULLVERSION >= 30000 } ntQWord,{$ENDIF} ntInteger]);
    'number': Result := Data.JSONType = jtNumber;
    'string': Result := Data.JSONType = jtString;
    'boolean': Result := Data.JSONType = jtBoolean;
    'null': Result := Data.JSONType = jtNull;
  end;
end;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject): Boolean;
var
  Validator: TJSONSchemaValidator;
begin
  Validator := TJSONSchemaValidator.Create;
  try
    Result := Validator.Validate(Data, SchemaData);
  finally
    Validator.Destroy;
  end;
end;

function ValidateJSON(Data: TJSONData; SchemaData: TJSONObject; out
  Errors: TStrings): Boolean;
var
  Validator: TJSONSchemaValidator;
begin
  Errors := nil;
  Validator := TJSONSchemaValidator.Create;
  try
    Result := Validator.Validate(Data, SchemaData);
    if not Result then
    begin
      Errors := Validator.Errors;
      Validator.FErrors := nil;
    end;
  finally
    Validator.Destroy;
  end;
end;

{ TJSONSchemaValidator }

procedure TJSONSchemaValidator.AddError(const Error: String);
begin
  FErrors.Add(Error);
end;

procedure TJSONSchemaValidator.AddError(const Error: String;
  const Args: array of const);
begin
  FErrors.Add(Format(Error, Args));
end;

function TJSONSchemaValidator.PropertyMatchesPattern(const PropName: String; PatternPropertiesData: TJSONObject): Boolean;
var
  i: Integer;
  Pattern: String;
begin
  Result := False;
  for i := 0 to PatternPropertiesData.Count - 1 do
  begin
    Pattern := PatternPropertiesData.Names[i];
    Result := MatchesPattern(PropName, Pattern);
    if Result then
      exit;
  end;
end;

procedure TJSONSchemaValidator.CheckAdditionalProperties(Data, PropertiesData,
  PatternPropertiesData: TJSONObject; AdditionalPropertiesData: TJSONData);
var
  i: Integer;
  AdditionalSchemaData: TJSONObject;
  PropName: String;
begin
  case AdditionalPropertiesData.JSONType of
    jtBoolean:
      begin
        if AdditionalPropertiesData.AsBoolean then
          Exit;

        AdditionalSchemaData := nil;
      end;
    jtObject:
      begin
        AdditionalSchemaData := TJSONObject(AdditionalPropertiesData);
      end;
  else
    //todo: schema error
    exit;
  end;

  for i := 0 to Data.Count - 1 do
  begin
    PropName := Data.Names[i];
    if (PropertiesData = nil) or (PropertiesData.IndexOfName(PropName) = -1) then
    begin
      if not ((PatternPropertiesData <> nil) and PropertyMatchesPattern(PropName, PatternPropertiesData)) then
      begin
        if AdditionalSchemaData <> nil then
          DoValidate(Data.Items[i], AdditionalSchemaData)
        else
          AddError('additionalProperties %s', [PropName]);
      end;
    end;
  end;
end;

procedure TJSONSchemaValidator.CheckPatternProperties(Data,
  PatternPropertiesData: TJSONObject);
var
  i, k: Integer;
  Pattern: String;
  SchemaData: TJSONObject;
begin
  for i := 0 to PatternPropertiesData.Count - 1 do
  begin
    Pattern := PatternPropertiesData.Names[i];
    SchemaData := PatternPropertiesData.Items[i] as TJSONObject;
    for k := 0 to Data.Count - 1 do
    begin
      if MatchesPattern(Data.Names[k], Pattern) then
        DoValidate(Data.Items[k], SchemaData);
    end;
  end;
end;

procedure TJSONSchemaValidator.DoValidate(Data: TJSONData;
  SchemaData: TJSONObject);
var
  TypeData: TJSONData;
begin
  TypeData := SchemaData.Find('type');
  if TypeData <> nil then
    CheckType(Data, TypeData);
  case Data.JSONType of
    jtObject:
      ValidateObject(TJSONObject(Data), SchemaData);
    jtString:
      ValidateString(Data.AsString, SchemaData);
    jtNumber:
      ValidateNumber(Data.AsFloat, SchemaData);
    jtArray:
      ValidateArray(TJSONArray(Data), SchemaData);
  end;
end;

function TJSONSchemaValidator.MatchesPattern(const Str, Pattern: String): Boolean;
begin
  if FRegex = nil then
    FRegex := TRegExpr.Create;
  //todo: caches regex?
  FRegex.Expression := Pattern;
  Result := FRegex.Exec(Str);
end;

procedure TJSONSchemaValidator.ValidateArray(Data: TJSONArray;
  SchemaData: TJSONObject);
var
  ItemsData: TJSONData;
  ItemCountLimit: Integer;
begin
  if SchemaData.Find('items', ItemsData) then
  begin
    case ItemsData.JSONType of
      jtObject: ValidateArrayList(Data, TJSONObject(ItemsData));
      jtArray: ValidateArrayTuple(Data, TJSONArray(ItemsData));
    end;
  end;

  if SchemaData.Find('minItems', ItemCountLimit) and (Data.Count < ItemCountLimit) then
    AddError('minItems %d', [ItemCountLimit]);

  if SchemaData.Find('maxItems', ItemCountLimit) and (Data.Count > ItemCountLimit) then
    AddError('maxItems %d', [ItemCountLimit]);
end;

procedure TJSONSchemaValidator.ValidateArrayList(Data: TJSONArray;
  SchemaData: TJSONObject);
var
  ErrorCount, i: Integer;
begin
  ErrorCount := FErrors.Count;
  for i := 0 to Data.Count - 1 do
  begin
    DoValidate(Data.Items[i], SchemaData);
    if FErrors.Count > ErrorCount then
      Break;
  end;
end;

procedure TJSONSchemaValidator.ValidateArrayTuple(Data,
  TupleSchemaData: TJSONArray);
var
  ItemCount, i: Integer;
begin
  ItemCount := Min(Data.Count, TupleSchemaData.Count);
  for i := 0 to ItemCount - 1 do
    DoValidate(Data.Items[i], TupleSchemaData.Objects[i]);
end;

function IsMultipleOf(X, Y: Double): Boolean;
begin
  Result := frac(X/Y) = 0;
end;

procedure TJSONSchemaValidator.ValidateNumber(Number: Double;
  SchemaData: TJSONObject);
var
  NumberLimit: Double;
  OutLimit: Boolean;
begin
  if SchemaData.Find('minimum', NumberLimit) then
  begin
    if SchemaData.Get('exclusiveMinimum', False) then
      OutLimit := Number <= NumberLimit
    else
      OutLimit := Number < NumberLimit;
    if OutLimit then
      AddError('minimum %g', [NumberLimit]);
  end;

  if SchemaData.Find('maximum', NumberLimit) then
  begin
    if SchemaData.Get('exclusiveMaximum', False) then
      OutLimit := Number >= NumberLimit
    else
      OutLimit := Number > NumberLimit;
    if OutLimit then
      AddError('maximum %g', [NumberLimit]);
  end;

  if SchemaData.Find('multipleOf', NumberLimit) and not IsMultipleOf(Number, NumberLimit) then
    AddError('multipleOf %g', [NumberLimit]);
end;

procedure TJSONSchemaValidator.ValidateObject(Data, SchemaData: TJSONObject);
var
  PropertiesData, PatternPropertiesData: TJSONObject;
  RequiredData: TJSONArray;
  PropName: String;
  i, PropCountLimit: Integer;
  PropData, AdditionalPropertiesData: TJSONData;
begin
  if SchemaData.Find('properties', PropertiesData) then
  begin
    for i := 0 to PropertiesData.Count - 1 do
    begin
      PropName := PropertiesData.Names[i];
      if Data.Find(PropName, PropData) then
        DoValidate(PropData, PropertiesData.Items[i] as TJSONObject);
    end;
  end;

  if SchemaData.Find('patternProperties', PatternPropertiesData) then
    CheckPatternProperties(Data, PatternPropertiesData);

  if SchemaData.Find('additionalProperties', AdditionalPropertiesData) then
    CheckAdditionalProperties(Data, PropertiesData, PatternPropertiesData, AdditionalPropertiesData);

  if SchemaData.Find('minProperties', PropCountLimit) and (Data.Count < PropCountLimit) then
    AddError('minProperties %d', [PropCountLimit]);

  if SchemaData.Find('maxProperties', PropCountLimit) and (Data.Count > PropCountLimit) then
    AddError('maxProperties %d', [PropCountLimit]);

  if SchemaData.Find('required', RequiredData) then
  begin
    for i := 0 to RequiredData.Count - 1 do
    begin
      PropName := RequiredData[i].AsString;
      if Data.IndexOfName(PropName) = -1 then
        AddError('Property %s is required', [PropName]);
    end;
  end;
end;

procedure TJSONSchemaValidator.ValidateString(const Str: String;
  SchemaData: TJSONObject);
var
  LengthLimit: Integer;
  Pattern: String;
begin
  if SchemaData.Find('minLength', LengthLimit) and (Length(Str) < LengthLimit) then
    AddError('minLength %d', [LengthLimit]);

  if SchemaData.Find('maxLength', LengthLimit) and (Length(Str) > LengthLimit) then
    AddError('maxLength %d', [LengthLimit]);

  if SchemaData.Find('pattern', Pattern) and not MatchesPattern(Str, Pattern) then
    AddError('pattern %s', [Pattern]);
end;

procedure TJSONSchemaValidator.CheckType(Data, TypeData: TJSONData);
var
  TypeDataArray: TJSONArray absolute TypeData;
  i: Integer;
  MatchesType: Boolean;
begin
  case TypeData.JSONType of
    jtString:
      MatchesType := DataIsType(Data, TypeData.AsString);
    jtArray:
      begin
        MatchesType := False;
        for i := 0 to TypeDataArray.Count - 1 do
        begin
          if TypeDataArray[i].JSONType = jtString then
            MatchesType := DataIsType(Data, TypeDataArray[i].AsString)
          else
            ;//todo: error
          if MatchesType then
            Break;
        end;
      end;
  end;
  if not MatchesType then
  begin
    if TypeData.JSONType = jtString then
      AddError('Expected type %s', [TypeData.AsString])
    else
      AddError('Expected type %s', [TypeData.AsJSON]);
  end;
end;

constructor TJSONSchemaValidator.Create;
begin
  FErrors := TStringList.Create;
end;

destructor TJSONSchemaValidator.Destroy;
begin
  FRegex.Free;
  FErrors.Free;
  inherited Destroy;
end;

function TJSONSchemaValidator.Validate(Data: TJSONData; SchemaData: TJSONObject): Boolean;
begin
  FErrors.Clear;
  FRootSchemaData := SchemaData;
  DoValidate(Data, SchemaData);
  Result := FErrors.Count = 0;
end;

end.

