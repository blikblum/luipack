unit SqliteUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomSqliteDS, fgl, db;

type

  TSqliteQueryBuilder = class;

  //TSqliteQueryBuilderOption = (sqoSaveToSqlList);

  //TSqliteQueryBuilderOptions = set of TSqliteQueryBuilderOption;

  TIntegerList = specialize TFPGList <Integer>;

  { TSqliteTableDef }

  TSqliteTableDef = class(TCollectionItem)
  private
    FExcludeFields: TStrings;
    FIncludeFields: TStrings;
    FPrimaryKey: String;
    FTableName: String;
    procedure SetExcludeFields(const Value: TStrings);
    procedure SetIncludeFields(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ExcludeFields: TStrings read FExcludeFields write SetExcludeFields;
    property IncludeFields: TStrings read FIncludeFields write SetIncludeFields;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property TableName: String read FTableName write FTableName;
  end;

  { TSqliteTableDefs }

  TSqliteTableDefs = class(TCollection)
  private
    FOwner: TSqliteQueryBuilder;
    function GetItems(Index: Integer): TSqliteTableDef;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TSqliteQueryBuilder);
    function Find(const TableName: String): TSqliteTableDef;
    property Items[Index: Integer]: TSqliteTableDef read GetItems; default;
  end;

  { TSqliteQueryBuilder }

  TSqliteQueryBuilder = class(TComponent)
  private
    FDataset: TCustomSqliteDataset;
    //FOptions: TSqliteQueryBuilderOptions;
    FRecordStates: TRecordStateSet;
    FSQL: TStrings;
    FFieldList: TIntegerList;
    FPrimaryKeyList: TIntegerList;
    FTableDefs: TSqliteTableDefs;
    FWorkTableName: String;
    //templates
    FDeleteTemplate: String;
    FInsertTemplate: String;
    FUpdateTemplate: String;
    FPrimaryKeyTemplate: String;
    FFieldValues: array of String;
    FFieldPrimaryKey: array of String;
    procedure BuildPrimaryKeyList(TableDef: TSqliteTableDef);
    procedure BuildTableSQL(TableDef: TSqliteTableDef);
    procedure BuildTemplates(TableDef: TSqliteTableDef);
    function FormatPrimaryKeyList(Values: PPChar): String;
    function FormatValueList(const Template: String; Values: PPChar): String;
    function GetMappedFieldName(TableDef: TSqliteTableDef; FieldIndex: Integer): String;
    procedure ParseUpdate(UserData: Pointer; Values: PPChar; ABookmark: TBookmark; State: TRecordState);
    procedure SetTableDefs(Value: TSqliteTableDefs);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildSQL(const TableName: String = '');
    property SQL: TStrings read FSQL;
  published
    property Dataset: TCustomSqliteDataset read FDataset write FDataset;
    //property Options: TSqliteQueryBuilderOptions read FOptions write FOptions default [sqoSaveToSqlList];
    property RecordStates: TRecordStateSet read FRecordStates write FRecordStates default [rsAdded, rsUpdated, rsDeleted];
    property TableDefs: TSqliteTableDefs read FTableDefs write SetTableDefs;
  end;

  function FloatToSQL(D: Double): String;

implementation

//replace parameters
//modified from StrUtils StringsReplace
function ReplaceParams(const S: string; Pattern: array of string): string;

var pc,pcc,lastpc : pchar;
    strcount      : integer;
    ResStr,
    CompStr,
    ParamIndex    : string;
    Found         : Boolean;
    sc            : integer;

begin
  sc := length(Pattern);
  dec(sc);

  CompStr := s;

  ResStr := '';
  pc := @CompStr[1];
  pcc := @s[1];
  lastpc := pc+Length(S);

  while pc < lastpc do
    begin
    Found := False;
    for strcount := 0 to sc do
      begin
      ParamIndex := '%' + IntToStr(strcount) + '%';
      if (ParamIndex[1]=pc^) and
         (Length(ParamIndex) <= (lastpc-pc)) and
         (CompareByte(ParamIndex[1],pc^,Length(ParamIndex))=0) then
        begin
        ResStr := ResStr + Pattern[strcount];
        pc := pc+Length(ParamIndex);
        pcc := pcc+Length(ParamIndex);
        Found := true;
        end
      end;
    if not found then
      begin
      ResStr := ResStr + pcc^;
      inc(pc);
      inc(pcc);
      end;
    end;
  Result := ResStr;
end;

function FloatToSQL(D: Double): String;
begin
  Str(D, Result);
end;

{ TSqliteQueryBuilder }

procedure TSqliteQueryBuilder.BuildPrimaryKeyList(TableDef: TSqliteTableDef);
var
  PrimaryKeyStrList: TStringList;
  i, j: Integer;
  FieldName: String;
begin
  PrimaryKeyStrList := TStringList.Create;
  PrimaryKeyStrList.Delimiter := ',';
  PrimaryKeyStrList.StrictDelimiter := True;
  PrimaryKeyStrList.DelimitedText := TableDef.PrimaryKey;

  if PrimaryKeyStrList.Count = 0 then
    PrimaryKeyStrList.DelimitedText := FDataset.PrimaryKey;
  FPrimaryKeyList.Clear;
  for i := 0 to PrimaryKeyStrList.Count - 1 do
  begin
    FieldName := PrimaryKeyStrList.Names[i];
    if FieldName = '' then
      FieldName := PrimaryKeyStrList[i];
    j := FDataset.FieldDefs.IndexOf(FieldName);
    if j <> -1 then
      FPrimaryKeyList.Add(j);
  end;
  if ([rsUpdated, rsDeleted] * FRecordStates <> []) and (FPrimaryKeyList.Count = 0) then
    raise Exception.CreateFmt('PrimaryKey "%s" invalid. A valid key is required for update or delete records',
      [PrimaryKeyStrList.DelimitedText]);
  SetLength(FFieldPrimaryKey, FPrimaryKeyList.Count);

  PrimaryKeyStrList.Destroy;
end;

procedure TSqliteQueryBuilder.BuildTableSQL(TableDef: TSqliteTableDef);
var
  i, j: Integer;
  FieldName: String;
begin
  FFieldList.Clear;
  //add the fields of the include list
  for i := 0 to TableDef.IncludeFields.Count - 1 do
  begin
    FieldName := TableDef.IncludeFields.Names[i];
    if FieldName = '' then
      FieldName := TableDef.IncludeFields[i];
    j := FDataset.FieldDefs.IndexOf(FieldName);
    if j <> -1 then
      FFieldList.Add(j);
  end;
  //if include list is empty or if no field is found, add all
  if FFieldList.Count = 0 then
  begin
    for i := 0 to FDataset.FieldDefs.Count - 1 do
      FFieldList.Add(i);
  end;

  //now remove the fields of the exclude list
  for i := 0 to TableDef.ExcludeFields.Count - 1 do
    FFieldList.Remove(FDataset.FieldDefs.IndexOf(TableDef.ExcludeFields[i]));
  if FFieldList.Count = 0 then
    raise Exception.Create('No field specified');
  SetLength(FFieldValues, FFieldList.Count);

  //build primary key list
  BuildPrimaryKeyList(TableDef);

  if TableDef.TableName = '' then
    FWorkTableName := FDataset.TableName
  else
    FWorkTableName := TableDef.TableName;

  BuildTemplates(TableDef);
  FSql.Add('BEGIN;');
  FDataset.QueryUpdates(FRecordStates, @ParseUpdate);
  FSql.Add('COMMIT;');
end;

procedure TSqliteQueryBuilder.BuildTemplates(TableDef: TSqliteTableDef);
var
  i: Integer;
  FieldName, ValueList: String;
begin
  FDeleteTemplate := 'DELETE FROM ' + FWorkTableName + ' WHERE ';
  FInsertTemplate := 'INSERT INTO ' + FWorkTableName + '(';
  FUpdateTemplate := 'UPDATE ' + FWorkTableName + ' SET ';
  ValueList := '';
  for i := 0 to FFieldList.Count - 2 do
  begin
    ValueList := ValueList + '%' + IntToStr(i) + '%,';
    FieldName := GetMappedFieldName(TableDef, FFieldList[i]);
    FInsertTemplate := FInsertTemplate + FieldName + ',';
    FUpdateTemplate := FUpdateTemplate + FieldName + ' = %' + IntToStr(i) + '%,';
  end;
  //set the last field
  i := FFieldList.Count - 1;
  FieldName := GetMappedFieldName(TableDef, FFieldList[i]);
  ValueList := ValueList + '%' + IntToStr(i) + '%';
  FInsertTemplate := FInsertTemplate + FieldName + ') VALUES (' + ValueList + ');';
  FUpdateTemplate := FUpdateTemplate + FieldName + '= %' + IntToStr(i) + '% WHERE ';

  //primary key
  FPrimaryKeyTemplate := '';
  for i := 0 to FPrimaryKeyList.Count - 1 do
  begin
    FieldName := GetMappedFieldName(TableDef, FPrimaryKeyList[i]);
    FPrimaryKeyTemplate := FPrimaryKeyTemplate + FieldName + ' = %' + IntToStr(i) + '%';
    if i < FPrimaryKeyList.Count - 1 then
      FPrimaryKeyTemplate := FPrimaryKeyTemplate + ' AND '
  end;
  FPrimaryKeyTemplate := FPrimaryKeyTemplate + ';';
end;

function TSqliteQueryBuilder.FormatPrimaryKeyList(Values: PPChar): String;
var
  i: Integer;
begin
  for i := 0 to FPrimaryKeyList.Count - 1 do
    FFieldPrimaryKey[i] := FDataset.GetSQLValue(Values, FPrimaryKeyList[i]);
  Result := ReplaceParams(FPrimaryKeyTemplate, FFieldPrimaryKey);
end;

function TSqliteQueryBuilder.FormatValueList(const Template: String;
  Values: PPChar): String;
var
  i: Integer;
begin
  for i := 0 to FFieldList.Count - 1 do
    FFieldValues[i] := FDataset.GetSQLValue(Values, FFieldList[i]);
  Result := ReplaceParams(Template, FFieldValues);
end;

function TSqliteQueryBuilder.GetMappedFieldName(TableDef: TSqliteTableDef; FieldIndex: Integer): String;
var
  i: Integer;
begin
  i := TableDef.IncludeFields.IndexOfName(FDataset.FieldDefs[FieldIndex].Name);
  if i <> -1 then
    Result := TableDef.IncludeFields.ValueFromIndex[i]
  else
    Result := FDataset.FieldDefs[FieldIndex].Name;
end;

procedure TSqliteQueryBuilder.ParseUpdate(UserData: Pointer; Values: PPChar;
  ABookmark: TBookmark; State: TRecordState);
begin
  case State of
    rsAdded:
    begin
      FSql.Add(FormatValueList(FInsertTemplate, Values));
    end;
    rsDeleted:
    begin
      FSql.Add(FDeleteTemplate + FormatPrimaryKeyList(Values));
    end;
    rsUpdated:
    begin
      FSql.Add(FormatValueList(FUpdateTemplate, Values) +
        FormatPrimaryKeyList(Values));
    end;
  end;
end;

procedure TSqliteQueryBuilder.SetTableDefs(Value: TSqliteTableDefs);
begin
  FTableDefs.Assign(Value);
end;

constructor TSqliteQueryBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableDefs := TSqliteTableDefs.Create(Self);
  FSQL := TStringList.Create;
  FFieldList := TIntegerList.Create;
  FPrimaryKeyList := TIntegerList.Create;
  FRecordStates := [rsAdded, rsUpdated, rsDeleted];
end;

destructor TSqliteQueryBuilder.Destroy;
begin
  FTableDefs.Destroy;
  FSQL.Destroy;
  FFieldList.Destroy;
  FPrimaryKeyList.Destroy;
  inherited Destroy;
end;

procedure TSqliteQueryBuilder.BuildSQL(const TableName: String);
var
  i: Integer;
  TableDef: TSqliteTableDef;
begin
  if FDataset = nil then
    raise Exception.Create('Dataset not set');
  FSql.Clear;
  if TableName <> '' then
  begin
    TableDef := FTableDefs.Find(TableName);
    if TableDef <> nil then
      BuildTableSQL(TableDef);
  end
  else
  begin
    for i := 0 to FTableDefs.Count - 1 do
      BuildTableSQL(TSqliteTableDef(FTableDefs.Items[i]));
  end;
end;

{ TSqliteTableDef }

procedure TSqliteTableDef.SetExcludeFields(const Value: TStrings);
begin
  FExcludeFields.Assign(Value);
end;

procedure TSqliteTableDef.SetIncludeFields(const Value: TStrings);
begin
  FIncludeFields.Assign(Value);
end;

function TSqliteTableDef.GetDisplayName: string;
begin
  Result := FTableName;
end;

constructor TSqliteTableDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIncludeFields := TStringList.Create;
  FExcludeFields := TStringList.Create;
end;

destructor TSqliteTableDef.Destroy;
begin
  FIncludeFields.Destroy;
  FExcludeFields.Destroy;
  inherited Destroy;
end;

procedure TSqliteTableDef.Assign(Source: TPersistent);
begin
  if Source is TSqliteTableDef then
  begin
    ExcludeFields := TSqliteTableDef(Source).ExcludeFields;
    IncludeFields := TSqliteTableDef(Source).IncludeFields;
    TableName := TSqliteTableDef(Source).TableName;
    PrimaryKey := TSqliteTableDef(Source).PrimaryKey;
  end
  else
    inherited Assign(Source);
end;

{ TSqliteTableDefs }

function TSqliteTableDefs.GetItems(Index: Integer): TSqliteTableDef;
begin
  Result := TSqliteTableDef(inherited GetItem(Index));
end;

function TSqliteTableDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TSqliteTableDefs.Create(AOwner: TSqliteQueryBuilder);
begin
  inherited Create(TSqliteTableDef);
  FOwner := AOwner;
end;

function TSqliteTableDefs.Find(const TableName: String): TSqliteTableDef;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TSqliteTableDef(inherited GetItem(i));
    if CompareText(TableName, Result.TableName) = 0 then
      Exit;
  end;
  Result := nil;
end;

end.

